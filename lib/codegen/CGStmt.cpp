// Copyright (c) 2019, Compiler Tree Technologies Pvt Ltd.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "AST/Statements.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "codegen/CGASTHelper.h"
#include "codegen/CodeGen.h"
#include "codegen/LLVMUtil.h"
#include "codegen/RuntimeHelper.h"
#include "common/Debug.h"

#include "llvm-c/Target.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/ErrorHandling.h"

using namespace fc;
using namespace ast;
using namespace llvm;

static bool isStringAssignment(Expr *lhs, Expr *rhs) {
  auto lhsTy = lhs->getType();
  auto rhsTy = rhs->getType();
  if (rhsTy->isCharacterTy() && lhsTy->isArrayTy() &&
      llvm::isa<ConstantVal>(rhs))
    return true;

  // Should be a char array section as all other are expanded!
  if (llvm::isa<ArraySection>(lhs) && llvm::isa<ArraySection>(rhs))
    return true;

  return false;
}

// Currently we are freeing the memory location.
bool CodeGen::emitDeAllocateStmt(DeAllocateStmt *stmt) {
  SymbolList list = stmt->getDeAllocateObjList();
  assert(!list.empty());

  auto freeFn = cgHelper->getFreeFunction();
  auto zero = IRB->getInt64(0);
  auto one = IRB->getInt64(1);
  auto stat = stmt->getStat();
  llvm::Value *statValue = nullptr;
  if (stat) {
    statValue = emitExpression(stat, true);
  }
  for (unsigned i = 0; i < list.size(); ++i) {

    auto sym = list[i]->getOrigSymbol();
    auto arrayTy = llvm::dyn_cast<ArrayType>(sym->getType());
    assert(arrayTy);
    assert(arrayTy->isDynArrayTy());
    // the pointer is at 0th offset. So we can directly use alloca
    auto Alloca = context.getLLVMValueFor(sym->getName());
    llvm::Value *Ptr =
        IRB->CreateBitCast(Alloca, IRB->getInt8PtrTy()->getPointerTo());
    Ptr = IRB->CreateLoad(Ptr);
    IRB->CreateCall(freeFn, {Ptr});

    if (std == Standard::f77) {
      if (sym->getAllocKind() == fc::AllocationKind::StaticLocal) {
        context.functionAllocMap[context.symbolMap[sym->getName()]] = false;
      }
    }

    // Mark as unallocated
    if (std != Standard::f77) {
      auto isAllocated = IRB->CreateStructGEP(nullptr, Alloca, 2, "flag");
      emitStoreInstruction(IRB->getFalse(), isAllocated, true);
    }

    auto numDims = arrayTy->getNumDims();

    auto DimArr = IRB->CreateStructGEP(nullptr, Alloca, 1, "dimsArr");
    for (unsigned j = 0; j < numDims; ++j) {
      auto Dim = IRB->CreateInBoundsGEP(
          DimArr, {IRB->getInt32(0), IRB->getInt32(j)}, "dim");

      auto LB = IRB->CreateStructGEP(nullptr, Dim, 0, "lb");
      emitStructStoreInst(one, LB, Dim, Dim->getType(), 0);

      auto UB = IRB->CreateStructGEP(nullptr, Dim, 1, "ub");
      emitStructStoreInst(zero, UB, Dim, Dim->getType(), 1);

      auto Size = IRB->CreateStructGEP(nullptr, Dim, 2, "size");
      emitStructStoreInst(zero, Size, Dim, Dim->getType(), 2);
    }
  }

  // FIXME : Free doesn't return anything. Currently storing success value as 0
  if (statValue) {
    emitStoreInstruction(
        ConstantInt::get(statValue->getType()->getPointerElementType(), 0),
        statValue);
  }
  return true;
}

bool CodeGen::emitNullifyStmt(NullifyStmt *nullifyStmt) {
  for (Stmt *stmt : nullifyStmt->getOperands()) {
    Expr *expr = static_cast<Expr *>(stmt);

    llvm::Value *dest;
    if (auto objName = llvm::dyn_cast<ObjectName>(expr)) {
      dest = context.getLLVMValueFor(objName->getName());
    } else {
      dest = emitExpression(expr, /* isLHS = */ true);
    }

    assert(dest);
    llvm::PointerType *destType =
        llvm::dyn_cast<llvm::PointerType>(dest->getType());
    assert(destType);
    emitStoreInstruction(
        llvm::Constant::getNullValue(destType->getElementType()), dest,
        /* disableTBAA = */ true);
  }

  return true;
}

bool CodeGen::emitAllocateStmt(AllocateStmt *stmt) {
  SymbolList list = stmt->getAllocateObjList();
  assert(!list.empty());

  auto mallocFn = cgHelper->getMallocFunction();

  auto stat = stmt->getStat();
  llvm::Value *statValue = nullptr;
  llvm::Value *zero = nullptr;
  llvm::Value *one = nullptr;
  if (stat) {
    statValue = emitExpression(stat, true);
    zero = llvm::ConstantInt::get(statValue->getType()->getPointerElementType(),
                                  0);
    one = llvm::ConstantInt::get(statValue->getType()->getPointerElementType(),
                                 1);
  }
  for (unsigned i = 0; i < list.size(); ++i) {
    auto Alloca = context.getLLVMValueFor(list[i]->getName());
    auto arraySpec = stmt->getAllocateShape(i);

    // Mark as allocated
    if (std != Standard::f77) {
      auto isAllocated = IRB->CreateStructGEP(nullptr, Alloca, 2, "flag");
      emitStoreInstruction(IRB->getTrue(), isAllocated, true);
    }

    unsigned numBounds = arraySpec->getNumBounds();
    llvm::SmallVector<llvm::Value *, 2> sizesList;

    llvm::Value *DimArr = nullptr;
    if (std != Standard::f77)
      DimArr = IRB->CreateStructGEP(nullptr, Alloca, 1, "dimsArr");

    for (unsigned j = 0; j < numBounds; ++j) {

      llvm::Value *Dim = nullptr;
      if (std != Standard::f77) {
        // Store the size, ubound, lbound of each dimension
        Dim = IRB->CreateInBoundsGEP(
            DimArr, {IRB->getInt32(0), IRB->getInt32(j)}, "dim");
      }

      auto fcBounds = arraySpec->getBounds(j);
      auto lower = cgHelper->getSExt(emitExpression(fcBounds.first));
      auto upper = cgHelper->getSExt(emitExpression(fcBounds.second));
      auto size = IRB->CreateSub(upper, lower, "size");
      size = IRB->CreateAdd(size, IRB->getInt64(1));
      sizesList.push_back(size);

      if (std != Standard::f77) {
        auto LB = IRB->CreateStructGEP(nullptr, Dim, 0, "lb");
        // IRB->CreateStore(lower, LB);
        emitStructStoreInst(lower, LB, Dim, Dim->getType(), 0);

        auto UB = IRB->CreateStructGEP(nullptr, Dim, 1, "ub");
        // IRB->CreateStore(upper, UB);
        emitStructStoreInst(upper, UB, Dim, Dim->getType(), 1);

        auto Size = IRB->CreateStructGEP(nullptr, Dim, 2, "size");
        // IRB->CreateStore(size, Size);
        emitStructStoreInst(size, Size, Dim, Dim->getType(), 2);
      }
    }

    // Calculate number of elements
    llvm::Value *finalSize = nullptr;
    for (auto size : sizesList) {
      if (!finalSize) {
        finalSize = size;
        continue;
      }
      finalSize = IRB->CreateMul(size, finalSize, "alloc.size.calc");
    }

    if (finalSize->getType() == IRB->getInt32Ty())
      finalSize = IRB->CreateSExt(finalSize, IRB->getInt64Ty());

    llvm::Value *Base = nullptr;
    if (std != Standard::f77) {
      Base = IRB->CreateStructGEP(nullptr, Alloca, 0, "base");
    }

    auto fcArrayTy =
        llvm::dyn_cast<ArrayType>(list[0]->getOrigSymbol()->getType());
    assert(fcArrayTy);

    unsigned elementSize = cgHelper->getSizeForType(fcArrayTy->getElementTy());

    auto valuePtr =
        cgHelper->getLLVMTypeFor(fcArrayTy->getElementTy())->getPointerTo();

    auto sizeVal = IRB->getInt64(elementSize);

    // Number of bytes to allocate
    finalSize = IRB->CreateMul(sizeVal, finalSize, "alloc.finalsize");

    llvm::Value *malloc = IRB->CreateCall(mallocFn, {finalSize});
    if (statValue) {
      auto icmp = IRB->CreateIsNotNull(malloc, "isnull");
      auto select = IRB->CreateSelect(icmp, zero, one);
      if (i == 0)
        emitStoreInstruction(select, statValue);
      else {
        // Should overall status. Do or with existing value
        auto lastStat = emitLoadInstruction(statValue, "statValue");
        auto orValue = IRB->CreateOr(select, lastStat);
        emitStoreInstruction(orValue, statValue);
      }
    }

    malloc = IRB->CreateBitCast(malloc, valuePtr, list[i]->getName());

    if (std != Standard::f77) {
      // Store the size to pointer
      // emitStoreInstruction(malloc, Base);
      emitStructStoreInst(malloc, Base, Alloca, Alloca->getType(), 0);
    } else {
      context.symbolMap[list[i]->getName()] = malloc;
    }

    if (std == Standard::f77) {
      auto sym = list[i]->getOrigSymbol();
      if (sym->getAllocKind() == fc::AllocationKind::StaticLocal) {
        context.functionAllocMap[malloc] = true;
      }
    }
  }
  return true;
}

bool CodeGen::emitAssignment(llvm::Value *lhsVal, llvm::Value *rhsVal) {

  llvm::PointerType *rhsPtrTy = dyn_cast<llvm::PointerType>(rhsVal->getType());
  llvm::PointerType *lhsPtrTy = dyn_cast<llvm::PointerType>(lhsVal->getType());

  if (!lhsPtrTy || !rhsPtrTy) {
    emitStoreInstruction(rhsVal, lhsVal);
    return true;
  }

  auto ArrTy = dyn_cast<llvm::ArrayType>(lhsPtrTy->getElementType());
  auto rhsArrTy = dyn_cast<llvm::ArrayType>(rhsPtrTy->getElementType());

  if (!ArrTy && !rhsArrTy) {
    llvm_unreachable("Unknown assignment kind");
  }

  llvm::ArrayType *arrayType = ArrTy ? ArrTy : rhsArrTy;

  assert(arrayType->getElementType()->isIntegerTy(8));

  unsigned length = arrayType->getArrayNumElements();
  unsigned size =
      length * (arrayType->getElementType()->getScalarSizeInBits() / 8);

  llvm::Value *destPtr, *srcPtr;

  if (ArrTy)
    destPtr =
        IRB->CreateInBoundsGEP(lhsVal, {IRB->getInt32(0), IRB->getInt32(0)});
  else
    destPtr = lhsVal;

  if (rhsArrTy)
    srcPtr =
        IRB->CreateInBoundsGEP(rhsVal, {IRB->getInt32(0), IRB->getInt32(0)});
  else
    srcPtr = rhsVal;

  assert(length);
  // TODO: Handle alignment.
  IRB->CreateMemCpy(
      destPtr, destPtr->getPointerAlignment(TheModule->getDataLayout()), srcPtr,
      srcPtr->getPointerAlignment(TheModule->getDataLayout()), size);
  return true;
}

bool CodeGen::emitAssignment(AssignmentStmt *stmt) {
  auto rhs = stmt->getRHS();
  auto lhs = stmt->getLHS();
  auto rhsVal = emitExpression(rhs);
  auto lhsVal = emitExpression(lhs, true);
  assert(lhsVal);

  // Since we are emititing strcat, result is already copied to LHS.
  // op1 of binary expr is already copied to LHS in sema
  if (auto expr = llvm::dyn_cast<BinaryExpr>(rhs)) {
    if (expr->getOpKind() == BinaryOpKind::Concat) {
      return true;
    }
  }

  if (isStringAssignment(lhs, rhs)) {
    auto Fn = runtimeHelper->getStrCpyFunction();
    llvm::Value *arg1 = IRB->CreateBitCast(lhsVal, IRB->getInt8PtrTy());
    llvm::Value *arg2 = IRB->CreateBitCast(rhsVal, IRB->getInt8PtrTy());
    IRB->CreateCall(Fn, {arg1, arg2});
    return true;
  }

  return emitAssignment(lhsVal, rhsVal);
}

bool CodeGen::emitPointerAssignment(PointerAssignmentStmt *stmt) {
  auto lhs = stmt->getLHS();
  auto rhs = stmt->getRHS();
  llvm::Value *lhsVal = nullptr;
  llvm::Value *rhsVal = nullptr;

  auto getValForPtrAssign = [&](Expr *expr, bool isLHS) {
    // This works only for structure-component members that are ObjectName. This
    // will fail for array-section. TODO: A structure-component can inherently
    // be an object-name, array-element, array-section etc. Once we deduce it,
    // we should be able to reuse the non-pointer analagous APIs for these.
    // Currently emitExpression() differentiates struct-comp with other
    // designators since they are different Exprs, and this makes things messy.
    // Should we bring back the Designator class ?
    if (auto structComp = llvm::dyn_cast<StructureComponent>(expr)) {
      return emitStructureComponent(structComp, isLHS);

    } else if (auto obj = llvm::dyn_cast<ObjectName>(expr)) {
      if (obj->getSymbol()->getOrigSymbol()->getType()->isPointerTy() && !isLHS)
        return emitExpression(obj, true);
      return context.getLLVMValueFor(
          obj->getSymbol()->getOrigSymbol()->getName());

    } else {
      return static_cast<llvm::Value *>(nullptr);
    }
  };

  lhsVal = getValForPtrAssign(lhs, true);
  rhsVal = getValForPtrAssign(rhs, false);

  if (lhsVal && rhsVal) {
    emitStoreInstruction(rhsVal, lhsVal, /* disableTBAA = */ true);
    return true;
  }

  ArraySection *lhsArrSec, *rhsArrSec;
  if ((lhsArrSec = llvm::dyn_cast<ArraySection>(lhs)) &&
      (rhsArrSec = llvm::dyn_cast<ArraySection>(rhs))) {
    Symbol *lhsSym = lhsArrSec->getSymbol()->getOrigSymbol();
    Symbol *rhsSym = rhsArrSec->getSymbol()->getOrigSymbol();

    lhsVal = context.getLLVMValueFor(lhsSym->getName());

    auto lhsPtrTy = llvm::dyn_cast<fc::PointerType>(lhsSym->getType());
    assert(lhsPtrTy);
    auto lhsTy = llvm::dyn_cast<fc::ArrayType>(lhsPtrTy->getElementType());

    auto rhsTy = llvm::dyn_cast<fc::ArrayType>(rhsSym->getType());

    assert(lhsTy && rhsTy); // supporting only rhs as array cases for now

    rhsVal = getDynamicArrayFor(context.getLLVMValueFor(rhsSym->getName()),
                                rhsTy, lhsTy);

    assert(lhsVal && rhsVal);
    emitStoreInstruction(rhsVal, lhsVal, /* disableTBAA = */ true);
    return true;
  }

  llvm_unreachable("unhandled pointer assignment");
}

static bool isLhs(Expr *expr) {
  if (auto arrayEle = llvm::dyn_cast<ArrayElement>(expr)) {
    auto arrayTy = llvm::dyn_cast<fc::ArrayType>(arrayEle->getType());
    assert(arrayTy);
    if (arrayTy->getNumDims() == arrayEle->getNumIndices())
      return false;
    return true;
  }

  if (auto arraySec = llvm::dyn_cast<ArraySection>(expr)) {
    if (arraySec->isFullRange())
      return true;
    llvm_unreachable("Partial array section should be replaced in sema");
  }
  return false;
}

// Emit the print statement. Make a call to the
// runtime print routine.
bool CodeGen::emitPrintStmt(PrintStmt *stmt) {

  if (stmt->getNumOperands() == 0)
    return true;

  bool isLHS;
  auto printFn = runtimeHelper->getPrintFunction();
  llvm::SmallVector<llvm::Value *, 2> exprValList;
  for (auto stmt : stmt->getOperands()) {
    auto expr = llvm::dyn_cast<Expr>(stmt);
    assert(expr);
    isLHS = isLhs(expr);
    auto exprVal = emitExpression(expr, isLHS);
    assert(exprVal);
    auto *arrDimSize = getArrDimSizeVal(expr, exprVal);

    bool isDynArray = false;

    if (expr->getType()->isDynArrayTy()) {
      isDynArray = true;
    } else if (auto ptrTy = llvm::dyn_cast<fc::PointerType>(expr->getType())) {
      if (ptrTy->getElementType()->isArrayTy())
        isDynArray = true;
    }

    runtimeHelper->fillPrintArgsFor(exprVal, exprValList, arrDimSize, IRB,
                                    isDynArray);
  }
  exprValList.insert(exprValList.begin(), IRB->getInt32(exprValList.size()));

  // Create call now.
  auto printCall = IRB->CreateCall(printFn, exprValList);
  return (printCall != nullptr);
}

bool CodeGen::emitInternalWriteStmt(WriteStmt *stmt) {
  auto unit = stmt->getUnit();
  auto iostat = stmt->getIostat();
  llvm::Value *iostatValue = nullptr;
  llvm::Value *zero = nullptr;
  if (iostat) {
    iostatValue = emitExpression(iostat, true);
    zero = llvm::ConstantInt::get(
        iostatValue->getType()->getPointerElementType(), 0);
  }

  ArrayType *arrayTy = llvm::dyn_cast<ArrayType>(unit->getType());
  if (!arrayTy)
    return false;

  if (!arrayTy->getElementTy()->isCharacterTy() &&
      !arrayTy->getElementTy()->isStringCharTy())
    return false;

  ExprList exprList = stmt->getExprList();
  if (exprList.size() == 1) {
    auto expr = exprList[0];
    if (!expr->getType()->isIntegralTy()) {
      llvm_unreachable("Not handled!");
    }

    assert(expr->getType()->isIntegralTy());
    auto exprVal = emitExpression(expr, false);
    auto unitExpr = emitExpression(unit, true);
    unitExpr = IRB->CreateBitCast(unitExpr, IRB->getInt8PtrTy());
    auto Fn = runtimeHelper->getIntToStringFunction();
    auto call = IRB->CreateCall(Fn, {unitExpr, exprVal});
    assert(call);

    // Currently storing zero.
    if (iostatValue) {
      emitStoreInstruction(zero, iostatValue);
    }
    return true;
  }

  bool isLHS;
  auto sprintfFn = runtimeHelper->getSprintfFunction();
  llvm::SmallVector<llvm::Value *, 2> exprValList;

  for (auto expr : exprList) {
    assert(expr);
    isLHS = isLhs(expr);
    auto exprVal = emitExpression(expr, isLHS);
    assert(exprVal);

    auto *arrDimSize = getArrDimSizeVal(expr, exprVal);

    bool isDynArray = false;

    if (expr->getType()->isDynArrayTy()) {
      isDynArray = true;
    } else if (auto ptrTy = llvm::dyn_cast<fc::PointerType>(expr->getType())) {
      if (ptrTy->getElementType()->isArrayTy())
        isDynArray = true;
    }

    runtimeHelper->fillPrintArgsFor(exprVal, exprValList, arrDimSize, IRB,
                                    isDynArray);
  }

  auto unitExpr = emitExpression(unit, true);
  unitExpr = IRB->CreateBitCast(unitExpr, IRB->getInt8PtrTy());
  exprValList.insert(exprValList.begin(), unitExpr);
  exprValList.insert(exprValList.begin(),
                     IRB->getInt32(exprValList.size() - 1));

  IRB->CreateCall(sprintfFn, exprValList);

  // Currently storing zero.
  if (iostatValue) {
    emitStoreInstruction(zero, iostatValue);
  }
  return true;
}

// Emit write statement
bool CodeGen::emitWriteStmt(WriteStmt *stmt) {

  if (stmt->getNumOperands() == 0)
    return true;

  auto unitExpr = stmt->getUnit();
  llvm::Value *unit;
  bool fileWrite = false;

  if (unitExpr) {
    if (emitInternalWriteStmt(stmt))
      return true;

    fileWrite = true;
    unit = emitExpression(unitExpr);
  }

  assert(!stmt->getIostat() && "IOstat for normal write is not handled");

  llvm::Function *writeFn;
  if (fileWrite)
    writeFn = runtimeHelper->getFileWriteFunction();
  else
    writeFn = runtimeHelper->getWriteFunction();

  llvm::SmallVector<llvm::Value *, 2> exprValList;
  llvm::SmallVector<int, 2> spaceList;
  stmt->getSpaceList(spaceList);
  unsigned k = 0;
  assert(spaceList.size() == stmt->getExprList().size());

  for (auto expr : stmt->getExprList()) {
    auto exprVal = emitExpression(expr);
    bool isString = false;
    auto ArrTy = dyn_cast<ArrayType>(expr->getType());
    if (ArrTy && ArrTy->getElementTy()->isStringCharTy())
      isString = true;
    assert(exprVal);
    auto numSpaces = IRB->getInt32(spaceList[k++]);
    exprValList.push_back(numSpaces);
    auto *arrDimSize = getArrDimSizeVal(expr, exprVal);
    runtimeHelper->fillPrintArgsFor(exprVal, exprValList, arrDimSize, IRB,
                                    isString);
  }
  exprValList.insert(exprValList.begin(), IRB->getInt32(exprValList.size()));

  if (fileWrite)
    exprValList.insert(exprValList.begin(), unit);

  auto writeCall = IRB->CreateCall(writeFn, exprValList);
  return (writeCall != nullptr);
}

bool CodeGen::emitInternalReadStmt(ReadStmt *stmt) {
  auto unit = stmt->getUnit();
  ArrayType *arrayTy = llvm::dyn_cast<ArrayType>(unit->getType());
  if (!arrayTy)
    return false;
  if (!arrayTy->getElementTy()->isCharacterTy() &&
      !arrayTy->getElementTy()->isStringCharTy())
    return false;

  ExprList exprList = stmt->getExprList();
  assert(exprList.size() == 1);

  auto expr = exprList[0];

  // TODO Handle more generically.
  //      Remove duplicate code
  if (auto arraySec = llvm::dyn_cast<ArraySection>(expr)) {
    assert(arraySec->isFullRange());
    auto exprVal = emitExpression(exprList[0], true);
    exprVal = IRB->CreateBitCast(exprVal, IRB->getInt32Ty()->getPointerTo());
    auto unitExpr = emitExpression(unit, true);
    unitExpr = IRB->CreateBitCast(unitExpr, IRB->getInt8PtrTy());
    auto Fn = runtimeHelper->getStringToIntArrayFunction();
    IRB->CreateCall(Fn, {unitExpr, exprVal});
    return true;
  }

  if (!expr->getType()->isIntegralTy()) {
    auto arrayEle = llvm::dyn_cast<ArrayElement>(expr);
    assert(arrayEle && "Not handled");
    if (!arrayEle->getElementType()->isIntegralTy())
      llvm_unreachable("Not handled");
  }

  auto exprVal = emitExpression(exprList[0], true);
  auto unitExpr = emitExpression(unit, true);
  unitExpr = IRB->CreateBitCast(unitExpr, IRB->getInt8PtrTy());
  auto Fn = runtimeHelper->getStringToIntFunction();
  auto call = IRB->CreateCall(Fn, {unitExpr});
  emitStoreInstruction(call, exprVal);
  return true;
}

// Emit read statement, emit call to runtime function
bool CodeGen::emitReadStmt(ReadStmt *stmt) {

  if (stmt->getNumOperands() == 0)
    return true;

  auto unitExpr = stmt->getUnit();
  llvm::Value *unit;
  bool fileRead = false;
  if (unitExpr) {
    // auto unitConstVal = llvm::dyn_cast<ConstantVal>(unitExpr);
    if (emitInternalReadStmt(stmt))
      return true;
    unit = emitExpression(unitExpr);
    fileRead = true;
  }
  llvm::Function *readFn;
  if (fileRead)
    readFn = runtimeHelper->getFileReadFunction();
  else
    readFn = runtimeHelper->getReadFunction();

  llvm::SmallVector<llvm::Value *, 2> exprValList;
  for (auto expr : stmt->getExprList()) {
    auto exprVal = emitExpression(expr, true);
    bool isString = false;
    auto ArrTy = dyn_cast<ArrayType>(expr->getType());
    if (ArrTy && ArrTy->getElementTy()->isStringCharTy())
      isString = true;
    assert(exprVal);
    auto *arrDimSize = getArrDimSizeVal(expr, exprVal);
    runtimeHelper->fillReadArgsFor(exprVal, exprValList, arrDimSize, IRB,
                                   isString);
  }
  exprValList.insert(exprValList.begin(), IRB->getInt32(exprValList.size()));

  llvm::Value *statValue = nullptr;
  if (auto iostat = stmt->getIostat()) {
    statValue = emitExpression(iostat, true);
  }

  if (fileRead)
    exprValList.insert(exprValList.begin(), unit);

  auto readCall = IRB->CreateCall(readFn, exprValList);
  if (statValue) {
    emitStoreInstruction(readCall, statValue);
  }

  return (readCall != nullptr);
}

// emit open-stmt
bool CodeGen::emitOpenStmt(OpenStmt *stmt) {
  auto unitExpr = stmt->getUnit();
  auto fileExpr = stmt->getFile();

  auto fileName = emitExpression(fileExpr);
  auto unit = emitExpression(unitExpr);

  auto openFn = runtimeHelper->getOpenFunction();
  llvm::SmallVector<llvm::Value *, 2> argsList;
  runtimeHelper->fillOpenArgsFor(unit, fileName, argsList, IRB);

  llvm::Value *openCall = IRB->CreateCall(openFn, argsList);
  assert(openCall);

  if (auto iostat = stmt->getIostat()) {
    auto statValue = emitExpression(iostat, true);
    assert(statValue);
    openCall = IRB->CreateIntCast(openCall,
                                  statValue->getType()->getPointerElementType(),
                                  /*isSigned*/ true);
    emitStoreInstruction(openCall, statValue);
  }
  return true;
}

bool CodeGen::emitCloseStmt(CloseStmt *stmt) {
  auto unitExpr = stmt->getUnit();
  auto unit = emitExpression(unitExpr);
  auto closeFn = runtimeHelper->getCloseFunction();
  llvm::Value *closeCall = IRB->CreateCall(closeFn, {unit});

  if (auto iostat = stmt->getIostat()) {
    auto statValue = emitExpression(iostat, true);
    assert(statValue);
    closeCall = IRB->CreateIntCast(
        closeCall, statValue->getType()->getPointerElementType(),
        /*isSigned*/ true);
    emitStoreInstruction(closeCall, statValue);
  }

  return true;
}

bool CodeGen::emitStopStmt(StopStmt *stmt) {
  auto stopCode = stmt->getStopCode();
  auto currFn = context.currFn;
  // assert(stopCode->getType()->isIntegralTy() && "Expecting integer expr");
  if (stopCode) {
    auto stopVal = emitExpression(stopCode);
    IRB->CreateRet(stopVal);
  } else {
    auto Ty = currFn->getReturnType();
    if (Ty->isVoidTy()) {
      IRB->CreateRetVoid();
    } else {
      IRB->CreateRet(llvm::Constant::getNullValue(Ty));
    }
  }
  auto newBlock = getNewBlock("deadBlock", true);
  IRB->SetInsertPoint(newBlock);
  return true;
}

bool CodeGen::emitIfElseStmt(IfElseStmt *stmt) {
  auto &kindList = stmt->getKindList();
  auto ExitBB = BasicBlock::Create(*LLContext, "if.exit");

  for (unsigned i = 0; i < stmt->getNumOperands(); i++) {
    auto ifPair = stmt->getIfStmt(i);
    auto expr = ifPair->getCondition();
    auto block = ifPair->getBlock();
    llvm::Value *exprVal = nullptr;
    auto ThenBB = BasicBlock::Create(*LLContext, "if.then", context.currFn);
    llvm::BasicBlock *ElseBlock = nullptr;
    if (kindList[i] != IfConstructKind::ElseKind) {
      exprVal = emitExpression(expr);
      ElseBlock = ExitBB;
      if (i != stmt->getNumOperands() - 1) {
        ElseBlock = BasicBlock::Create(*LLContext, "else");
      }
      IRB->CreateCondBr(exprVal, ThenBB, ElseBlock);
    } else {
      IRB->CreateBr(ThenBB);
      ElseBlock = ExitBB;
    }

    // Emit code for if block.
    context.currBB = ThenBB;
    IRB->SetInsertPoint(ThenBB);
    // Emit the block statements.
    emitExectubaleConstructList(block->getStmtList());
    /// Add the terminator for the then block.
    IRB->CreateBr(ExitBB);

    // Set the current BB to else block (regular block)
    context.currBB = ElseBlock;
    ElseBlock->insertInto(context.currFn);
    IRB->SetInsertPoint(ElseBlock);
  }
  return true;
}

// Emit LLVM canonical unrotated loop!
bool CodeGen::emitDoWhileStmt(DoWhileStmt *stmt) {
  auto expr = stmt->getLogicalExpr();

  // Create a new block for logical header emission.
  auto Header = BasicBlock::Create(*LLContext, "for.header", context.currFn);

  auto Body = BasicBlock::Create(*LLContext, "for.body", context.currFn);
  auto Exit = BasicBlock::Create(*LLContext, "for.exit");

  // Create direct br.
  IRB->CreateBr(Header);

  context.currBB = Header;
  IRB->SetInsertPoint(Header);

  // Emit loop condition expression in Header.
  auto exprVal = emitExpression(expr);
  IRB->CreateCondBr(exprVal, Body, Exit);

  // Emit code for loop body.
  context.currBB = Body;
  IRB->SetInsertPoint(Body);

  // Emit the block statements.
  emitExectubaleConstructList(stmt->getBlock()->getStmtList());

  // Create backedge to Header from body.
  IRB->CreateBr(Header);

  // Set the current BB to exit block (regular block)
  context.currBB = Exit;
  Exit->insertInto(context.currFn);
  IRB->SetInsertPoint(Exit);
  return true;
}

bool CodeGen::emitDoStmt(DoStmt *stmt) {
  if (stmt->isParallel()) {
    context.isParallelLoop = true;
  }

  auto expr = stmt->getQuadExpr();

  // Block to do bounds calculation.
  // auto BoundsBB = getNewBlock("for.bounds", true);

  // Redirect current to bounds block.
  // IRB->CreateBr(BoundsBB);

  // We need to calculate runtime trip count for
  // the do loop as it may contain dynamic start,end and
  // increment values.
  // Note that there is no logical condition given
  // by the user to end the loop.
  // Hence for do i = L,U,S, we calculate the trip count
  // by doing ((U - L +S) /S) calculation in the for.bounds
  // block.

  // context.currBB = BoundsBB;
  // IRB->SetInsertPoint(BoundsBB);

  llvm::Value *tripCount = nullptr;
  AllocaInst *tripIndVar = nullptr;
  llvm::Value *doVar = nullptr;
  llvm::Value *stepVal = nullptr;
  bool IncrIsOne = false;
  llvm::Value *initVal = nullptr, *endVal = nullptr;
  if (expr) {

    auto incrExpr = expr->getOperand(3);
    if (auto constVal = llvm::dyn_cast<ConstantVal>(incrExpr)) {
      if (constVal->getInt() == 1) {
        IncrIsOne = true;
      }
    }

    // set dovar value to init.
    doVar = emitExpression(expr->getOperand(0), true);
    initVal = emitExpression(expr->getOperand(1));
    stepVal = emitExpression(expr->getOperand(3));
    endVal = emitExpression(expr->getOperand(2));

    // set tripIndVar to zero.
    if (!IncrIsOne) {
      auto Zero = llvm::ConstantInt::get(initVal->getType(), 0);

      tripIndVar = createAlloca(initVal->getType(), "tripIndVar");
      emitStoreInstruction(Zero, tripIndVar);

      auto UMinusB = IRB->CreateSub(endVal, initVal, "UMinusB");
      auto PlusS = IRB->CreateAdd(UMinusB, stepVal, "UminusBplusS");
      tripCount = IRB->CreateSDiv(PlusS, stepVal, "tripCountVal");
    }

    emitStoreInstruction(initVal, doVar);
  }

  // Create Header where loop comparison happens.
  auto Header = getNewBlock("for.header", true);

  // Body, where the loop block sits.
  auto BodyBB = getNewBlock("for.body", true);

  // Exit block for the loop.
  auto ExitBB = getNewBlock("for.exit");

  // Block where indvar increments happen.
  auto LatchBB = getNewBlock("for.latch");

  CGLoop *cgLoop = new CGLoop(Header, LatchBB, ExitBB, stmt->getName());
  context.currLoopVector.push_back(cgLoop);
  context.stmtLoopMap[stmt] = cgLoop;
  if (!stmt->getName().empty())
    context.nameLoopMap[stmt->getName()] = cgLoop;

  // Redirect to header.
  IRB->CreateBr(Header);

  context.currBB = Header;
  IRB->SetInsertPoint(Header);

  if (expr) {
    llvm::Value *LoopCmp = nullptr;
    if (!IncrIsOne) {
      auto indVarVal = emitLoadInstruction(tripIndVar, "tripIndVar.load");
      // Now emit trip count condition.
      LoopCmp = IRB->CreateICmpSLT(indVarVal, tripCount, "cmpIndvar");
    } else {
      auto doVarLoad = emitLoadInstruction(doVar, "doVar.load");
      auto finalVal = IRB->CreateAdd(endVal, IRB->getInt32(1));
      LoopCmp = IRB->CreateICmpSLT(doVarLoad, finalVal, "cmpDoVar");
    }

    IRB->CreateCondBr(LoopCmp, BodyBB, ExitBB);

  } else {
    IRB->CreateBr(BodyBB);
  }

  // Emit Loop body now.
  context.currBB = BodyBB;
  IRB->SetInsertPoint(BodyBB);

  // Emit the block statements.
  emitExectubaleConstructList(stmt->getBlock()->getStmtList());

  IRB->CreateBr(LatchBB);

  // After the loop body emission, set the current loop to null.
  context.currLoopVector.pop_back();

  // Emit Loop latch now.
  LatchBB->insertInto(context.currFn);
  context.currBB = LatchBB;
  IRB->SetInsertPoint(LatchBB);

  // Increment doVar by step.
  if (expr) {
    auto doVarLoad = emitLoadInstruction(doVar, "doVar.load");
    auto doVarInc = IRB->CreateAdd(doVarLoad, stepVal, "doVar.incr");
    emitStoreInstruction(doVarInc, doVar);

    if (!IncrIsOne) {
      // Increment tripIndVar by 1.
      auto One = llvm::ConstantInt::get(tripCount->getType(), 1);
      auto indVarLoad = emitLoadInstruction(tripIndVar, "indvar.load");
      auto indVarInc = IRB->CreateAdd(indVarLoad, One, "doVar.incr");
      emitStoreInstruction(indVarInc, tripIndVar);
    }
  }

  // Loop back edge to Header.
  auto backEdge = IRB->CreateBr(Header);
  if (context.isParallelLoop) {
    llvm::Metadata *MDList[] = {
        llvm::MDString::get(*LLContext, "llvm.loop.vectorize.enable"),
        llvm::ConstantAsMetadata::get(IRB->getTrue())};

    MDNode *LoopID = MDNode::get(*LLContext, MDList);
    backEdge->setMetadata(llvm::LLVMContext::MD_loop, LoopID);
  }

  // Now set continue IR dumping in Exit.
  ExitBB->insertInto(context.currFn);

  context.currBB = ExitBB;
  IRB->SetInsertPoint(ExitBB);

  context.isParallelLoop = false;

  return true;
}

bool CodeGen::emitMemCpy(CallStmt *stmt) {
  auto dest = emitExpression(stmt->getExpr(0));
  auto destAlloca = llvm::dyn_cast<llvm::AllocaInst>(dest);
  auto source = emitExpression(stmt->getExpr(1));
  auto sourceAlloca = llvm::dyn_cast<llvm::AllocaInst>(source);

  auto sourceSec = llvm::dyn_cast<ArraySection>(stmt->getExpr(0));
  auto destSec = llvm::dyn_cast<ArraySection>(stmt->getExpr(1));
  assert(sourceSec);
  assert(destSec);

  auto sourceTy = sourceAlloca->getType();
  auto destTy = destAlloca->getAllocatedType();

  assert(sourceTy && destTy);

  if (sourceTy->isStructTy()) {
    auto structPtr = source;
    source = IRB->CreateStructGEP(nullptr, source, 0, "source.base");
    source = emitStructLoadInst(source, structPtr, structPtr->getType(), 0,
                                "source.load");
  }

  if (destTy->isStructTy()) {
    auto structPtr = dest;
    dest = IRB->CreateStructGEP(nullptr, dest, 0, "dest.base");
    dest = emitStructLoadInst(dest, structPtr, structPtr->getType(), 0,
                              "dest.base");
    // dest = emitLoadInstruction(dest, "dest.load");
  }

  auto numElement = emitExpression(stmt->getExpr(2));
  IRB->CreateMemCpy(
      dest, dest->getPointerAlignment(TheModule->getDataLayout()), source,
      source->getPointerAlignment(TheModule->getDataLayout()), numElement);
  return true;
}

bool CodeGen::emitCallStmt(CallStmt *stmt) {
  auto symbol = stmt->getCalledFn();
  symbol = symbol->getOrigSymbol();
  auto fcFuncTy = llvm::dyn_cast<fc::FunctionType>(symbol->getType());
  if (!fcFuncTy) {
    llvm_unreachable("Called symbol is not a function.");
  }
  auto argsList = stmt->getArgsList();
  if (symbol->getName() == "memcpy") {
    return emitMemCpy(stmt);
  }

  if (symbol->getName() == "get_command_argument") {
    assert(stmt->getNumOperands() == 2);
    auto pos = emitExpression(stmt->getExpr(0));
    auto var = emitExpression(stmt->getExpr(1), true);
    auto argvBase = emitLoadInstruction(context.ArgV);
    auto argvGEP = IRB->CreateInBoundsGEP(argvBase, {pos}, "argv.load");

    llvm::Value *argvFinal = emitLoadInstruction(argvGEP);

    auto arrayType = llvm::dyn_cast<ArrayType>(stmt->getExpr(1)->getType());
    assert(arrayType && !arrayType->isDynArrayTy());
    auto list = arrayType->getBoundsList();
    assert(list.size() == 1);
    unsigned size = list[0].second - list[0].first + 1;
    IRB->CreateMemCpy(
        var, var->getPointerAlignment(TheModule->getDataLayout()), argvFinal,
        argvFinal->getPointerAlignment(TheModule->getDataLayout()), size);

    // TODO Use whichever more appropriate
    /*
    auto Fn = runtimeHelper->getStrCpyFunction();
    llvm::Value *val = IRB->CreateBitCast(var, IRB->getInt8PtrTy());
    IRB->CreateCall(Fn, {val, argvFinal});
    */
    return true;
  }
  if (symbol->getName() == "trim") {
    assert(stmt->getNumOperands() == 1);
    auto arg1 = emitExpression(argsList[0], true);
    auto castVal = IRB->CreateBitCast(arg1, IRB->getInt8PtrTy());

    auto arg1Ty = arg1->getType()->getPointerElementType();
    assert(arg1Ty && arg1Ty->isArrayTy());
    auto arrTy = cast<llvm::ArrayType>(arg1Ty);
    assert(arrTy->getElementType()->isSingleValueType());
    auto size = arrTy->getNumElements();

    auto llvmFuncTy = llvm::FunctionType::get(
        IRB->getVoidTy(), {castVal->getType(), IRB->getInt32Ty()}, false);
    auto trimFunc = cast<llvm::Function>(
        TheModule->getOrInsertFunction("__fc_runtime_trim", llvmFuncTy));
    trimFunc->addFnAttr(llvm::Attribute::ArgMemOnly);
    return IRB->CreateCall(trimFunc, {castVal, IRB->getInt32(size)});
  }

  if (symbol->getName() == "system_clock") {
    assert(stmt->getNumOperands() == 3);
    SmallVector<llvm::Value *, 2> funcArgsList;
    bool isInt = true;
    for (unsigned i = 0; i < argsList.size(); ++i) {
      auto assignmentExpr = llvm::dyn_cast<AssignmentExpr>(argsList[i]);
      if (!assignmentExpr->getExpr()->getType()->isIntegralTy())
        isInt = false;
      assert(assignmentExpr);
      funcArgsList.push_back(emitExpression(assignmentExpr->getExpr(), true));
    }

    // Currently only handling int
    assert(isInt);
    auto Fn = runtimeHelper->getISysClockFunction();
    IRB->CreateCall(Fn, funcArgsList);
    return true;
  }
  return emitCall(symbol, argsList, true);
}

llvm::CallInst *CodeGen::emitCall(Symbol *symbol, ExprList &list,
                                  bool isSubroutineCall) {
  auto fcFuncTy = llvm::dyn_cast<fc::FunctionType>(symbol->getType());
  if (!fcFuncTy) {
    llvm_unreachable("Called symbol is not a function.");
  }
  auto PU = cgHelper->getCalledProgramUnit(symbol);
  auto fnName = cgHelper->getFunctionNameForSymbol(symbol);

  bool isUndeclaredFn =
      fcFuncTy == fc::FunctionType::getUndeclaredFuncTy(fcFuncTy->getContext());

  llvm::Function *LLFunction = nullptr;
  if (!PU) {
    if (isSubroutineCall && isUndeclaredFn) {
      fcFuncTy =
          fc::FunctionType::getVoidUndeclaredFuncTy(fcFuncTy->getContext());
    }
    auto FuncTy =
        static_cast<llvm::FunctionType *>(cgHelper->getLLVMTypeFor(fcFuncTy));

    LLFunction = TheModule->getFunction(fnName);
    if (!LLFunction)
      LLFunction = static_cast<llvm::Function *>(
          TheModule->getOrInsertFunction(fnName, FuncTy));
  } else {
    LLFunction = TheModule->getFunction(fnName);
  }

  // Function is in same module and yet to be emitted
  // We emit a declaration in this case
  if (!LLFunction) {
    auto FuncTy =
        static_cast<llvm::FunctionType *>(cgHelper->getLLVMTypeFor(fcFuncTy));

    LLFunction = static_cast<llvm::Function *>(
        TheModule->getOrInsertFunction(fnName, FuncTy));
  }

  /*
  if (!LLFunction) {
    error() << "\n looking for function : " << fnName;
    llvm_unreachable("\nCould not find the llvm Function in CallStmt\n");
    return nullptr;
  }
  */

  llvm::SmallVector<llvm::Value *, 2> InitArgsList, ArgsList;

  for (auto expr : list) {
    auto exprVal = emitExpression(expr, true);
    InitArgsList.push_back(exprVal);
  }

  bool hasFrameArg = false;

  // If the current function has a parent PU and if it has
  // a frame arg, add that to the current arg list.
  if (PU && PU->isNestedUnit()) {
    auto helper = cgHelper->getSubPUHelper(PU->getParent());
    if (helper && helper->hasFrameArg) {
      if (PU->getParent() != context.currPU) {
        llvm::Argument *arg = &*context.currFn->args().begin();
        assert(arg);
        assert(arg->getType()->isPointerTy());
        assert(arg->getType()->getPointerElementType() == helper->frameTy);
        ArgsList.push_back(arg);
      } else {
        auto frameName = "FRAME." + context.currPU->getName().str();
        auto val = context.getLLVMValueFor(frameName);
        assert(val);
        ArgsList.push_back(val);
      }
      hasFrameArg = true;
    }
  }

  auto FnTy = (LLFunction->getFunctionType());
  for (unsigned I = 0; I < InitArgsList.size(); ++I) {

    auto llArgVal = InitArgsList[I];
    auto fcCurrArgTy = list[I]->getType();
    fc::Type *fcDummArgTy = nullptr;
    if (!isUndeclaredFn)
      fcDummArgTy = fcFuncTy->getArgType(I);

    auto currArg =
        getArgumentFor(llArgVal, fcCurrArgTy, fcDummArgTy, LLFunction, I);

    // Now do bitcast to the original actual argument of function.
    if (!isUndeclaredFn) {
      auto funcParamType = FnTy->getParamType(hasFrameArg ? I + 1 : I);
      assert(currArg->getType() == funcParamType);
    }
    ArgsList.push_back(currArg);
  }

  unsigned totalArgs = fcFuncTy->getArgList().size();
  // Fill null values when optinal arguments are left out
  if (ArgsList.size() < totalArgs) {
    for (unsigned i = ArgsList.size(); i < totalArgs; ++i) {
      auto tempArg = llvm::Constant::getNullValue(
          cgHelper->getLLVMTypeFor(fcFuncTy->getArgType(i))->getPointerTo());
      ArgsList.push_back(tempArg);
    }
  }

  llvm::CallInst *call = nullptr;
  if (isUndeclaredFn & isSubroutineCall) {
    llvm::SmallVector<llvm::Type *, 2> argList;
    for (auto arg : ArgsList) {
      argList.push_back(arg->getType());
    }

    auto actualFuncTy =
        llvm::FunctionType::get(IRB->getVoidTy(), argList, false);
    auto bitcastFunc =
        IRB->CreateBitCast(LLFunction, actualFuncTy->getPointerTo());
    call = IRB->CreateCall(bitcastFunc, ArgsList);
  } else {
    call = IRB->CreateCall(LLFunction, ArgsList);
  }
  for (unsigned I = 0; I < ArgsList.size(); ++I) {
    call->addParamAttr(I, llvm::Attribute::NoAlias);
  }
  return call;
}

bool CodeGen::emitExitStmt(ExitStmt *stmt) {
  llvm::BasicBlock *exitBB;

  if (stmt->hasConstructName()) {
    assert(context.nameLoopMap.find(stmt->getConstructName()) !=
           context.nameLoopMap.end());
    auto cgLoop = context.nameLoopMap[stmt->getConstructName()];
    exitBB = cgLoop->getExitBB();
  } else {
    assert(context.currLoopVector.size() > 0);
    exitBB = context.currLoopVector.back()->getExitBB();
  }

  auto nextBB = getNewBlock("for.no.cycle", true);

  IRB->CreateCondBr(IRB->getTrue(), exitBB, nextBB);

  context.currBB = nextBB;
  IRB->SetInsertPoint(nextBB);
  return true;
}

bool CodeGen::emitCycleStmt(CycleStmt *stmt) {
  llvm::BasicBlock *latchBB;

  if (stmt->hasConstructName()) {
    assert(context.nameLoopMap.find(stmt->getConstructName()) !=
           context.nameLoopMap.end());
    auto cgLoop = context.nameLoopMap[stmt->getConstructName()];
    latchBB = cgLoop->getLatchBB();
  } else {
    assert(context.currLoopVector.size() > 0);
    latchBB = context.currLoopVector.back()->getLatchBB();
  }

  auto nextBB = getNewBlock("for.no.cycle", true);

  IRB->CreateCondBr(IRB->getTrue(), latchBB, nextBB);

  context.currBB = nextBB;
  IRB->SetInsertPoint(nextBB);
  return true;
}

llvm::Value *CodeGen::getDynamicArrayFor(llvm::Value *val,
                                         fc::ArrayType *staticArrTy,
                                         fc::ArrayType *dynArrTy) {

  assert(!staticArrTy->boundsEmpty());
  if (!dynArrTy) {
    dynArrTy = fc::ArrayType::get(staticArrTy->getContext(),
                                  staticArrTy->getElementTy(),
                                  staticArrTy->getNumDims());
  }

  auto StructTy = cgHelper->getLLVMTypeFor(dynArrTy);
  assert(StructTy);

  auto NewVal = createAlloca(StructTy, val->getName().str() + ".dynamic");

  // Store the base pointer.
  auto name = val->getName();
  auto baseTy = StructTy->getContainedType(0);
  auto BC = IRB->CreateBitCast(val, baseTy);
  auto basePtr = IRB->CreateStructGEP(nullptr, NewVal, 0, name + ".base");
  // emitStoreInstruction(BC, basePtr);
  emitStructStoreInst(BC, basePtr, NewVal, NewVal->getType(), 0);
  // Now store the dims.
  auto dimsArr = IRB->CreateStructGEP(nullptr, NewVal, 1, name + ".dimsArr");

  unsigned I = 0;
  for (auto bounds : staticArrTy->getBoundsList()) {
    auto Dim = IRB->CreateInBoundsGEP(
        dimsArr, {IRB->getInt32(0), IRB->getInt32(I)}, name + ".dim");

    auto LB = IRB->CreateStructGEP(nullptr, Dim, 0, name + ".lb");
    // emitStoreInstruction(IRB->getInt64(bounds.first), LB);
    emitStructStoreInst(IRB->getInt64(bounds.first), LB, Dim, Dim->getType(),
                        0);

    auto UB = IRB->CreateStructGEP(nullptr, Dim, 1, name + ".ub");
    // emitStoreInstruction(IRB->getInt64(bounds.second), UB);
    emitStructStoreInst(IRB->getInt64(bounds.second), UB, Dim, Dim->getType(),
                        1);

    auto sizeVal = bounds.second - bounds.first + 1;
    auto Size = IRB->CreateStructGEP(nullptr, Dim, 2, name + ".size");
    // emitStoreInstruction(IRB->getInt64(sizeVal), Size);
    emitStructStoreInst(IRB->getInt64(sizeVal), Size, Dim, Dim->getType(), 2);
    I++;
  }
  return NewVal;
}

llvm::Value *CodeGen::getArgumentFor(llvm::Value *currArg, fc::Type *currArgTy,
                                     fc::Type *fcDummyArgTy,
                                     llvm::Function *llFn, unsigned argNum) {
  if (currArgTy->isStructTy()) {
    llvm::Type *llFuncArgType = llFn->getFunctionType()->getParamType(argNum);
    llvm::Type *llCurrArgType = currArg->getType();

    // This happens when multiple program-units inheriting the same derived-type
    // from a common module. Each will have a local llvm::StructType which are
    // equivalent, so we can safely bit-cast here
    if (llFuncArgType != llCurrArgType) {
      currArg = IRB->CreateBitCast(currArg, llFuncArgType);
    }
  }

  // If the current value is expression and not a memory location
  // create new alloca, store the expression value and then
  // pass it.
  if (!currArg->getType()->isPointerTy()) {
    auto argName = llFn->getName().str() + ".arg" + std::to_string(argNum + 1);
    llvm::Value *Alloca = nullptr;

    if (auto constant = llvm::dyn_cast<llvm::Constant>(currArg)) {
      auto global = new llvm::GlobalVariable(
          *TheModule, constant->getType(), true,
          llvm::GlobalValue::InternalLinkage, constant, argName);

      Alloca = global;

    } else {
      Alloca = createAlloca(currArg->getType(), argName);
      emitStoreInstruction(currArg, Alloca);
    }
    return Alloca;
  }

  if (currArgTy->isArrayTy()) {
    auto fcArrayTy = cast<fc::ArrayType>(currArgTy);
    auto dynArr = fcArrayTy->boundsEmpty();
    fc::ArrayType *fcDummArrayTy = nullptr;
    if (fcDummyArgTy)
      fcDummArrayTy = dyn_cast<fc::ArrayType>(fcDummyArgTy);

    bool dummyDynArr = false;
    if (fcDummArrayTy == nullptr)
      dummyDynArr = true;
    else {
      dummyDynArr = fcDummArrayTy->boundsEmpty();
    }

    // Everything is either dynamic/static. So, no issues.
    if (dynArr == dummyDynArr) {
      return currArg;
    }

    // Actual argument is not dynamic. But the dummy
    // argument is expecting the dynamic array.
    if (!dynArr && dummyDynArr) {
      if (std == Standard::f77) {
        auto baseEle = cgHelper->getLLVMTypeFor(fcArrayTy->getElementTy());
        auto arrTy = llvm::ArrayType::get(baseEle, 0);
        assert(arrTy);
        return IRB->CreateBitCast(currArg, baseEle->getPointerTo());
      }
      return getDynamicArrayFor(currArg, fcArrayTy, fcDummArrayTy);
    }

    // If the current array is dynamic,
    // and the dummy argument is accepting static array,
    // we do not know how to deal with this.
    if (dynArr && !dummyDynArr) {
      llvm_unreachable("Function expecting static array");
    }
  }

  return currArg;
}

bool CodeGen::emitExectubaleConstructList(StmtList &stmtList) {
  if (stmtList.empty())
    return true;

  for (auto actionStmt : stmtList) {
    if (isa<Expr>(actionStmt)) {
      continue;
    }

    setCurrLineForDebug(actionStmt->getSourceLoc());

    switch (actionStmt->getStmtType()) {
    case AssignmentStmtKind:
      emitAssignment(static_cast<AssignmentStmt *>(actionStmt));
      break;
    case StopStmtKind:
      emitStopStmt(static_cast<StopStmt *>(actionStmt));
      break;
    case PrintStmtKind:
      emitPrintStmt(static_cast<PrintStmt *>(actionStmt));
      break;
    case WriteStmtKind:
      emitWriteStmt(static_cast<WriteStmt *>(actionStmt));
      break;
    case ReadStmtKind:
      emitReadStmt(static_cast<ReadStmt *>(actionStmt));
      break;
    case OpenStmtKind:
      emitOpenStmt(static_cast<OpenStmt *>(actionStmt));
      break;
    case CloseStmtKind:
      emitCloseStmt(static_cast<CloseStmt *>(actionStmt));
      break;
    case IfElseStmtKind:
      emitIfElseStmt(static_cast<IfElseStmt *>(actionStmt));
      break;
    case DoWhileStmtKind:
      emitDoWhileStmt(static_cast<DoWhileStmt *>(actionStmt));
      break;
    case DoStmtKind:
      emitDoStmt(static_cast<DoStmt *>(actionStmt));
      break;
    case CallStmtKind:
      emitCallStmt(static_cast<CallStmt *>(actionStmt));
      break;
    case CycleStmtKind:
      emitCycleStmt(static_cast<CycleStmt *>(actionStmt));
      break;
    case ExitStmtKind:
      emitExitStmt(static_cast<ExitStmt *>(actionStmt));
      break;
    case NullifyStmtKind:
      emitNullifyStmt(static_cast<NullifyStmt *>(actionStmt));
      break;
    case AllocateStmtKind:
      emitAllocateStmt(static_cast<AllocateStmt *>(actionStmt));
      break;
    case DeAllocateStmtKind:
      emitDeAllocateStmt(static_cast<DeAllocateStmt *>(actionStmt));
      break;
    case ReturnStmtKind: {
      auto returnStmt = static_cast<ReturnStmt *>(actionStmt);
      auto exitVal = returnStmt->getExpr();
      assert(!exitVal);
      IRB->CreateRetVoid();

      auto newBlock = getNewBlock("deadBlock", true);
      IRB->SetInsertPoint(newBlock);
      break;
    }
    case PointerAssignmentStmtKind:
      emitPointerAssignment(static_cast<PointerAssignmentStmt *>(actionStmt));
      break;

    default:
      llvm_unreachable("Unknown Stmt kind");
    };
  }
  return true;
}

bool CodeGen::emitExecutionPart(ExecutionPart *execPart) {
  if (!execPart)
    return true;

  return emitExectubaleConstructList(execPart->getBlock()->getStmtList());
}
