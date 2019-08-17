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
#include "AST/ProgramUnit.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "codegen/CGASTHelper.h"
#include "codegen/CodeGen.h"
#include "codegen/LLVMUtil.h"
#include "codegen/RuntimeHelper.h"
#include "common/Debug.h"
#include "sema/Intrinsics.h"

#include "AST/Expressions.h"
#include "llvm-c/Target.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

using namespace fc;
using namespace ast;
using namespace llvm;

Value *CodeGen::emitConstant(llvm::ArrayRef<llvm::StringRef> valueList,
                             fc::Type *fcType, fc::Type *lhsTy,
                             bool constructOnlyConstant) {
  bool isString = false;
  auto ArrTy = dyn_cast<ArrayType>(fcType);
  if ((ArrTy && ArrTy->getElementTy()->isStringCharTy()) ||
      fcType->isCharacterTy()) {
    isString = true;
  }
  auto LLTy = cgHelper->getLLVMTypeFor(fcType);

  std::string valString = valueList[0];
  llvm::StringRef value(valString.c_str(), valString.size());

  if (isString) {
    // We can not use IRBuilder here as incase of modules string consts
    // IRBuilder is yet to be initialised
    if (lhsTy) {
      auto arrayTy = llvm::dyn_cast<fc::ArrayType>(lhsTy);
      assert(arrayTy);
      assert(!arrayTy->isDynArrayTy());
      auto bounds = arrayTy->getBoundsList();
      assert(bounds.size() == 1);
      auto stringSize = bounds[0].second - bounds[0].first;
      for (unsigned i = value.size(); i < stringSize; ++i) {
        valString.push_back('\0');
      }
    }
    llvm::StringRef finalValue(valString.c_str(), valString.size());
    llvm::Constant *StrConstant =
        llvm::ConstantDataArray::getString(TheModule->getContext(), finalValue);

    if (constructOnlyConstant)
      return StrConstant;

    auto *GV = new llvm::GlobalVariable(*TheModule, StrConstant->getType(),
                                        true, llvm::GlobalValue::PrivateLinkage,
                                        StrConstant, "str", nullptr,
                                        llvm::GlobalVariable::NotThreadLocal);
    GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    return GV;
  }

  if (LLTy->isIntegerTy()) {
    long int val = INT_MIN;

    if (LLTy->isIntegerTy(1))
      val = (value.compare_lower(".false.") == 0) ? 0 : 1;
    else if (LLTy->isIntegerTy(64))
      val = std::stol(value);
    else if (LLTy->isIntegerTy(128)) {
      assert(false);
      llvm::APInt apInt(128, val, 10);
      return IRB->getInt(apInt);
    } else if (LLTy->isIntegerTy(8)) {
      assert(value.size() == 1);
      llvm::APInt apInt(8, value.str()[0], 10);
      return IRB->getInt(apInt);
    } else if (LLTy->isIntegerTy(32)) {
      val = std::stoi(value);
    } else {
      errs() << *LLTy;
      assert(false && "unhandled integer type");
    }

    return IRB->getIntN(LLTy->getScalarSizeInBits(), val);
  }

  if (LLTy->isArrayTy()) {
    auto llArrTy = llvm::cast<llvm::ArrayType>(LLTy);
    auto fcArrTy = llvm::cast<fc::ArrayType>(fcType);
    llvm::SmallVector<llvm::Constant *, 2> constValList;
    for (auto val : valueList) {
      auto constVal = llvm::cast<llvm::Constant>(
          emitConstant(val, fcArrTy->getElementTy()));
      constValList.push_back(constVal);
    }
    auto constArr = ConstantArray::get(llArrTy, constValList);
    return constArr;
  }

  assert(LLTy->isFloatingPointTy());
  auto str = value.str();
  for (auto &ch : str) {
    if (ch == 'd') {
      ch = 'e';
      LLTy = IRB->getDoubleTy();
    }
  }
  auto Const = ConstantFP::get(LLTy, str);
  return Const;
}

llvm::Value *CodeGen::castIntToFP(llvm::Value *val, llvm::Type *castType) {
  assert(val->getType()->isIntegerTy());
  auto IntTy = static_cast<IntegerType *>(val->getType());
  auto bitWidth = IntTy->getBitWidth();
  if (bitWidth == 64) {
    // convert to float
    castType = IRB->getDoubleTy();
  } else if (bitWidth > 64) {
    assert(false && "unhandled integer type");
  } else {
    assert(bitWidth == 32);
    // TODO: hande other smaller types.
  }
  return IRB->CreateSIToFP(val, castType, "castforPow");
}

llvm::Value *CodeGen::getLLVMBinaryOp(llvm::Value *lhsVal, llvm::Value *rhsVal,
                                      fc::ast::BinaryOpKind opKind) {

  // This conversion is required as some intrinsics are resoved in codegen
  // Type of binaryexpressions involving these will not resolved in sema.
  // Either we have to explicitely infer intrinsic type or do this
  if (lhsVal->getType()->isIntegerTy(32) &&
      rhsVal->getType()->isIntegerTy(64)) {
    lhsVal = IRB->CreateSExt(lhsVal, rhsVal->getType());
  }

  if (lhsVal->getType()->isIntegerTy(64) &&
      rhsVal->getType()->isIntegerTy(32)) {
    rhsVal = IRB->CreateSExt(rhsVal, lhsVal->getType());
  }

  if (lhsVal->getType()->isFloatTy() && rhsVal->getType()->isDoubleTy()) {
    lhsVal = IRB->CreateFPExt(lhsVal, rhsVal->getType());
  }

  if (rhsVal->getType()->isFloatTy() && lhsVal->getType()->isDoubleTy()) {
    rhsVal = IRB->CreateFPExt(rhsVal, lhsVal->getType());
  }

  if (rhsVal->getType()->isFloatingPointTy() &&
      lhsVal->getType()->isIntegerTy()) {
    lhsVal = IRB->CreateSIToFP(lhsVal, rhsVal->getType());
  }

  if (lhsVal->getType()->isFloatingPointTy() &&
      rhsVal->getType()->isIntegerTy()) {
    rhsVal = IRB->CreateSIToFP(rhsVal, lhsVal->getType());
  }

  bool isInt = lhsVal->getType()->isIntegerTy();
  bool isRHSInt = rhsVal->getType()->isIntegerTy();
  auto OpType = Instruction::Add;

  switch (opKind) {
  case BinaryOpKind::Concat: {
    auto Fn = runtimeHelper->getStrCatFunction();
    lhsVal = IRB->CreateBitCast(lhsVal, IRB->getInt8PtrTy());
    rhsVal = IRB->CreateBitCast(rhsVal, IRB->getInt8PtrTy());
    return IRB->CreateCall(Fn, {lhsVal, rhsVal});
  }

  case BinaryOpKind::Addition:
    OpType = isInt ? Instruction::Add : Instruction::FAdd;
    break;
  case BinaryOpKind::Subtraction:
    OpType = isInt ? Instruction::Sub : Instruction::FSub;
    break;
  case BinaryOpKind::Multiplication:
    OpType = isInt ? Instruction::Mul : Instruction::FMul;
    break;
  case BinaryOpKind::Division:
    OpType = isInt ? Instruction::SDiv : Instruction::FDiv;
    break;
  case BinaryOpKind::Power: {
    // TODO: check if
    auto OpIntrin = Intrinsic::pow;

    bool convertedRHS = false;
    bool convertedLHS = false;
    auto OrigType = lhsVal->getType();

    if (isInt) {
      lhsVal = castIntToFP(lhsVal, IRB->getFloatTy());
      convertedLHS = true;
    }

    if (isInt && isRHSInt) {
      rhsVal = castIntToFP(rhsVal, lhsVal->getType());
      convertedRHS = true;
    }

    if (!isInt && isRHSInt)
      OpIntrin = Intrinsic::powi;

    if (lhsVal->getType() != rhsVal->getType() &&
        rhsVal->getType()->isFloatingPointTy() &&
        lhsVal->getType()->isFloatingPointTy()) {
      rhsVal = IRB->CreateFPCast(rhsVal, lhsVal->getType(), "fpcast");
    }

    Value *finalVal = nullptr;
    if (auto constRHS = llvm::dyn_cast<llvm::ConstantFP>(rhsVal)) {
      if (constRHS->isExactlyValue(2.0)) {
        finalVal = IRB->CreateFMul(lhsVal, lhsVal, "pow");
      } else if (constRHS->isExactlyValue(0.75)) {
        auto sqrt = IRB->CreateIntrinsic(llvm::Intrinsic::sqrt, {lhsVal},
                                         nullptr, "pow.sqrt");
        auto sqrtOfSqrt = IRB->CreateIntrinsic(llvm::Intrinsic::sqrt, {sqrt},
                                               nullptr, "pow.sqrt");
        finalVal = IRB->CreateFMul(sqrt, sqrtOfSqrt, "pow.final");
      }
    }

    if (!finalVal)
      finalVal = IRB->CreateBinaryIntrinsic(OpIntrin, lhsVal, rhsVal);

    if (convertedLHS && convertedRHS) {
      return IRB->CreateFPToSI(finalVal, OrigType);
    }
    return finalVal;
  }
  default:
    llvm_unreachable("unhandled llvm binary op");
  };

  assert(lhsVal->getType() == rhsVal->getType());

  return IRB->CreateBinOp(OpType, lhsVal, rhsVal);
}

llvm::Value *CodeGen::emitStrCmp(llvm::Value *lhsVal, llvm::Value *rhsVal,
                                 RelationalOpKind opKind) {
  auto I8Ptr = IRB->getInt8PtrTy();
  auto arg1 = IRB->CreateBitCast(lhsVal, I8Ptr);
  auto arg2 = IRB->CreateBitCast(rhsVal, I8Ptr);
  auto Fn = runtimeHelper->getStrCmpFunction();
  auto cmpVal = IRB->CreateCall(Fn, {arg1, arg2});

  switch (opKind) {
  case RelationalOpKind::EQ:
    return IRB->CreateICmpEQ(cmpVal, IRB->getInt32(0));
  case RelationalOpKind::NE:
    return IRB->CreateICmpNE(cmpVal, IRB->getInt32(0));
  default:
    llvm_unreachable("String comparision of this type is not handled yet");
  }
}

llvm::Value *CodeGen::getLLVMRelationalOp(llvm::Value *lhsVal,
                                          llvm::Value *rhsVal,
                                          RelationalOpKind opKind) {

  auto isInteger = lhsVal->getType()->isIntegerTy();
  switch (opKind) {
  case RelationalOpKind::EQ:
    if (isInteger)
      return IRB->CreateICmpEQ(lhsVal, rhsVal);
    // TODO: using ordered float compare. Refer to fortran standard.
    return IRB->CreateFCmpOEQ(lhsVal, rhsVal);
  case RelationalOpKind::NE:
    if (isInteger)
      return IRB->CreateICmpNE(lhsVal, rhsVal);
    return IRB->CreateFCmpONE(lhsVal, rhsVal);
  case RelationalOpKind::LT:
    if (isInteger)
      return IRB->CreateICmpSLT(lhsVal, rhsVal);
    return IRB->CreateFCmpOLT(lhsVal, rhsVal);
  case RelationalOpKind::LE:
    if (isInteger)
      return IRB->CreateICmpSLE(lhsVal, rhsVal);
    return IRB->CreateFCmpOLE(lhsVal, rhsVal);
  case RelationalOpKind::GT:
    if (isInteger)
      return IRB->CreateICmpSGT(lhsVal, rhsVal);
    return IRB->CreateFCmpOGT(lhsVal, rhsVal);
  case RelationalOpKind::GE:
    if (isInteger)
      return IRB->CreateICmpSGE(lhsVal, rhsVal);
    return IRB->CreateFCmpOGE(lhsVal, rhsVal);
  default:
    llvm_unreachable("unhandled llvm  relational op");
  };
  return nullptr;
}

llvm::Value *CodeGen::getLLVMLogicalOp(llvm::Value *lhsVal, llvm::Value *rhsVal,
                                       LogicalOpKind opKind) {
  if (opKind != LogicalOpKind::NOT)
    assert(lhsVal->getType()->isIntegerTy(1) && "Expecting logical value");
  assert(rhsVal->getType()->isIntegerTy(1) && "Expecting logical value");
  switch (opKind) {
  case LogicalOpKind::AND:
    return IRB->CreateAnd(lhsVal, rhsVal);
  case LogicalOpKind::NOT:
    return IRB->CreateNot(rhsVal);
  case LogicalOpKind::OR:
    return IRB->CreateOr(lhsVal, rhsVal);
  case LogicalOpKind::NEQV:
    return IRB->CreateXor(lhsVal, rhsVal);
  case LogicalOpKind::EQV:
    return IRB->CreateNot(IRB->CreateXor(lhsVal, rhsVal));
  default:
    llvm_unreachable("unhandled logical op");
  }
  return nullptr;
}

Value *CodeGen::emitSizeForArrBounds(ArrayBounds &bounds) {
  llvm::Value *lowerVal = nullptr;
  lowerVal = IRB->getInt64(bounds.first);
  auto upperVal = IRB->getInt64(bounds.second);

  // Size of the current dimension is {upper - lower + 1}
  auto sizeVal = IRB->CreateSub(upperVal, lowerVal);
  sizeVal = IRB->CreateAdd(sizeVal, IRB->getInt64(1));
  return sizeVal;
}

Value *CodeGen::emitDynArrayElement(ArrayElement *arrEle, bool isLHS,
                                    llvm::Value *addr) {
  llvm::Value *LLValue;

  if (!addr)
    LLValue = context.getLLVMValueFor(arrEle->getName());
  else
    LLValue = addr;

  auto fcTy = arrEle->getSymbol()->getOrigType();

  fc::ArrayType *fcArrayTy;
  if (auto fcPointerTy = llvm::dyn_cast<fc::PointerType>(fcTy)) {
    fcArrayTy = llvm::dyn_cast<fc::ArrayType>(fcPointerTy->getElementType());

    if (isLHS)
      LLValue =
          emitLoadInstruction(LLValue, LLValue->getName().str() + ".loadptr",
                              /* disableTBAA = */ true);

  } else {
    fcArrayTy = llvm::dyn_cast<fc::ArrayType>(fcTy);
  }

  assert(fcArrayTy && fcArrayTy->getBoundsList().empty());

  auto llStructTy = LLVMUtil::getStructTypeForAlloc(LLValue);
  auto llArrTy = dyn_cast<llvm::ArrayType>(llStructTy->getContainedType(1));
  assert(llArrTy);

  auto list = arrEle->getSubscriptList();

  // FIXME
  // Not necessarily true, for example,
  // character(line_length), dimension(:), intent(in) :: keywords
  // or we have to push nullptr to dimsarray!
  // assert(numDims == list.size());

  auto numDims = list.size();
  // First emit all the subs.
  llvm::SmallVector<llvm::Value *, 2> llSubs, LBs, SizeList;
  for (auto expr : list) {
    auto llExpr = emitExpression(expr);
    assert(llExpr);
    llExpr = IRB->CreateSExt(llExpr, IRB->getInt64Ty());
    llSubs.push_back(llExpr);
  }

  // Load the array type.
  auto DimArr = IRB->CreateStructGEP(nullptr, LLValue, 1, "dimsArr");
  // Get lower bound and size list of each dimension.
  for (unsigned I = 0; I < numDims; ++I) {

    auto Dim = IRB->CreateInBoundsGEP(
        DimArr, {IRB->getInt32(0), IRB->getInt32(I)}, "dim");

    auto LB = IRB->CreateStructGEP(nullptr, Dim, 0, "lb");
    // LB = emitLoadInstruction(LB, "lb.load");
    LB = emitStructLoadInst(LB, Dim, Dim->getType(), 0, "lb.load");
    LBs.push_back(LB);

    if (I < numDims - 1) {
      auto Size = IRB->CreateStructGEP(nullptr, Dim, 2, "size");
      Size = emitStructLoadInst(Size, Dim, Dim->getType(), 2, "size.load");
      SizeList.push_back(Size);
    }
  }

  // Get the actual offset for every dimension.
  for (unsigned I = 1; I < SizeList.size(); ++I) {
    SizeList[I] = IRB->CreateMul(SizeList[I - 1], SizeList[I],
                                 "size" + std::to_string(I + 1));
  }

  llvm::Value *offsetVal = nullptr;
  for (unsigned I = numDims - 1; I >= 0; --I) {
    auto sub = llSubs[I];
    auto LB = LBs[I];

    if (sub->getType()->isIntegerTy(32)) {
      sub = IRB->CreateSExt(sub, LB->getType());
    }
    if (I == 0) {
      if (offsetVal) {
        offsetVal = IRB->CreateSub(offsetVal, LB, "negatelast");
        offsetVal = IRB->CreateAdd(offsetVal, sub, "addsub");
      } else {
        offsetVal = IRB->CreateSub(sub, LB);
      }
      break;
    }

    llvm::Value *currExprVal = nullptr;
    // Multiply the expression with other upper dimension.
    auto currDim = IRB->CreateMul(sub, SizeList[I - 1]);
    auto minusLB = IRB->CreateMul(LB, SizeList[I - 1], "negate");
    currExprVal = IRB->CreateSub(currDim, minusLB);
    if (I == numDims - 1)
      offsetVal = currExprVal;
    else
      offsetVal = IRB->CreateAdd(offsetVal, currExprVal);
  }

  auto CurrBase = IRB->CreateStructGEP(nullptr, LLValue, 0, "base");
  // CurrBase = emitLoadInstruction(CurrBase, "base.load");
  CurrBase =
      emitStructLoadInst(CurrBase, LLValue, LLValue->getType(), 0, "base");

  CurrBase = IRB->CreateInBoundsGEP(
      CurrBase, offsetVal, arrEle->getName() + (isLHS ? ".write" : ".read"));

  if (isLHS)
    return CurrBase;

  return emitLoadInstruction(CurrBase, arrEle->getName() + ".read");
}

Value *CodeGen::emitf77DynArrayElement(ArrayElement *arrEle, bool isLHS,
                                       llvm::Value *addr) {
  llvm::Value *LLValue;
  if (!addr)
    LLValue = context.getLLVMValueFor(arrEle->getName());
  else
    LLValue = addr;

  auto fcTy = arrEle->getSymbol()->getOrigType();
  auto fcArrayTy = llvm::dyn_cast<fc::ArrayType>(fcTy);

  auto numDims = fcArrayTy->getNumDims();

  auto arrSpec =
      context.currPU->getSpec()->getArraySpecFor(arrEle->getSymbol());
  assert(arrSpec);

  auto boundsList = arrSpec->getBoundsList();
  auto list = arrEle->getSubscriptList();
  assert(numDims == list.size());

  // First emit all the subs.
  llvm::SmallVector<llvm::Value *, 2> llSubs, LBs, SizeList;
  for (auto expr : list) {
    auto llExpr = emitExpression(expr);
    assert(llExpr);
    llExpr = IRB->CreateSExt(llExpr, IRB->getInt64Ty());
    llSubs.push_back(llExpr);
  }

  // Load the array type.
  // Get lower bound and size list of each dimension.
  for (unsigned I = 0; I < numDims; ++I) {

    auto LB = emitExpression(boundsList[I].first);
    LB = IRB->CreateSExt(LB, IRB->getInt64Ty());
    LBs.push_back(LB);

    // We do not need to compute last dimension.
    if (I < numDims - 1) {
      auto UB = emitExpression(boundsList[I].second);
      UB = IRB->CreateSExt(UB, IRB->getInt64Ty());
      auto sizeVal = IRB->CreateSub(UB, LB, "sizeMinus1");
      sizeVal = IRB->CreateAdd(sizeVal, IRB->getInt64(1),
                               "size_of_" + std::to_string(I + 1));
      SizeList.push_back(sizeVal);
    }
  }

  // Get the actual offset for every dimension.
  for (unsigned I = 1; I < SizeList.size(); ++I) {
    SizeList[I] = IRB->CreateMul(SizeList[I - 1], SizeList[I],
                                 "size" + std::to_string(I + 1));
  }

  llvm::Value *offsetVal = nullptr;
  for (unsigned I = numDims - 1; I >= 0; --I) {
    auto sub = llSubs[I];
    auto LB = LBs[I];
    if (sub->getType()->isIntegerTy(32)) {
      sub = IRB->CreateSExt(sub, LB->getType());
    }
    if (I == 0) {
      if (offsetVal) {
        offsetVal = IRB->CreateSub(offsetVal, LB, "negatelast");
        offsetVal = IRB->CreateAdd(offsetVal, sub, "addsub");
      } else {
        offsetVal = IRB->CreateSub(sub, LB);
      }
      break;
    }

    llvm::Value *currExprVal = nullptr;
    // Multiply the expression with other upper dimension.
    auto currDim = IRB->CreateMul(sub, SizeList[I - 1]);
    auto minusLB = IRB->CreateMul(LB, SizeList[I - 1], "negate");
    currExprVal = IRB->CreateSub(currDim, minusLB);
    if (I == numDims - 1)
      offsetVal = currExprVal;
    else
      offsetVal = IRB->CreateAdd(offsetVal, currExprVal);
  }

  auto CurrBase = LLValue;
  CurrBase = IRB->CreateInBoundsGEP(
      CurrBase, offsetVal, arrEle->getName() + (isLHS ? ".write" : ".read"));
  if (isLHS)
    return CurrBase;

  return emitLoadInstruction(CurrBase, arrEle->getName() + ".read");
}

llvm::Value *CodeGen::expandIntrinsic(FunctionReference *funcReference) {
  auto sym = funcReference->getSymbol()->getOrigSymbol();
  auto argList = funcReference->getArgsList();

  auto intrinKind = fc::intrin::getIntrinsicKind(sym->getName());
  switch (intrinKind) {
  case fc::intrin::real: {
    assert(argList.size() == 1);
    auto expr = argList[0];
    llvm::Value *arg = emitExpression(expr);

    if (arg->getType()->isIntegerTy())
      return IRB->CreateSIToFP(arg, IRB->getFloatTy(), "realIntrin");
    else if (arg->getType()->isDoubleTy())
      return IRB->CreateFPCast(arg, IRB->getFloatTy(), "realIntrin");
    llvm_unreachable("Unhanled type in real intrinsic");
  }
  case fc::intrin::sin: {
    assert(argList.size() == 1);
    llvm::Value *args = {emitExpression(argList[0])};
    return IRB->CreateIntrinsic(llvm::Intrinsic::sin, args, nullptr, "sinVal");
  }
  case fc::intrin::cos: {
    assert(argList.size() == 1);
    llvm::Value *args = {emitExpression(argList[0])};
    return IRB->CreateIntrinsic(llvm::Intrinsic::cos, args, nullptr, "cosVal");
  }
  case fc::intrin::exp: {
    assert(argList.size() == 1);
    llvm::Value *args = {emitExpression(argList[0])};
    return IRB->CreateIntrinsic(llvm::Intrinsic::exp, args, nullptr, "expVal");
  }
  case fc::intrin::sqrt: {
    assert(argList.size() == 1);
    llvm::Value *args[] = {emitExpression(argList[0])};
    return IRB->CreateIntrinsic(llvm::Intrinsic::sqrt, args, nullptr,
                                "sqrtVal");
  }
  case fc::intrin::abs: {
    assert(argList.size() == 1);
    llvm::Value *args[] = {emitExpression(argList[0])};
    return IRB->CreateIntrinsic(llvm::Intrinsic::fabs, args, nullptr, "absVal");
  }
  case fc::intrin::log: {
    assert(argList.size() == 1);
    llvm::Value *args[] = {emitExpression(argList[0])};
    return IRB->CreateIntrinsic(llvm::Intrinsic::log, args, nullptr, "logVal");
  }
  case fc::intrin::max: {
    assert(argList.size() == 2);
    llvm::Value *args[] = {emitExpression(argList[0]),
                           emitExpression(argList[1])};

    auto cmp = IRB->CreateFCmpOGT(args[0], args[1]);
    return IRB->CreateSelect(cmp, args[0], args[1]);
  }
  case fc::intrin::min: {
    assert(argList.size() == 2);
    llvm::Value *args[] = {emitExpression(argList[0]),
                           emitExpression(argList[1])};
    return IRB->CreateIntrinsic(llvm::Intrinsic::minnum, args, nullptr,
                                "minVal");
  }
  case fc::intrin::INT: {
    assert(argList.size() == 1);
    return IRB->CreateFPToSI(emitExpression(argList[0]), IRB->getInt32Ty());
  }
  case fc::intrin::mod: {
    assert(argList.size() == 2);
    return IRB->CreateSRem(emitExpression(argList[0]),
                           emitExpression(argList[1]), "mod.val");
  }
  case fc::intrin::lbound: {
    assert(argList.size() == 2);

    if (std == Standard::f77) {
      assert(false);
    }
    llvm::Value *dimension = emitExpression(argList[1]);
    dimension = IRB->CreateSub(dimension, IRB->getInt32(1), "size.sub");

    auto LLValue = emitExpression(argList[0]);
    auto DimArr = IRB->CreateStructGEP(nullptr, LLValue, 1, "dimsArr");
    auto Dim =
        IRB->CreateInBoundsGEP(DimArr, {IRB->getInt32(0), dimension}, "dim");

    auto Size = IRB->CreateStructGEP(nullptr, Dim, 0, "lb");
    // auto lbLoad = emitLoadInstruction(Size, "lb.load");
    auto lbLoad = emitStructLoadInst(Size, Dim, Dim->getType(), 0, "lb.load");
    return IRB->CreateTrunc(lbLoad, IRB->getInt32Ty());
  }
  case fc::intrin::ubound: {
    assert(argList.size() == 2);

    if (std == Standard::f77) {
      assert(false);
    }
    llvm::Value *dimension = emitExpression(argList[1]);
    dimension = IRB->CreateSub(dimension, IRB->getInt32(1), "size.sub");
    auto LLValue = emitExpression(argList[0]);
    auto DimArr = IRB->CreateStructGEP(nullptr, LLValue, 1, "dimsArr");
    auto Dim =
        IRB->CreateInBoundsGEP(DimArr, {IRB->getInt32(0), dimension}, "dim");

    auto Size = IRB->CreateStructGEP(nullptr, Dim, 1, "ub");
    // auto lbLoad = emitLoadInstruction(Size, "ub.load");
    auto ubLoad = emitStructLoadInst(Size, Dim, Dim->getType(), 1, "ub.load");
    return IRB->CreateTrunc(ubLoad, IRB->getInt32Ty());
  }
  case fc::intrin::command_argument_count: {
    assert(argList.size() == 0);
    auto LI = emitLoadInstruction(context.ArgC);
    return IRB->CreateSub(LI, IRB->getInt32(1), "argc");
  }
  case fc::intrin::iachar: {
    assert(argList.size() == 1);
    llvm::Value *charValue = emitExpression(argList[0]);
    return IRB->CreateSExt(charValue, IRB->getInt32Ty());
  }
  case fc::intrin::allocated: {
    assert(argList.size() == 1);
    auto expr = emitExpression(argList[0], true);
    auto isAllocated = IRB->CreateStructGEP(nullptr, expr, 2, "flag");
    return emitLoadInstruction(isAllocated, "isAllocated", true);
  }
  case fc::intrin::present: {
    assert(argList.size() == 1);
    auto expr = emitExpression(argList[0], true);
    return IRB->CreateIsNotNull(expr, "presentIntrin");
  }
  case fc::intrin::associated: {
    assert(argList.size() == 1); // TODO: Handle the second arg
    llvm::Value *expr;

    // FIXME: This is a temporary solution, we need isLHS for emitExpression to
    // be more general by considering whether the expr is in a pointer
    // assignment.
    if (!llvm::isa<StructureComponent>(argList[0]))
      expr = emitExpression(argList[0], /* isLHS */ true);
    else
      expr = emitExpression(argList[0], /* isLHS */ false);

    return IRB->CreateIsNotNull(expr, "associated");
  }
  default:
    return nullptr;
  };
}

Value *CodeGen::emitStaticArrayElement(ArrayElement *arrEle, bool isLHS,
                                       llvm::Value *addr) {
  llvm::Value *LLValue;
  if (!addr)
    LLValue = context.getLLVMValueFor(arrEle->getName());
  else
    LLValue = addr;

  auto fcTy = arrEle->getSymbol()->getOrigType();
  auto fcArrayTy = llvm::dyn_cast<fc::ArrayType>(fcTy);
  assert(fcArrayTy);

  llvm::Type *LLBaseTy = LLVMUtil::getLLVMArrayBaseType(LLValue->getType());
  assert(LLBaseTy);

  auto boundsList = fcArrayTy->getBoundsList();
  auto CurrBase = LLValue;
  llvm::Value *offsetVal = nullptr;
  auto numDims = fcArrayTy->getNumDims();

  // if (!fcArrayTy->isStringCharTy()) {
  //   assert(arrEle->getNumOperands() - 1 == boundsList.size());
  // }
  //
  auto list = arrEle->getSubscriptList();

  bool isString = false;
  if (fcArrayTy->getElementTy()->isStringCharTy()) {
    isString = true;
  }

  if (list.size() != numDims) {
    for (unsigned i = 0; i < list.size(); ++i) {
      auto expr = arrEle->getSubsExpr(i);
      auto exprVal = emitExpression(expr);
      if (isString) {
        exprVal = IRB->CreateSub(exprVal, IRB->getInt32(1));
      }

      exprVal = IRB->CreateSExt(exprVal, IRB->getInt64Ty());

      llvm::Value *size = nullptr;
      // Multiply the expression with other upper dimensions.
      for (unsigned j = boundsList.size() - 1; j > i; j--) {
        auto currSize = emitSizeForArrBounds(boundsList[j]);
        if (!size) {
          size = currSize;
          continue;
        }
        size = IRB->CreateMul(size, currSize);
      }
      offsetVal = IRB->CreateMul(size, exprVal);
    }

  } else {

    // First emit all the subs.
    llvm::SmallVector<llvm::Value *, 2> llSubs, LBs, SizeList;
    for (auto expr : list) {
      auto llExpr = emitExpression(expr);
      assert(llExpr);
      if (isString) {
        llExpr = IRB->CreateSub(llExpr, IRB->getInt32(1));
      }
      llExpr = IRB->CreateSExt(llExpr, IRB->getInt64Ty());
      llSubs.push_back(llExpr);
    }

    // Load the array type.
    // Get lower bound and size list of each dimension.
    for (unsigned I = 0; I < numDims; ++I) {

      auto LB = IRB->getInt64(boundsList[I].first);
      LBs.push_back(LB);

      // We do not need to compute last dimension.
      if (I < numDims - 1) {
        auto UB = IRB->getInt64(boundsList[I].second);
        auto sizeVal = IRB->CreateSub(UB, LB, "sizeMinus1");
        sizeVal = IRB->CreateAdd(sizeVal, IRB->getInt64(1),
                                 "size_of_" + std::to_string(I + 1));
        SizeList.push_back(sizeVal);
      }
    }

    // Get the actual offset for every dimension.
    for (unsigned I = 1; I < SizeList.size(); ++I) {
      SizeList[I] = IRB->CreateMul(SizeList[I - 1], SizeList[I],
                                   "size" + std::to_string(I + 1));
    }

    for (unsigned I = numDims - 1; I >= 0; --I) {
      auto sub = llSubs[I];
      auto LB = LBs[I];
      if (I == 0) {
        if (offsetVal) {
          offsetVal = IRB->CreateSub(offsetVal, LB, "negatelast");
          offsetVal = IRB->CreateAdd(offsetVal, sub, "addsub");
        } else {
          offsetVal = IRB->CreateSub(sub, LB);
        }
        break;
      }

      llvm::Value *currExprVal = nullptr;
      // Multiply the expression with other upper dimension.
      auto currDim = IRB->CreateMul(sub, SizeList[I - 1]);
      auto minusLB = IRB->CreateMul(LB, SizeList[I - 1], "negate");
      currExprVal = IRB->CreateSub(currDim, minusLB);
      if (I == numDims - 1)
        offsetVal = currExprVal;
      else
        offsetVal = IRB->CreateAdd(offsetVal, currExprVal);
    }
  }

  CurrBase = IRB->CreateBitCast(CurrBase, LLBaseTy->getPointerTo());
  CurrBase = IRB->CreateInBoundsGEP(
      CurrBase, offsetVal, arrEle->getName() + (isLHS ? ".write" : ".read"));
  if (isLHS)
    return CurrBase;

  return emitLoadInstruction(CurrBase, arrEle->getName() + ".read");
}

Value *CodeGen::emitArrayElement(ArrayElement *arrEle, bool isLHS,
                                 llvm::Value *addr) {
  auto fcTy = arrEle->getSymbol()->getOrigType();

  fc::ArrayType *fcArrayTy;
  if (auto fcPointerTy = llvm::dyn_cast<fc::PointerType>(fcTy)) {
    fcArrayTy = llvm::dyn_cast<fc::ArrayType>(fcPointerTy->getElementType());
  } else {
    fcArrayTy = llvm::dyn_cast<fc::ArrayType>(fcTy);
  }

  assert(fcArrayTy);
  auto boundsList = fcArrayTy->getBoundsList();

  if (!boundsList.empty()) {
    return emitStaticArrayElement(arrEle, isLHS, addr);
  }
  if (std == Standard::f77) {
    return emitf77DynArrayElement(arrEle, isLHS, addr);
  }

  if ((fcArrayTy->getElementTy()->isCharacterTy() ||
       fcArrayTy->getElementTy()->isStringCharTy()) &&
      arrEle->getNumIndices() < fcArrayTy->getNumDims()) {
    isLHS = true;
  }

  // pointer to arrays are dynamic, they are handled inside
  // emitDynArrayElement()
  return emitDynArrayElement(arrEle, isLHS, addr);
}

llvm::Value *CodeGen::emitStructureComponent(fc::StructureComponent *structComp,
                                             bool isLHS) {
  Symbol *outerStructSym = nullptr;
  ObjectName *outerStructObj = nullptr;
  ArrayElement *outerStructArr = nullptr;

  Expr *firstPartRef = structComp->getPartRefAt(0);

  if ((outerStructObj = llvm::dyn_cast<ObjectName>(firstPartRef))) {
    outerStructSym = outerStructObj->getSymbol()->getOrigSymbol();
  } else if ((outerStructArr = llvm::dyn_cast<ArrayElement>(firstPartRef))) {
    outerStructSym = outerStructArr->getSymbol()->getOrigSymbol();
  } else {
    llvm_unreachable("unhandled first part-ref");
  }

  StructType *outerStructType = llvm::dyn_cast<StructType>(
      Type::getCoreElementType(outerStructSym->getType()));

  assert(outerStructType);

  llvm::Value *llValue = nullptr;
  if (outerStructObj)
    llValue = context.getLLVMValueFor(outerStructSym->getName());
  else if (outerStructArr)
    llValue = emitArrayElement(outerStructArr, /*isLHS*/ true);

  assert(llValue && llValue->getType()->isPointerTy());

  // If the outer-most part-ref is a pointer-type, then dereference it,
  if (llValue->getType()->getPointerElementType()->isPointerTy()) {
    llValue =
        emitLoadInstruction(llValue, llValue->getName().str() + ".loadptr",
                            /* disableTBAA = */ true);
  }

  assert(llValue->getType()->isPointerTy() &&
         llValue->getType()->getPointerElementType()->isStructTy());

  unsigned numPartRefs = structComp->getOperands().size();
  unsigned offset = 0;
  std::string refName = outerStructSym->getName();

  for (unsigned i = 1; i < numPartRefs; ++i) {
    std::string fieldName;
    Expr *partRef = structComp->getPartRefAt(i);

    if (auto field = llvm::dyn_cast<ObjectName>(partRef)) {
      fieldName = field->getName();
    } else if (auto field = llvm::dyn_cast<ArrayElement>(partRef)) {
      fieldName = field->getName();
    } else {
      llvm_unreachable("unhandled part-ref");
    }

    offset = outerStructType->getIndexOf(fieldName);
    refName.append("." + fieldName);

    // Last part-ref won't be "gep'ed into".
    if (i != (numPartRefs - 1)) {
      Type *partRefType = outerStructType->getContainedType(offset);

      ArrayElement *partRefArrayElem = nullptr;
      PointerType *partRefPtrType = nullptr;

      if ((partRefArrayElem = llvm::dyn_cast<ArrayElement>(partRef))) {

        outerStructType =
            llvm::dyn_cast<StructType>(partRefArrayElem->getElementType());

        // It might be a PonterType to StructType
        if (!outerStructType) {
          PointerType *ptrType =
              llvm::dyn_cast<PointerType>(partRefArrayElem->getElementType());
          assert(ptrType);
          outerStructType =
              llvm::dyn_cast<StructType>(ptrType->getElementType());
        }
        assert(outerStructType);

        llValue = IRB->CreateStructGEP(nullptr, llValue, offset, refName);
        // Note that emitArrayElement handles the case if this part-ref is
        // an array-elem from a pointer. Also, we are "GEPing" the array-addr
        // here, so we treat is like an lvalue.
        llValue = emitArrayElement(partRefArrayElem, /*isLHS*/ true, llValue);

        // If not ArrayElement, then the part-ref could only be an ObjectName
        // here.
        // If this is a pointer to a structure, then deref it
      } else if ((partRefPtrType = llvm::dyn_cast<PointerType>(partRefType))) {
        outerStructType =
            llvm::dyn_cast<StructType>(partRefPtrType->getElementType());

        // Pointer as array-elem should be handled in the above if
        assert(outerStructType);

        llValue = IRB->CreateStructGEP(nullptr, llValue, offset, refName);
        llValue = emitLoadInstruction(llValue, refName + ".loadptr",
                                      /* disableTBAA = */ true);

      } else {
        outerStructType = llvm::dyn_cast<StructType>(partRefType);
        assert(outerStructType);
        llValue = IRB->CreateStructGEP(nullptr, llValue, offset, refName);
      }
    }
  }

  Expr *lastPartRef = structComp->getPartRefAt(numPartRefs - 1);
  Value *lastGEP = IRB->CreateStructGEP(nullptr, llValue, offset, refName);

  // ArrayElement as last part-ref handling:
  if (auto fcArr = llvm::dyn_cast<ArrayElement>(lastPartRef)) {
    freezeTBAA = true;
    llvm::Value *arrEle = emitArrayElement(fcArr, isLHS, lastGEP);
    freezeTBAA = false;
    return arrEle;
  }

  // ObjectName as last part-ref handling:
  if (isLHS) {
    return lastGEP;
  }

  auto load = emitStructLoadInst(lastGEP, llValue, llValue->getType(), offset,
                                 refName + ".load");
  return load;
}

Value *CodeGen::emitExpression(Expr *expr, bool isLHS) {
  auto kind = expr->getStmtType();
  switch (kind) {
  case StmtType::ConstantValKind: {
    auto Const = static_cast<ConstantVal *>(expr);
    auto type = Const->getType();
    if (type->isArrayTy()) {
      llvm::SmallVector<llvm::StringRef, 2> constList;
      for (auto &val : Const->getConstant()->getArrValue()) {
        constList.push_back(val);
      }
      return emitConstant(constList, Const->getType());
    }
    return emitConstant(Const->getValueRef(), Const->getType());
  }

  case StmtType::ObjectNameKind: {
    auto objName = static_cast<ObjectName *>(expr);
    auto Alloca = context.getLLVMValueFor(objName->getName());
    assert(Alloca);

    if (isLHS && objName->getSymbol()->getOrigType()->isPointerTy()) {
      return emitLoadInstruction(Alloca, Alloca->getName().str() + ".loadptr",
                                 /* disableTBAA = */ true);
    }

    if (objName->getSymbol()->getOrigType()->isPointerTy()) {
      auto ptrLoad =
          emitLoadInstruction(Alloca, Alloca->getName().str() + ".loadptr",
                              /* disableTBAA = */ true);
      return emitLoadInstruction(ptrLoad, Alloca->getName().str() + ".load",
                                 /* disableTBAA = */ true);
    }

    if (isLHS || objName->getType()->isArrayTy() ||
        objName->getType()->isStringCharTy())
      return Alloca;
    return emitLoadInstruction(Alloca, Alloca->getName().str() + ".load");
  }

  case StmtType::StructureComponentKind: {
    auto structComp = static_cast<StructureComponent *>(expr);
    return emitStructureComponent(structComp, isLHS);
  }

  // Handle array access!
  case StmtType::ArrayElementKind: {
    auto arrEle = cast<ArrayElement>(expr);
    return emitArrayElement(arrEle, isLHS);
  }

  case StmtType::BinaryExprKind: {
    auto binaryExpr = static_cast<BinaryExpr *>(expr);
    auto lhs = binaryExpr->getLHS();
    auto rhs = binaryExpr->getRHS();
    auto OpKind = binaryExpr->getOpKind();

    if (!lhs) { // unary minus (note that unary plus is merely ignored in the
      // parser)
      assert(false);
      auto rhsVal = emitExpression(rhs);
      assert(rhsVal);
      assert(OpKind == BinaryOpKind::Subtraction);
      return IRB->CreateBinOp(Instruction::Sub, IRB->getInt32(0), rhsVal);
    }

    auto lhsVal = emitExpression(lhs);
    auto rhsVal = emitExpression(rhs);
    assert(lhsVal && rhsVal);
    return getLLVMBinaryOp(lhsVal, rhsVal, OpKind);
  }
  case StmtType::RelationalExprKind: {
    auto relExpr = static_cast<RelationalExpr *>(expr);
    auto lhs = relExpr->getLHS();
    auto rhs = relExpr->getRHS();
    auto OpKind = relExpr->getOpKind();

    auto lhsVal = emitExpression(lhs);
    auto rhsVal = emitExpression(rhs);
    assert(lhsVal && rhsVal);

    // String comparision
    if (auto arrSec = llvm::dyn_cast<fc::ArraySection>(lhs)) {
      auto arrayTy = llvm::dyn_cast<fc::ArrayType>(arrSec->getType());
      assert(arrayTy);
      assert(arrayTy->getElementTy()->isCharacterTy() ||
             arrayTy->getElementTy()->isStringCharTy());
      return emitStrCmp(lhsVal, rhsVal, OpKind);
    }
    // For character comparison where RHS is a constant which can be string or
    // char
    // TODO : A sema pass to infer constants as string or character will help
    if (llvm::isa<ArrayElement>(lhs) && llvm::isa<ConstantVal>(rhs) &&
        rhs->getType()->isCharacterTy()) {
      rhsVal =
          IRB->CreateInBoundsGEP(rhsVal, {IRB->getInt32(0), IRB->getInt32(0)});
      rhsVal = emitLoadInstruction(rhsVal, "str.load");
    }

    return getLLVMRelationalOp(lhsVal, rhsVal, OpKind);
  }
  case StmtType::LogicalExprKind: {
    auto logicalExpr = static_cast<LogicalExpr *>(expr);
    auto lhs = logicalExpr->getLHS();
    auto rhs = logicalExpr->getRHS();
    auto OpKind = logicalExpr->getOpKind();

    if (!lhs) { // no lhs for .NOT.
      auto rhsVal = emitExpression(rhs);
      assert(rhsVal);
      return getLLVMLogicalOp(nullptr, rhsVal, OpKind);
    }
    auto lhsVal = emitExpression(lhs);
    auto rhsVal = emitExpression(rhs);
    assert(lhsVal && rhsVal);
    return getLLVMLogicalOp(lhsVal, rhsVal, OpKind);
  }
  case StmtType::CastExprKind: {
    auto castExpr = static_cast<CastExpr *>(expr);
    auto fromExpr = castExpr->getExpr();
    auto fromVal = emitExpression(fromExpr);
    auto toType = cgHelper->getLLVMTypeFor(castExpr->getType());
    auto fromType = fromVal->getType();

    if (fromType == toType)
      return fromVal;

    switch (fromVal->getType()->getTypeID()) {
    case llvm::Type::IntegerTyID: {
      if (toType->isFloatingPointTy()) {
        return IRB->CreateSIToFP(fromVal, toType, "cast");
      }

      if (fromType->isIntegerTy(32) && toType->isIntegerTy(64)) {
        return IRB->CreateSExt(fromVal, toType, "case");
      }

      if (fromType == toType)
        return fromVal;

      llvm_unreachable("unknown integer cast type in llvm");
    }
    case llvm::Type::FloatTyID: {
      if (toType->isIntegerTy()) {
        return IRB->CreateFPToSI(fromVal, toType, "cast");
      }
      if (toType->isDoubleTy()) {
        return IRB->CreateFPExt(fromVal, toType, "fpext");
      }
      llvm_unreachable("unknown float cast type in llvm");
    }
    case llvm::Type::DoubleTyID: {
      if (toType->isIntegerTy()) {
        return IRB->CreateFPToSI(fromVal, toType, "cast");
      }
      if (toType->isFloatTy()) {
        return IRB->CreateFPTrunc(fromVal, toType, "fptrunc");
      }
      if (toType->isDoubleTy()) {
        return fromVal;
      }
      llvm_unreachable("unknown double cast type in llvm");
    }
    default:
      llvm_unreachable("unknown cast type in llvm");
      break;
    }
    break;
  }
  case StmtType::FunctionReferenceKind: {

    auto funcReference = static_cast<FunctionReference *>(expr);
    if (auto val = expandIntrinsic(funcReference)) {
      return val;
    }
    auto sym = funcReference->getSymbol()->getOrigSymbol();
    auto argList = funcReference->getArgsList();
    auto Call = emitCall(sym, argList);
    if (intrin::isIntrinsic(sym->getName()))
      return Call;

    // auto funcRefName = cgHelper->getEmittedNameForPU(sym->getName());
    // auto GV = cgHelper->getReturnValueFor(funcRefName);
    return Call;
  }

  case StmtType::ArraySectionKind: {
    auto arrSec = static_cast<ArraySection *>(expr);
    assert(arrSec->isFullRange());
    auto sym = arrSec->getSymbol()->getOrigSymbol();
    auto Alloca = context.getLLVMValueFor(sym->getName());

    if (sym->getType()->isPointerTy()) {
      return emitLoadInstruction(Alloca, Alloca->getName().str() + ".loadptr",
                                 /* disableTBAA = */ true);
    }

    if (isLHS || sym->getType()->isArrayTy() ||
        sym->getType()->isStringCharTy())
      return Alloca;
    return emitLoadInstruction(Alloca, Alloca->getName().str() + ".load");
  }
  default:
    llvm_unreachable("Unkown base expression type.");
  };
  return nullptr;
}

llvm::Value *CodeGen::getArrDimSizeVal(Expr *expr, llvm::Value *exprVal) {
  auto arrEle = dyn_cast<ArraySection>(expr);
  if (!arrEle)
    return nullptr;

  Type *arrEleType = arrEle->getType();

  if (auto arrElePtrTy = llvm::dyn_cast<fc::PointerType>(arrEleType)) {
    arrEleType = arrElePtrTy->getElementType();
  }

  if (!arrEleType->isArrayTy())
    return nullptr;
  auto arrType = static_cast<ArrayType *>(arrEleType);

  // If this is a static array.
  if (!arrType->boundsEmpty()) {
    auto boundsList = arrType->getBoundsList();

    llvm::Value *sizeVal = nullptr;
    for (unsigned i = 0; i < boundsList.size(); ++i) {
      auto currSizeVal = emitSizeForArrBounds(boundsList[i]);
      if (!sizeVal) {
        sizeVal = currSizeVal;
        continue;
      }
      sizeVal = IRB->CreateMul(sizeVal, currSizeVal);
    }
    return sizeVal;
  }

  // If the array type is dynamic, get the value and compute the bounds.
  // Now store the dims.
  assert(exprVal->getType()->isPointerTy() &&
         exprVal->getType()->getPointerElementType()->isStructTy());
  auto name = exprVal->getName();
  auto dimsArr = IRB->CreateStructGEP(nullptr, exprVal, 1, name + ".dimsArr");
  llvm::Value *sizeVal = nullptr;
  for (int I = 0; I < arrType->getNumDims(); ++I) {
    auto Dim = IRB->CreateInBoundsGEP(
        dimsArr, {IRB->getInt32(0), IRB->getInt32(I)}, name + ".dim");

    auto Size = IRB->CreateStructGEP(nullptr, Dim, 2, name + ".size");
    // auto Load = emitLoadInstruction(Size, name + ".load");
    auto Load =
        emitStructLoadInst(Size, Dim, Dim->getType(), 2, name + ".size");

    if (I == 0) {
      sizeVal = Load;
      continue;
    }
    sizeVal = IRB->CreateMul(sizeVal, Load);
  }
  return sizeVal;
}
