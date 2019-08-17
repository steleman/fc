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
#include "AST/ASTContext.h"
#include "AST/ASTPass.h"

#include "AST/ParseTreeBuilder.h"
#include "AST/ProgramUnit.h"
#include "AST/StmtVisitor.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"
#include "sema/ExpansionUtils.h"
#include "sema/Intrinsics.h"

using namespace fc;
using namespace ast;
namespace fc {

static unsigned getSizeForType(Type *Ty) {
  switch (Ty->getTypeID()) {
  case Type::RealID:
  case Type::Int32ID:
    return 4;
  default:
    llvm_unreachable("Not handled");
  }
  return 0;
}

class IntrinsicHandler : public StmtVisitor<IntrinsicHandler, bool> {
  ParseTreeBuilder builder;
  Block *currBlock{nullptr};
  SymbolTable *currSymTable{nullptr};
  Stmt *currStmt{nullptr};
  ASTContext &C;

  static long int getMinValueForType(Type *ty) {
    switch (ty->getTypeID()) {
    case Type::Int32ID:
      return -2147483648l;
    default:
      llvm_unreachable("Unhandled min value requested");
    }
  }

  static long int getMaxValueForType(Type *ty) {
    switch (ty->getTypeID()) {
    case Type::Int32ID:
      return 2147483647l;
    default:
      llvm_unreachable("Unhandled max value requested");
    }
  }

public:
  void setBlock(Block *block) { currBlock = block; }

  void setStmt(Stmt *stmt) { currStmt = stmt; }

  void setSymbolTable(SymbolTable *symTable) { currSymTable = symTable; }

  IntrinsicHandler(ASTContext &C) : builder(C), C(C) {}

  bool expandAdjustl(FunctionReference *ref) {
    // Emit a dowhile loop to find number of spaces at the beginning of string
    // Add so many spaces in the result array in same loop
    // Emit do loop to copy remaining elements

    assert(ref->getNumOperands() == 1);
    auto loc = ref->getSourceLoc();
    auto arrSec = llvm::dyn_cast<ArraySection>(ref->getOperand(0));
    assert(arrSec);
    if (!arrSec->isFullRange())
      return true;

    auto sym = arrSec->getSymbol();

    StmtVecList stmtList;
    ArraySectionHelper arrSecHelper(builder, currSymTable, C);

    // Temp to store the result
    auto resultSym =
        arrSecHelper.getTempArray(arrSec, nullptr, stmtList, -1, false);
    auto resultObj = builder.buildObjectName(resultSym, loc);

    // Build required constant values
    auto I32 = Type::getInt32Ty(C);
    auto one = builder.getConstantOne(loc, I32);
    auto zero = builder.buildConstantVal(std::to_string(0), I32, loc);

    // Temporary values to hold number of spaces, index and reverse index
    auto spaces = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
    auto index = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
    auto reverseIndex = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);

    auto spaceObj = builder.buildObjectName(spaces, loc);
    auto indexObj = builder.buildObjectName(index, loc);
    auto reverseIndexObj = builder.buildObjectName(reverseIndex, loc);

    // Initialise all the values
    auto spaceInit =
        builder.buildAssignmentStmt(spaceObj->clone(), zero->clone(), loc);
    auto reverseIndexInit = builder.buildAssignmentStmt(
        reverseIndexObj, builder.buildUBoundIntrin(resultSym, 1, loc), loc);

    auto indexInit =
        builder.buildAssignmentStmt(indexObj->clone(), one->clone(), loc);

    stmtList.push_back(reverseIndexInit);
    stmtList.push_back(spaceInit);
    stmtList.push_back(indexInit);

    // Updating the variables
    auto spaceIncr = builder.buildAddExpr(spaceObj->clone(), one->clone());
    auto spaceIncrAssign =
        builder.buildAssignmentStmt(spaceObj->clone(), spaceIncr, loc);
    auto indexIncr = builder.buildAddExpr(indexObj->clone(), one->clone());
    auto reverseIndexDecr =
        builder.buildExpr(reverseIndexObj->clone(), one->clone(),
                          BinaryOpKind::Subtraction, I32, loc);

    auto indexIncrAssign =
        builder.buildAssignmentStmt(indexObj->clone(), indexIncr, loc);
    auto reverseIndexDecrAssign = builder.buildAssignmentStmt(
        reverseIndexObj->clone(), reverseIndexDecr, loc);

    // Construct assignment of array element result[reverseindex] =
    // arrsec[index]
    ExprList subsList1;
    subsList1.push_back(reverseIndexObj->clone());
    auto resultEle1 = builder.buildArrayElement(resultSym, resultObj->clone(),
                                                subsList1, loc);

    auto arrayBase = builder.buildObjectName(sym, loc);
    ExprList subsList2{indexObj->clone()};
    auto arrayEle = builder.buildArrayElement(sym, arrayBase, subsList2, loc);

    auto eleAssignment1 =
        builder.buildAssignmentStmt(resultEle1, arrayEle, loc);

    // Push all the constructed intructions to dowhile block
    StmtList doStmtList;
    doStmtList.push_back(eleAssignment1);
    doStmtList.push_back(indexIncrAssign);
    doStmtList.push_back(reverseIndexDecrAssign);
    doStmtList.push_back(spaceIncrAssign);

    // Create DoWhile stmt
    auto spaceConstant =
        builder.buildConstantVal(" ", Type::getCharacterTy(C), loc);
    auto neSpace = builder.buildRelationalExpr(arrayEle, spaceConstant->clone(),
                                               RelationalOpKind::EQ, loc);
    auto doBlock = builder.buildBlock(doStmtList, loc);

    auto doWhileLoop = builder.buildDoWhileStmt(neSpace, doBlock, loc);
    stmtList.push_back(doWhileLoop);

    // Build the DoStmt for copying all the other elements
    auto index2 = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
    auto indexObj2 = builder.buildObjectName(index2, loc);

    ExprList subsList3;
    subsList3.push_back(indexObj2->clone());
    auto resultEle2 = builder.buildArrayElement(resultSym, resultObj->clone(),
                                                subsList3, loc);

    Expr *ubound = builder.buildUBoundIntrin(sym, 1, loc);
    ubound = builder.buildExpr(ubound, spaceObj->clone(),
                               BinaryOpKind::Subtraction, I32, loc);

    Expr *ops[4] = {indexObj2, one, ubound, one->clone()};

    StmtList forBlockList2;

    // We can re-use old arrayelement object as index variable remains same
    auto eleAssignment2 = builder.buildAssignmentStmt(resultEle2->clone(),
                                                      arrayEle->clone(), loc);
    auto indexIncr2 = builder.buildAddExpr(indexObj->clone(), one);
    auto indexIncrAssign2 =
        builder.buildAssignmentStmt(indexObj->clone(), indexIncr2, loc);

    forBlockList2.push_back(eleAssignment2);
    forBlockList2.push_back(indexIncrAssign2);
    Block *forBlock = builder.buildBlock(forBlockList2, loc);
    auto quadExpr = builder.buildQuadExpr(ops, index2->getType(), loc);
    auto doStmt = builder.buildDoStmt(quadExpr, forBlock, "", loc);

    stmtList.push_back(doStmt);
    for (auto stmt : stmtList) {
      currBlock->insertStmtBefore(stmt, currStmt);
    }

    auto resultArrSec =
        builder.buildArraySection(resultSym, resultObj->clone(), loc);

    ref->replaceWith(resultArrSec);

    return true;
  }

  bool expandSize(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();

    auto arrSec = llvm::dyn_cast<ArraySection>(ref->getOperand(0));
    assert(arrSec);
    auto arrayTy = llvm::dyn_cast<ArrayType>(arrSec->getType());
    auto sym = arrSec->getSymbol();
    assert(arrayTy);

    if (ref->getNumOperands() == 1) {
      auto numDims = arrayTy->getNumDims();
      Expr *size = nullptr;
      for (unsigned i = 1; i <= numDims; ++i) {
        auto lbound = builder.buildLBoundIntrin(sym, i, loc);
        auto ubound = builder.buildUBoundIntrin(sym, i, loc);
        auto currSize = builder.buildExpr(
            ubound, lbound, BinaryOpKind::Subtraction, ubound->getType(), loc);
        currSize = builder.buildAddExpr(
            currSize, builder.getConstantOne(loc, Type::getInt32Ty(C)));

        if (!size) {
          size = currSize;
          continue;
        }

        size = builder.buildExpr(currSize, size, BinaryOpKind::Multiplication,
                                 size->getType(), loc);
      }

      ref->replaceWith(size);
      return true;
    }

    assert(ref->getNumOperands() == 2);
    auto dimConstant = llvm::dyn_cast<ConstantVal>(ref->getOperand(1));
    assert(dimConstant);
    auto dim = dimConstant->getInt();
    auto lbound = builder.buildLBoundIntrin(sym, dim, loc);
    auto ubound = builder.buildUBoundIntrin(sym, dim, loc);
    auto currSize = builder.buildExpr(ubound, lbound, BinaryOpKind::Subtraction,
                                      ubound->getType(), loc);
    Expr *size = builder.buildAddExpr(
        currSize, builder.getConstantOne(loc, Type::getInt32Ty(C)));
    ref->replaceWith(size);
    return true;
  }

  bool expandRepeat(FunctionReference *ref) {
    // Create 1D array of same length
    // assign each element with repeat char
    // Replace ref with temp array. Alternatively we can emit memeset

    auto loc = ref->getSourceLoc();
    assert(ref->getNumOperands() == 2);
    auto repeatChar = static_cast<Expr *>(ref->getOperand(0));

    assert(repeatChar->getType()->isCharacterTy());

    auto len = static_cast<Expr *>(ref->getOperand(1));
    auto one = builder.getConstantOne(loc, Type::getInt32Ty(C));
    DynArrBoundsList bounds;
    bounds.push_back(std::make_pair(one, len));

    auto indVar = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);

    auto indVarObj = builder.buildObjectName(indVar, loc);

    auto shapeSpec = builder.buildArraySpec(bounds, 1, loc);

    StmtVecList stmtList;
    ArraySectionHelper arrSecHelper(builder, currSymTable, C);

    auto resultSym = arrSecHelper.getTempArray(
        shapeSpec, 1, Type::getCharacterTy(C), stmtList);

    for (auto stmt : stmtList) {
      currBlock->insertStmtBefore(stmt, currStmt);
    }

    auto resultObj = builder.buildObjectName(resultSym, loc);
    ExprList indexList;
    indexList.push_back(indVarObj->clone());

    auto resultEle = builder.buildArrayElement(resultSym, resultObj->clone(),
                                               indexList, loc);

    auto assignStmt =
        builder.buildAssignmentStmt(resultEle, repeatChar->clone(), loc);

    StmtList blockStmtList{assignStmt};
    Block *block = builder.buildBlock(blockStmtList, loc);

    Expr *ops[4] = {indVarObj, one, len->clone(), one->clone()};
    auto quadExpr = builder.buildQuadExpr(ops, indVar->getType(), loc);
    auto doStmt = builder.buildDoStmt(quadExpr, block, "", loc);
    currBlock->insertStmtBefore(doStmt, currStmt);

    auto resultArrSec =
        builder.buildArraySection(resultSym, resultObj->clone(), loc);

    ref->replaceWith(resultArrSec);
    return true;
  }

  bool expandIndex(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    assert(ref->getNumOperands() == 2);

    auto arrSec = llvm::dyn_cast<ArraySection>(ref->getOperand(0));

    if (!arrSec)
      return true;

    ArrayType *type = llvm::dyn_cast<ArrayType>(arrSec->getType());
    assert(type);
    assert(type->getNumDims() == 1);

    auto constVal = llvm::dyn_cast<ConstantVal>(ref->getOperand(1));

    if (!constVal)
      return true;

    std::string str = constVal->getValue();
    assert(str.size() == 1);
    // auto asciiChar = (int) str[0];
    // auto asciiNull = 0;
    auto result = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
    auto index = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
    assert(result);

    auto charConstant =
        builder.buildConstantVal(str, Type::getCharacterTy(C), loc);
    auto nullConstant =
        builder.buildConstantVal("\0", Type::getCharacterTy(C), loc);

    auto negOneConstant =
        builder.buildConstantVal(std::to_string(-1), Type::getInt32Ty(C), loc);

    auto oneConstant =
        builder.buildConstantVal(std::to_string(1), Type::getInt32Ty(C), loc);
    auto resultObject = builder.buildObjectName(result, loc);
    auto indexObject = builder.buildObjectName(index, loc);

    auto resultInit =
        builder.buildAssignmentStmt(resultObject, negOneConstant, loc);

    auto indexInit = builder.buildAssignmentStmt(indexObject, oneConstant, loc);

    currBlock->insertStmtBefore(resultInit, currStmt);
    currBlock->insertStmtBefore(indexInit, currStmt);

    auto arrayBase = builder.buildObjectName(arrSec->getSymbol(), loc);

    ExprList subsList;
    subsList.push_back(indexObject->clone());
    auto arrayElement = builder.buildArrayElement(arrSec->getSymbol(),
                                                  arrayBase, subsList, loc);

    auto relExpr1 = builder.buildRelationalExpr(arrayElement, nullConstant,
                                                RelationalOpKind::NE, loc);

    auto relExpr2 = builder.buildRelationalExpr(
        arrayElement->clone(), charConstant, RelationalOpKind::NE, loc);

    auto finalCond =
        builder.buildLogicalExpr(relExpr1, relExpr2, LogicalOpKind::AND, loc);
    StmtList stmtList;
    auto indexIncrement =
        builder.buildAddExpr(oneConstant->clone(), indexObject->clone());

    auto indexIndcr =
        builder.buildAssignmentStmt(indexObject->clone(), indexIncrement, loc);

    stmtList.push_back(indexIndcr);
    auto doBlock = builder.buildBlock(stmtList, loc);

    auto doWhileLoop = builder.buildDoWhileStmt(finalCond, doBlock, loc);

    currBlock->insertStmtBefore(doWhileLoop, currStmt);

    StmtList ifStmtList;
    auto finalResult = builder.buildAssignmentStmt(resultObject->clone(),
                                                   indexObject->clone(), loc);

    ifStmtList.push_back(finalResult);
    auto ifBlock = builder.buildBlock(ifStmtList, loc);

    auto maskExpr = builder.buildRelationalExpr(
        arrayElement->clone(), charConstant, RelationalOpKind::EQ, loc);
    auto ifStmt = builder.buildIfStmt(maskExpr, ifBlock, loc);

    IfStmtList ifList{ifStmt};
    IfConstructKindList kindList{IfConstructKind::IfKind};
    auto ifElseStmt = builder.buildIfElseStmt(ifList, kindList, loc);
    currBlock->insertStmtBefore(ifElseStmt, currStmt);

    ref->replaceWith(resultObject->clone());
    return true;
  }

  bool expandArraySecReshape(FunctionReference *ref) {
    auto arrSec = llvm::cast<ArraySection>(ref->getOperand(0));
    auto loc = arrSec->getSourceLoc();
    auto shape = llvm::cast<ArraySection>(ref->getOperand(1));

    auto shapeSym = shape->getSymbol();
    auto shapeTy = llvm::dyn_cast<ArrayType>(shapeSym->getOrigType());
    assert(shapeTy->getNumDims() == 1);

    auto assignStmt = llvm::dyn_cast<AssignmentStmt>(currStmt);
    assert(assignStmt);

    auto lhsArr = llvm::cast<ArraySection>(assignStmt->getLHS());
    auto lhsSym = lhsArr->getSymbol()->getOrigSymbol();
    assert(lhsArr->isFullRange());

    ArraySectionHelper arrSecHelper(builder, currSymTable, C);
    ExprRangeInfoMap map;
    bool hasRange = false;
    StmtVecList topLevelStmtList;
    arrSecHelper.collectRangeInfoListFor(arrSec, map, hasRange);
    arrSecHelper.collectRangeInfoListFor(lhsArr, map, hasRange);

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrSecHelper.getLoopNestFor(map[arrSec].list, doStmtList);

    // Initialize the lhs array index value to lbound value.
    auto lhsIndvar = map[lhsArr].list[0].indvar;
    auto lbound = builder.buildLBoundIntrin(lhsSym, 1, loc);
    topLevelStmtList.push_back(
        builder.buildAssignmentStmt(lhsIndvar, lbound, loc));

    // Now insert the top-level do loop to top level stmt list.
    topLevelStmtList.push_back(doStmtList.back());

    // Now insert the assignment statement in the innermost loop.
    // and change all the expression values to scalar.
    auto innerBlock = doStmtList[0]->getBlock();

    // Build the assignment for copying here.
    auto assign = builder.buildAssignmentStmt(map[lhsArr].finalExpr,
                                              map[arrSec].finalExpr, loc);
    innerBlock->insertFront(assign);

    // Now increment the lhs indvar.
    auto one = builder.getConstantOne(loc, lhsIndvar->getType());
    auto update = builder.buildAddExpr(lhsIndvar->clone(), one);
    auto incr = builder.buildAssignmentStmt(lhsIndvar->clone(), update, loc);
    innerBlock->addStmt(incr);

    for (auto newStmt : topLevelStmtList) {
      currBlock->insertStmtBefore(newStmt, assignStmt);
    }
    currBlock->removeStmt(assignStmt);
    return true;
  }

  bool expandReshape(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    assert(ref->getNumOperands() == 2);

    auto expr = ref->getOperand(0);

    auto inArray = llvm::dyn_cast<ArraySection>(expr);
    assert(inArray);
    auto inTy = llvm::dyn_cast<ArrayType>(inArray->getSymbol()->getOrigType());
    assert(inTy);
    auto elementTy = inTy->getElementTy();

    auto shape = llvm::dyn_cast<ArraySection>(ref->getOperand(1));
    assert(shape);
    auto shapeSym = shape->getSymbol();
    auto shapeTy = llvm::dyn_cast<ArrayType>(shapeSym->getOrigType());
    assert(shapeTy);

    assert(!shapeTy->isDynArrayTy());
    auto bounds = shapeTy->getBoundsList();
    assert(bounds.size() == 1);

    if (!inArray->isFullRange()) {
      return expandArraySecReshape(ref);
    }

    unsigned numDims = bounds[0].second;

    DynArrBoundsList boundsList;
    Type *intTy = Type::getInt32Ty(C);
    ExprList sizes;

    for (unsigned i = 0; i < numDims; ++i) {
      auto lower = builder.getConstantOne(loc, intTy);
      auto sub = builder.buildConstantVal(std::to_string(i + 1), intTy, loc);
      ExprList subscrList;
      subscrList.push_back(sub);
      auto base = builder.buildObjectName(shapeSym, loc);
      auto upper = builder.buildArrayElement(shapeSym, base, subscrList, loc);

      sizes.push_back(upper);
      boundsList.push_back(std::make_pair(lower, upper));
    }

    Expr *numElements = nullptr;
    for (auto size : sizes) {
      if (!numElements) {
        numElements = size;
        continue;
      }

      numElements = builder.buildExpr(numElements, size,
                                      BinaryOpKind::Multiplication, intTy, loc);
    }

    unsigned elementSize = getSizeForType(elementTy);
    auto elementVal =
        builder.buildConstantVal(std::to_string(elementSize), intTy, loc);

    numElements = builder.buildExpr(numElements, elementVal,
                                    BinaryOpKind::Multiplication, intTy, loc);

    auto shapeSpec = builder.buildArraySpec(boundsList, numDims, loc);

    StmtVecList newStmtList;
    ArraySectionHelper arrSecHelper(builder, currSymTable, C);
    auto tempSym =
        arrSecHelper.getTempArray(shapeSpec, numDims, elementTy, newStmtList);
    auto tempObj = builder.buildObjectName(tempSym, loc);
    auto tempArrSec = builder.buildArraySection(tempSym, tempObj, loc);

    auto memFunc = currSymTable->getOrInsertSymbol(
        "memcpy", FunctionType::getUndeclaredFuncTy(C), loc);

    ExprList argList{tempArrSec, inArray, numElements};
    auto callStmt = builder.buildCallStmt(memFunc, argList, loc);

    ExprList printList{tempArrSec->clone()};

    newStmtList.push_back(callStmt);

    for (auto newStmt : newStmtList) {
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    ref->replaceWith(tempArrSec->clone());
    return true;
  }

  bool expandCshift(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    assert(ref->getNumOperands() == 2);

    auto expr = ref->getOperand(0);

    auto arrSec = llvm::dyn_cast<ArraySection>(expr);
    if (!arrSec) {
      FC_DEBUG(debug() << "Not expanding array section.");
      return true;
    }

    // Default dimVal.
    int dimVal = 1;

    Expr *shiftVal = ref->getOperand(1);
    assert(shiftVal);

    auto origSym = arrSec->getSymbol()->getOrigSymbol();
    auto arrType = llvm::cast<ArrayType>(origSym->getType());

    ArraySectionHelper arrSecHelper(builder, currSymTable, C);
    StmtVecList newStmtList;

    auto baseEleType = arrType->getElementTy();
    auto tempArr = arrSecHelper.getTempArray(arrSec, baseEleType, newStmtList);
    assert(tempArr);
    auto tempArrSec = builder.buildArraySection(
        tempArr, builder.buildObjectName(tempArr, loc), loc);

    // Now do the conversion.
    ExprRangeInfoMap map;
    bool hasRange = false;
    arrSecHelper.collectRangeInfoListFor(expr, map, hasRange);
    arrSecHelper.collectRangeInfoListFor(tempArrSec, map, hasRange);

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrSecHelper.getLoopNestFor(map[arrSec].list, doStmtList);
    auto innerBlock = doStmtList[0]->getBlock();

    arrSecHelper.insertOtherRangeInfo(map, arrSec, doStmtList, newStmtList);

    // Now do the scalar conversion.
    auto arrEle = llvm::cast<ArrayElement>(map[arrSec].finalExpr);

    auto tempArrEle = llvm::cast<ArrayElement>(map[tempArrSec].finalExpr);
    assert(tempArrEle);

    // Now cshift the array for the given dimension.
    auto origSubsList = arrEle->getSubscriptList();
    auto *dim = origSubsList[dimVal - 1];

    // Change the subscript list  for dimVal dimension in the temporary array to
    // temp = mod(dim -lbound() - shiftVal,size(originalArr,dim=dimVal))
    // if (temp < 0) temp = ubound + 1+ res
    // else temp = lbound + res

    // 1.  temp = mod(dim -lbound() - shiftVal,size(originalArr,dim=dimVal))
    auto sum = builder.buildExpr(dim, shiftVal, BinaryOpKind::Subtraction,
                                 dim->getType(), loc);
    auto lbound = builder.buildLBoundIntrin(origSym, dimVal, loc);
    auto modLBSum = builder.buildExpr(sum, lbound, BinaryOpKind::Subtraction,
                                      sum->getType(), loc);

    auto funcRef = builder.buildSizeIntrin(origSym, dimVal, loc);
    auto modRef =
        builder.buildModIntrin(origSym, modLBSum, funcRef->clone(), loc);

    auto tempSym = currSymTable->getTempSymbol(sum->getType(), loc);
    auto tempObj = builder.buildObjectName(tempSym, loc);

    auto tempInitVal =
        builder.buildAssignmentStmt(tempObj->clone(), modRef, loc);
    innerBlock->insertFront(tempInitVal);

    // 2.     // if (temp < 0) temp = ubound + 1 +  temp
    auto zero = builder.buildConstantVal("0", sum->getType(), loc);
    auto logicalExpr = builder.buildRelationalExpr(tempObj->clone(), zero,
                                                   RelationalOpKind::LT, loc);

    StmtList ifStmtList;
    auto one = builder.getConstantOne(loc, sum->getType());
    auto ubound = builder.buildUBoundIntrin(origSym, dimVal, loc);
    auto ifSum1 = builder.buildAddExpr(one, ubound);
    ifSum1 = builder.buildAddExpr(tempObj->clone(), ifSum1);

    ifStmtList.push_back(
        builder.buildAssignmentStmt(tempObj->clone(), ifSum1, loc));
    auto ifBlock = builder.buildBlock(ifStmtList, loc);
    auto ifStmt = builder.buildIfStmt(logicalExpr, ifBlock, loc);

    // else temp = lbound + res
    StmtList elseStmtList;
    auto elseSum = builder.buildAddExpr(tempObj->clone(), lbound->clone());
    elseStmtList.push_back(
        builder.buildAssignmentStmt(tempObj->clone(), elseSum, loc));
    auto elseBlock = builder.buildBlock(elseStmtList, loc);
    auto elseStmt = builder.buildIfStmt(nullptr, elseBlock, loc);

    IfStmtList ifList{ifStmt, elseStmt};
    IfConstructKindList kindList{IfKind, ElseKind};

    auto ifElseStmt = builder.buildIfElseStmt(ifList, kindList, loc);
    innerBlock->insertStmtAfter(ifElseStmt, tempInitVal);

    auto tempSubsList = tempArrEle->getSubscriptList();
    tempSubsList[dimVal - 1]->replaceWith(tempObj->clone());

    auto asssignStmt = builder.buildAssignmentStmt(tempArrEle, arrEle, loc);
    innerBlock->insertStmtAfter(asssignStmt, ifElseStmt);

    newStmtList.push_back(doStmtList.back());

    for (auto newStmt : newStmtList) {
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    // now replace the function reference with the arraysecion of temp array.
    ref->replaceWith(tempArrSec->clone());
    return true;
  }

  bool expandInt(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    assert(ref->getNumOperands() == 1);

    auto expr = ref->getOperand(0);

    auto arrSec = llvm::dyn_cast<ArraySection>(expr);
    if (!arrSec || !arrSec->isFullRange()) {
      FC_DEBUG(debug() << "Not expanding array section/ array element in INT.");
      return true;
    }
    auto origSym = arrSec->getSymbol()->getOrigSymbol();
    auto arrType = llvm::cast<ArrayType>(origSym->getType());
    assert(arrType->getElementTy()->isRealTy());

    ArraySectionHelper arrSecHelper(builder, currSymTable, C);
    StmtVecList newStmtList;

    auto baseEleType = Type::getInt32Ty(C);
    auto tempArr = arrSecHelper.getTempArray(arrSec, baseEleType, newStmtList);
    assert(tempArr);
    auto tempArrSec = builder.buildArraySection(
        tempArr, builder.buildObjectName(tempArr, loc), loc);

    // Now do the conversion.
    ExprRangeInfoMap map;
    bool hasRange = false;
    arrSecHelper.collectRangeInfoListFor(expr, map, hasRange);
    arrSecHelper.collectRangeInfoListFor(tempArrSec, map, hasRange);

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrSecHelper.getLoopNestFor(map[arrSec].list, doStmtList);
    auto innerBlock = doStmtList[0]->getBlock();

    arrSecHelper.insertOtherRangeInfo(map, arrSec, doStmtList, newStmtList);

    // Now do the scalar conversion.
    auto arrEle = llvm::cast<ArrayElement>(map[arrSec].finalExpr);
    auto tempArrEle = llvm::cast<ArrayElement>(map[tempArrSec].finalExpr);
    assert(tempArrEle);

    ExprList argList{arrEle};
    TypeList list{arrType->getElementTy()};
    auto *intType = FunctionType::get(C, baseEleType, list);
    auto funcRef =
        builder.buildFunctionReference(ref->getSymbol(), argList, intType, loc);

    auto asssignStmt = builder.buildAssignmentStmt(tempArrEle, funcRef, loc);
    innerBlock->insertFront(asssignStmt);

    newStmtList.push_back(doStmtList.back());

    for (auto newStmt : newStmtList) {
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    // now replace the function reference with the arraysecion of temp array.
    ref->replaceWith(tempArrSec->clone());
    return true;
  }

  bool expandSum(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    if (ref->getNumOperands() > 3) {
      return true;
    }
    auto expr = ref->getOperand(0);

    int dimVal = -1;

    if (ref->getNumOperands() > 1) {
      auto assignExpr = llvm::cast<AssignmentExpr>(ref->getOperand(1));
      assert(assignExpr->getName() == "dim");
      auto dimExpr = llvm::cast<ConstantVal>(assignExpr->getExpr());
      dimVal = dimExpr->getInt();
    }

    Expr *maskExpr = nullptr;
    if (ref->getNumOperands() > 2) {
      auto assignExpr = llvm::cast<AssignmentExpr>(ref->getOperand(2));
      assert(assignExpr->getName() == "mask");
      maskExpr = assignExpr->getExpr();
    }

    ArraySectionHelper arrySecHelper(builder, currSymTable, C);
    ExprRangeInfoMap map;
    bool hasRange = false;
    arrySecHelper.collectRangeInfoListFor(expr, map, hasRange);

    RangeInfoList *referenceInfoList = nullptr;
    Expr *referenceExpr = nullptr;
    for (auto &val : map) {
      for (auto rangeInfo : val.second.list) {
        if (rangeInfo.isRange()) {
          if (!llvm::isa<ArraySection>(val.first))
            continue;
          referenceInfoList = &val.second.list;
          referenceExpr = val.first;
        }
      }
    }

    StmtVecList topLevelStmtList;

    // Build a variable to count the true values.
    Symbol *countTemp = nullptr;
    Expr *countExpr = nullptr;
    if (dimVal == -1) {
      countTemp = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
    } else {
      countTemp =
          arrySecHelper.getTempArray(static_cast<ArraySection *>(referenceExpr),
                                     nullptr, topLevelStmtList, dimVal);
      auto arrSec = builder.buildArraySection(
          countTemp, builder.buildObjectName(countTemp, loc), loc);
      countExpr = arrSec;
      arrySecHelper.collectRangeInfoListFor(arrSec, map, hasRange);
    }

    if (maskExpr != nullptr) {
      arrySecHelper.collectRangeInfoListFor(maskExpr, map, hasRange);
    }

    if (!referenceInfoList) {
      assert(false && "handle scalar count");
    }

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrySecHelper.getLoopNestFor(*referenceInfoList, doStmtList);

    // Top level stmt list to add in place of current statement.
    arrySecHelper.insertOtherRangeInfo(map, referenceExpr, doStmtList,
                                       topLevelStmtList, dimVal, countTemp);

    // Now insert the top-level do loop to top level stmt list.
    topLevelStmtList.push_back(doStmtList.back());

    // Now insert the assignment statement in the innermost loop.
    // and change all the expression values to scalar.
    auto innerBlock = doStmtList[0]->getBlock();

    // Now handle the count part.

    // 2. Build the if statement for storing the count.
    // COnver the expr to scalar one.
    // Replace all the expressions in statements with the scalar ones.
    for (auto &val : map) {
      auto expr = val.first;
      auto finalExpr = val.second.finalExpr;
      if (expr == countExpr) {
        continue;
      }
      expr->replaceWith(finalExpr);
    }

    Expr *countTempVal = nullptr;
    countTempVal = builder.buildObjectName(countTemp, loc);
    // Init tempVal to zero.
    if (dimVal != -1) {
      countTempVal = builder.buildArraySection(countTemp, countTempVal, loc);
    }
    auto initVal = builder.buildAssignmentStmt(
        countTempVal, builder.buildConstantVal("0", Type::getInt32Ty(C), loc),
        loc);
    topLevelStmtList.insert(topLevelStmtList.begin(), initVal);

    // Now, increment the countTemp if the statement is true.

    if (dimVal != -1) {
      countTempVal = map[countExpr].finalExpr;
    }
    auto incr = builder.buildAddExpr(countTempVal->clone(),
                                     map[referenceExpr].finalExpr);
    auto assignStmt =
        builder.buildAssignmentStmt(countTempVal->clone(), incr, loc);

    if (maskExpr) {
      StmtList list;
      list.push_back(assignStmt);
      auto ifBlock = builder.buildBlock(list, loc);
      auto ifStmt = builder.buildIfStmt(maskExpr, ifBlock, loc);

      IfStmtList ifList{ifStmt};
      IfConstructKindList kindList{IfConstructKind::IfKind};
      auto ifElseStmt = builder.buildIfElseStmt(ifList, kindList, loc);

      // Now place the if condition inside the innermost loop
      innerBlock->insertFront(ifElseStmt);
    } else {
      innerBlock->insertFront(assignStmt);
    }

    for (auto newStmt : topLevelStmtList) {
      newStmt->setParentNode(currStmt->getParentNode());
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    if (dimVal != -1) {
      ref->replaceWith(countExpr);
    } else {
      ref->replaceWith(countTempVal->clone());
    }
    return true;
  }

  bool expandAny(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    if (ref->getNumOperands() > 2) {
      return true;
    }
    auto expr = ref->getOperand(0);
    int dimVal = -1;
    if (ref->getNumOperands() == 2) {
      auto assignExpr = llvm::cast<AssignmentExpr>(ref->getOperand(1));
      assert(assignExpr->getName() == "dim");
      auto dimExpr = llvm::cast<ConstantVal>(assignExpr->getExpr());
      dimVal = dimExpr->getInt();
    }

    ArraySectionHelper arrySecHelper(builder, currSymTable, C);
    ExprRangeInfoMap map;
    bool hasRange = false;
    arrySecHelper.collectRangeInfoListFor(expr, map, hasRange);

    RangeInfoList *referenceInfoList = nullptr;
    Expr *referenceExpr = nullptr;
    for (auto &val : map) {
      for (auto rangeInfo : val.second.list) {
        if (rangeInfo.isRange()) {
          if (!llvm::isa<ArraySection>(val.first))
            continue;
          referenceInfoList = &val.second.list;
          referenceExpr = val.first;
        }
      }
    }

    StmtVecList topLevelStmtList;

    // Build a variable to count the true values.
    Symbol *countTemp = nullptr;
    Expr *countExpr = nullptr;
    if (dimVal == -1) {
      countTemp = currSymTable->getTempSymbol(Type::getLogicalTy(C), loc);
    } else {
      countTemp = arrySecHelper.getTempArray(
          static_cast<ArraySection *>(referenceExpr), Type::getLogicalTy(C),
          topLevelStmtList, dimVal);
      auto arrSec = builder.buildArraySection(
          countTemp, builder.buildObjectName(countTemp, loc), loc);
      countExpr = arrSec;
      arrySecHelper.collectRangeInfoListFor(arrSec, map, hasRange);
    }

    if (!referenceInfoList) {
      assert(false && "handle scalar count");
    }

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrySecHelper.getLoopNestFor(*referenceInfoList, doStmtList);

    // Top level stmt list to add in place of current statement.
    arrySecHelper.insertOtherRangeInfo(map, referenceExpr, doStmtList,
                                       topLevelStmtList, dimVal, countTemp);

    // Now insert the top-level do loop to top level stmt list.
    topLevelStmtList.push_back(doStmtList.back());

    // Now insert the assignment statement in the innermost loop.
    // and change all the expression values to scalar.
    auto innerBlock = doStmtList[0]->getBlock();

    // Now handle the count part.

    // 2. Build the if statement for storing the count.
    // COnver the expr to scalar one.
    // Replace all the expressions in statements with the scalar ones.
    for (auto &val : map) {
      auto expr = val.first;
      auto finalExpr = val.second.finalExpr;
      if (expr == countExpr) {
        continue;
      }
      expr->replaceWith(finalExpr);
    }

    Expr *countTempVal = nullptr;
    countTempVal = builder.buildObjectName(countTemp, loc);
    // Init tempVal to zero.
    if (dimVal != -1) {
      countTempVal = builder.buildArraySection(countTemp, countTempVal, loc);
    }
    auto initVal = builder.buildAssignmentStmt(
        countTempVal,
        builder.buildConstantVal(".FALSE.", Type::getLogicalTy(C), loc), loc);
    topLevelStmtList.insert(topLevelStmtList.begin(), initVal);

    // Now, increment the countTemp if the statement is true.

    if (dimVal != -1) {
      countTempVal = map[countExpr].finalExpr;
    }
    auto logicalExpr = builder.buildLogicalExpr(countTempVal->clone(), expr,
                                                LogicalOpKind::OR, loc);
    auto assignStmt =
        builder.buildAssignmentStmt(countTempVal->clone(), logicalExpr, loc);

    // Now place the if condition inside the innermost loop
    innerBlock->insertFront(assignStmt);

    for (auto newStmt : topLevelStmtList) {
      newStmt->setParentNode(currStmt->getParentNode());
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    if (dimVal != -1) {
      ref->replaceWith(countExpr);
    } else {
      ref->replaceWith(countTempVal->clone());
    }
    return true;
  }

  bool expandAll(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    if (ref->getNumOperands() > 2) {
      return true;
    }
    auto expr = ref->getOperand(0);
    int dimVal = -1;
    if (ref->getNumOperands() == 2) {
      auto assignExpr = llvm::cast<AssignmentExpr>(ref->getOperand(1));
      assert(assignExpr->getName() == "dim");
      auto dimExpr = llvm::cast<ConstantVal>(assignExpr->getExpr());
      dimVal = dimExpr->getInt();
    }

    ArraySectionHelper arrySecHelper(builder, currSymTable, C);
    ExprRangeInfoMap map;
    bool hasRange = false;
    arrySecHelper.collectRangeInfoListFor(expr, map, hasRange);

    RangeInfoList *referenceInfoList = nullptr;
    Expr *referenceExpr = nullptr;
    for (auto &val : map) {
      for (auto rangeInfo : val.second.list) {
        if (rangeInfo.isRange()) {
          if (!llvm::isa<ArraySection>(val.first))
            continue;
          referenceInfoList = &val.second.list;
          referenceExpr = val.first;
        }
      }
    }

    StmtVecList topLevelStmtList;

    // Build a variable to count the true values.
    Symbol *countTemp = nullptr;
    Expr *countExpr = nullptr;
    if (dimVal == -1) {
      countTemp = currSymTable->getTempSymbol(Type::getLogicalTy(C), loc);
    } else {
      countTemp = arrySecHelper.getTempArray(
          static_cast<ArraySection *>(referenceExpr), Type::getLogicalTy(C),
          topLevelStmtList, dimVal);
      auto arrSec = builder.buildArraySection(
          countTemp, builder.buildObjectName(countTemp, loc), loc);
      countExpr = arrSec;
      arrySecHelper.collectRangeInfoListFor(arrSec, map, hasRange);
    }

    if (!referenceInfoList) {
      assert(false && "handle scalar count");
    }

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrySecHelper.getLoopNestFor(*referenceInfoList, doStmtList);

    // Top level stmt list to add in place of current statement.
    arrySecHelper.insertOtherRangeInfo(map, referenceExpr, doStmtList,
                                       topLevelStmtList, dimVal, countTemp);

    // Now insert the top-level do loop to top level stmt list.
    topLevelStmtList.push_back(doStmtList.back());

    // Now insert the assignment statement in the innermost loop.
    // and change all the expression values to scalar.
    auto innerBlock = doStmtList[0]->getBlock();

    // Now handle the count part.

    // 2. Build the if statement for storing the count.
    // COnver the expr to scalar one.
    // Replace all the expressions in statements with the scalar ones.
    for (auto &val : map) {
      auto expr = val.first;
      auto finalExpr = val.second.finalExpr;
      if (expr == countExpr) {
        continue;
      }
      expr->replaceWith(finalExpr);
    }

    Expr *countTempVal = nullptr;
    countTempVal = builder.buildObjectName(countTemp, loc);
    // Init tempVal to zero.
    if (dimVal != -1) {
      countTempVal = builder.buildArraySection(countTemp, countTempVal, loc);
    }
    auto initVal = builder.buildAssignmentStmt(
        countTempVal,
        builder.buildConstantVal(".TRUE.", Type::getLogicalTy(C), loc), loc);
    topLevelStmtList.insert(topLevelStmtList.begin(), initVal);

    if (dimVal != -1) {
      countTempVal = map[countExpr].finalExpr;
    }
    auto logicalExpr = builder.buildLogicalExpr(countTempVal->clone(), expr,
                                                LogicalOpKind::AND, loc);
    auto assignStmt =
        builder.buildAssignmentStmt(countTempVal->clone(), logicalExpr, loc);

    // Now place the if condition inside the innermost loop
    innerBlock->insertFront(assignStmt);

    for (auto newStmt : topLevelStmtList) {
      newStmt->setParentNode(currStmt->getParentNode());
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    if (dimVal != -1) {
      ref->replaceWith(countExpr);
    } else {
      ref->replaceWith(countTempVal->clone());
    }
    return true;
  }

  bool expandMax(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    unsigned numOps = ref->getNumOperands();
    assert(numOps > 1);

    auto arg = ref->getOperand(0);
    auto argTy = arg->getType();
    if (argTy->isArrayTy()) {
      argTy = static_cast<ArrayType *>(argTy)->getElementTy();
    }

    // Handle it as llvm.maximum.*
    if (argTy->isFloatingTy() && numOps == 2) {
      return true;
    }

    ExprList list;
    for (auto stmt : ref->getOperands()) {
      auto expr = static_cast<Expr *>(stmt);
      list.push_back(expr);
    }

    StmtVecList topLevelStmtList;

    auto tempVal = currSymTable->getTempSymbol(list[0]->getType(), loc);
    auto tempObj = builder.buildObjectName(tempVal, loc);

    // Assign first val as min.
    auto assignMax =
        builder.buildAssignmentStmt(tempObj->clone(), list[0], loc);
    topLevelStmtList.push_back(assignMax);

    for (unsigned I = 1; I < list.size(); ++I) {
      auto logicalExpr = builder.buildRelationalExpr(list[I], tempObj->clone(),
                                                     RelationalOpKind::GT, loc);

      auto assignMax =
          builder.buildAssignmentStmt(tempObj->clone(), list[I]->clone(), loc);
      StmtList ifList{assignMax};
      auto ifBlock = builder.buildBlock(ifList, loc);

      auto ifElseStmt = builder.buildIfElseStmt(logicalExpr, ifBlock, loc);
      topLevelStmtList.push_back(ifElseStmt);
    }

    for (auto newStmt : topLevelStmtList) {
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    ref->replaceWith(tempObj->clone());
    return true;
  }

  bool expandMin(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    unsigned numOps = ref->getNumOperands();
    assert(numOps > 1);

    auto arg = ref->getOperand(0);
    auto argTy = arg->getType();
    if (argTy->isArrayTy()) {
      argTy = static_cast<ArrayType *>(argTy)->getElementTy();
    }

    // Handle it as llvm.minimum.*
    if (argTy->isFloatingTy() && numOps == 2) {
      return true;
    }

    ExprList list;
    for (auto stmt : ref->getOperands()) {
      auto expr = static_cast<Expr *>(stmt);
      list.push_back(expr);
    }

    StmtVecList topLevelStmtList;

    auto tempVal = currSymTable->getTempSymbol(list[0]->getType(), loc);
    auto tempObj = builder.buildObjectName(tempVal, loc);

    // Assign first val as min.
    auto assignMin =
        builder.buildAssignmentStmt(tempObj->clone(), list[0], loc);
    topLevelStmtList.push_back(assignMin);

    for (unsigned I = 1; I < list.size(); ++I) {
      auto logicalExpr = builder.buildRelationalExpr(list[I], tempObj->clone(),
                                                     RelationalOpKind::LT, loc);

      auto assignMin =
          builder.buildAssignmentStmt(tempObj->clone(), list[I]->clone(), loc);
      StmtList ifList{assignMin};
      auto ifBlock = builder.buildBlock(ifList, loc);

      auto ifElseStmt = builder.buildIfElseStmt(logicalExpr, ifBlock, loc);
      topLevelStmtList.push_back(ifElseStmt);
    }

    for (auto newStmt : topLevelStmtList) {
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    ref->replaceWith(tempObj->clone());
    return true;
  }

  bool expandCount(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    if (ref->getNumOperands() > 2) {
      return true;
    }
    auto expr = ref->getOperand(0);
    int dimVal = -1;
    if (ref->getNumOperands() == 2) {
      auto assignExpr = llvm::cast<AssignmentExpr>(ref->getOperand(1));
      assert(assignExpr->getName() == "dim");
      auto dimExpr = llvm::cast<ConstantVal>(assignExpr->getExpr());
      dimVal = dimExpr->getInt();
    }

    ArraySectionHelper arrySecHelper(builder, currSymTable, C);
    ExprRangeInfoMap map;
    bool hasRange = false;
    arrySecHelper.collectRangeInfoListFor(expr, map, hasRange);

    RangeInfoList *referenceInfoList = nullptr;
    Expr *referenceExpr = nullptr;
    for (auto &val : map) {
      for (auto rangeInfo : val.second.list) {
        if (rangeInfo.isRange()) {
          if (!llvm::isa<ArraySection>(val.first))
            continue;
          referenceInfoList = &val.second.list;
          referenceExpr = val.first;
        }
      }
    }

    StmtVecList topLevelStmtList;

    // Build a variable to count the true values.
    Symbol *countTemp = nullptr;
    Expr *countExpr = nullptr;
    if (dimVal == -1) {
      countTemp = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
    } else {
      countTemp =
          arrySecHelper.getTempArray(static_cast<ArraySection *>(referenceExpr),
                                     nullptr, topLevelStmtList, dimVal);
      auto arrSec = builder.buildArraySection(
          countTemp, builder.buildObjectName(countTemp, loc), loc);
      countExpr = arrSec;
      arrySecHelper.collectRangeInfoListFor(arrSec, map, hasRange);
    }

    if (!referenceInfoList) {
      assert(false && "handle scalar count");
    }

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrySecHelper.getLoopNestFor(*referenceInfoList, doStmtList);

    // Top level stmt list to add in place of current statement.
    arrySecHelper.insertOtherRangeInfo(map, referenceExpr, doStmtList,
                                       topLevelStmtList, dimVal, countTemp);

    // Now insert the top-level do loop to top level stmt list.
    topLevelStmtList.push_back(doStmtList.back());

    // Now insert the assignment statement in the innermost loop.
    // and change all the expression values to scalar.
    auto innerBlock = doStmtList[0]->getBlock();

    // Now handle the count part.

    // 2. Build the if statement for storing the count.
    // COnver the expr to scalar one.
    // Replace all the expressions in statements with the scalar ones.
    for (auto &val : map) {
      auto expr = val.first;
      auto finalExpr = val.second.finalExpr;
      if (expr == countExpr) {
        continue;
      }
      expr->replaceWith(finalExpr);
    }

    Expr *countTempVal = nullptr;
    countTempVal = builder.buildObjectName(countTemp, loc);
    // Init tempVal to zero.
    if (dimVal != -1) {
      countTempVal = builder.buildArraySection(countTemp, countTempVal, loc);
    }
    auto initVal = builder.buildAssignmentStmt(
        countTempVal, builder.buildConstantVal("0", Type::getInt32Ty(C), loc),
        loc);
    topLevelStmtList.insert(topLevelStmtList.begin(), initVal);

    // Now, increment the countTemp if the statement is true.

    if (dimVal != -1) {
      countTempVal = map[countExpr].finalExpr;
    }
    auto incr = builder.buildExpr(
        countTempVal->clone(), builder.getConstantOne(loc, Type::getInt32Ty(C)),
        BinaryOpKind::Addition, countTemp->getType(), loc);
    auto assignStmt =
        builder.buildAssignmentStmt(countTempVal->clone(), incr, loc);

    StmtList list;
    list.push_back(assignStmt);
    auto ifBlock = builder.buildBlock(list, loc);
    auto ifStmt = builder.buildIfStmt(expr, ifBlock, loc);

    IfStmtList ifList{ifStmt};
    IfConstructKindList kindList{IfConstructKind::IfKind};
    auto ifElseStmt = builder.buildIfElseStmt(ifList, kindList, loc);

    // Now place the if condition inside the innermost loop
    innerBlock->insertFront(ifElseStmt);

    for (auto newStmt : topLevelStmtList) {
      newStmt->setParentNode(currStmt->getParentNode());
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    if (dimVal != -1) {
      ref->replaceWith(countExpr);
    } else {
      ref->replaceWith(countTempVal->clone());
    }
    return true;
  }

  bool expandMaxloc(FunctionReference *ref, bool isMin = false) {
    auto loc = ref->getSourceLoc();
    if (ref->getNumOperands() > 2) {
      assert(false && "unhandled intrinic");
      return true;
    }
    auto expr = ref->getOperand(0);
    Expr *maskExpr = nullptr;
    int dimVal = -1;
    if (ref->getNumOperands() == 2) {
      auto assignExpr = llvm::cast<AssignmentExpr>(ref->getOperand(1));
      if (assignExpr->getName() == "dim") {
        auto dimExpr = llvm::cast<ConstantVal>(assignExpr->getExpr());
        dimVal = dimExpr->getInt();
      } else if (assignExpr->getName() == "mask") {
        maskExpr = assignExpr->getExpr();
      } else {
        assert(false && "unknown argument type for intrinsic");
      }
    }

    ArraySectionHelper arrySecHelper(builder, currSymTable, C);
    ExprRangeInfoMap map;
    bool hasRange = false;
    arrySecHelper.collectRangeInfoListFor(expr, map, hasRange);
    arrySecHelper.collectRangeInfoListFor(maskExpr, map, hasRange);

    RangeInfoList *referenceInfoList = nullptr;
    Expr *referenceExpr = nullptr;
    for (auto &val : map) {
      for (auto rangeInfo : val.second.list) {
        if (rangeInfo.isRange()) {
          if (!llvm::isa<ArraySection>(val.first))
            continue;
          referenceInfoList = &val.second.list;
          referenceExpr = val.first;
        }
      }
    }

    StmtVecList topLevelStmtList;
    auto sym =
        llvm::cast<ArraySection>(referenceExpr)->getSymbol()->getOrigSymbol();
    auto arrType = llvm::cast<ArrayType>(sym->getType());

    if (!referenceInfoList) {
      assert(false && "handle scalar count");
    }

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrySecHelper.getLoopNestFor(*referenceInfoList, doStmtList);

    auto val = isMin ? getMaxValueForType(arrType->getElementTy())
                     : getMinValueForType(arrType->getElementTy());
    auto minVal = builder.buildConstantVal(std::to_string(val),
                                           arrType->getElementTy(), loc);

    // Build a variable to count the max loc values.
    Symbol *countTemp = nullptr;
    Expr *countExpr = nullptr;
    Symbol *tempMaxSym = nullptr;
    Expr *maxValExpr = nullptr;
    Expr *maxValObj = nullptr;
    if (dimVal == -1 || doStmtList.size() == 1) {

      countTemp = currSymTable->getTempSymbol(Type::getLogicalTy(C), loc);

      ArrBoundsList list;
      auto tempArrTy = Type::getInt32Ty(C);
      if (doStmtList.size() != 1) {
        list.push_back(std::make_pair(1, doStmtList.size()));
        tempArrTy = ArrayType::get(C, Type::getInt32Ty(C), list);
      }
      countTemp = currSymTable->getTempSymbol(tempArrTy, loc);
      tempMaxSym = currSymTable->getTempSymbol(arrType->getElementTy(), loc);
      maxValObj = builder.buildObjectName(tempMaxSym, loc);
    } else {
      countTemp = arrySecHelper.getTempArray(
          static_cast<ArraySection *>(referenceExpr), Type::getInt32Ty(C),
          topLevelStmtList, dimVal);
      tempMaxSym = arrySecHelper.getTempArray(
          static_cast<ArraySection *>(referenceExpr), arrType->getElementTy(),
          topLevelStmtList, dimVal);

      auto arrSec = builder.buildArraySection(
          countTemp, builder.buildObjectName(countTemp, loc), loc);

      maxValObj = builder.buildObjectName(tempMaxSym, loc);
      maxValObj = builder.buildArraySection(tempMaxSym, maxValObj, loc);
      countExpr = arrSec;
      maxValExpr = maxValObj;
      arrySecHelper.collectRangeInfoListFor(arrSec, map, hasRange);
      arrySecHelper.collectRangeInfoListFor(maxValObj, map, hasRange);
    }

    // Assign the min value to temp array.
    auto minValAssign =
        builder.buildAssignmentStmt(maxValObj->clone(), minVal, loc);
    topLevelStmtList.push_back(minValAssign);

    // Top level stmt list to add in place of current statement.
    arrySecHelper.insertOtherRangeInfo(map, referenceExpr, doStmtList,
                                       topLevelStmtList, dimVal,
                                       {countTemp, tempMaxSym});

    // Now insert the top-level do loop to top level stmt list.
    topLevelStmtList.push_back(doStmtList.back());

    // Now insert the assignment statement in the innermost loop.
    // and change all the expression values to scalar.
    auto innerBlock = doStmtList[0]->getBlock();

    // Now handle the count part.

    // 2. Build the if statement for storing the count.
    // COnver the expr to scalar one.
    // Replace all the expressions in statements with the scalar ones.
    for (auto &val : map) {
      auto expr = val.first;
      auto finalExpr = val.second.finalExpr;
      if (expr == countExpr || expr == maxValObj) {
        continue;
      }

      expr->replaceWith(finalExpr);
    }

    Expr *countTempVal = nullptr;
    countTempVal = builder.buildObjectName(countTemp, loc);
    // Init tempVal to 0.
    if (doStmtList.size() > 1)
      countTempVal = builder.buildArraySection(countTemp, countTempVal, loc);
    auto initVal = builder.buildAssignmentStmt(
        countTempVal, builder.buildConstantVal("0", Type::getInt32Ty(C), loc),
        loc);
    topLevelStmtList.insert(topLevelStmtList.begin(), initVal);

    if (dimVal != -1 && doStmtList.size() > 1) {
      countTempVal = map[countExpr].finalExpr;
      maxValObj = map[maxValObj].finalExpr;
    }
    if (expr->isDesignator()) {
      expr = map[expr].finalExpr;
    }

    auto logicalExpr = builder.buildRelationalExpr(
        expr, maxValObj->clone(),
        isMin ? RelationalOpKind::LT : RelationalOpKind::GT, loc);

    StmtList ifList;

    // Now Assign the array location in the temp count val.
    // auto refArrEle = llvm::cast<ArrayElement>(map[referenceExpr].finalExpr);
    auto subsList = map[referenceExpr].list;

    if (dimVal == -1 || doStmtList.size() == 1) {
      unsigned I = 1;
      for (auto &rangeInfo : subsList) {
        if (!rangeInfo.isRange()) {
          continue;
        }
        auto currSub = builder.buildConstantVal(std::to_string(I),
                                                Type::getInt32Ty(C), loc);
        Expr *countVal = builder.buildObjectName(countTemp, loc);

        if (doStmtList.size() != 1) {
          ExprList tempSubList{currSub};
          countVal =
              builder.buildArrayElement(countTemp, countVal, tempSubList, loc);
        }

        auto diff = builder.buildExpr(
            rangeInfo.indvar->clone(), rangeInfo.bounds.first->clone(),
            BinaryOpKind::Subtraction, rangeInfo.indvar->getType(), loc);

        auto absInput =
            currSymTable->getTempSymbol(rangeInfo.indvar->getType(), loc);
        auto absObj = builder.buildObjectName(absInput, loc);

        auto absAssign = builder.buildAssignmentStmt(absObj, diff, loc);
        ifList.push_back(absAssign);
        auto absRef = currSymTable->getOrInsertSymbol(
            "abs", FunctionType::getUndeclaredFuncTy(C), loc);

        ExprList absList{absObj};
        auto absExpr = builder.buildFunctionReference(absRef, absList,
                                                      absRef->getType(), loc);
        auto addOne = builder.buildAddExpr(
            absExpr, builder.getConstantOne(loc, rangeInfo.indvar->getType()));
        auto assignIndex = builder.buildAssignmentStmt(countVal, addOne, loc);
        ifList.push_back(assignIndex);
        I++;
      }
    } else {
      unsigned I = 1;
      for (auto &rangeInfo : subsList) {
        if (!rangeInfo.isRange()) {
          continue;
        }
        if (I == dimVal) {
          auto diff = builder.buildExpr(
              rangeInfo.indvar->clone(), rangeInfo.bounds.first->clone(),
              BinaryOpKind::Subtraction, rangeInfo.indvar->getType(), loc);

          auto absInput =
              currSymTable->getTempSymbol(rangeInfo.indvar->getType(), loc);

          auto absObj = builder.buildObjectName(absInput, loc);

          auto absAssign = builder.buildAssignmentStmt(absObj, diff, loc);
          ifList.push_back(absAssign);
          auto absRef = currSymTable->getOrInsertSymbol(
              "abs", FunctionType::getUndeclaredFuncTy(C), loc);

          ExprList absList{absObj};
          auto absExpr = builder.buildFunctionReference(absRef, absList,
                                                        absRef->getType(), loc);

          auto addOne = builder.buildAddExpr(
              absExpr,
              builder.getConstantOne(loc, rangeInfo.indvar->getType()));
          auto assignIndex =
              builder.buildAssignmentStmt(countTempVal->clone(), addOne, loc);
          ifList.push_back(assignIndex);
        }
        I++;
      }
    }

    // Update the map value in maxValObj.
    auto updateMaxVal =
        builder.buildAssignmentStmt(maxValObj->clone(), expr->clone(), loc);
    ifList.push_back(updateMaxVal);

    // Build the if statement.
    auto ifBlock = builder.buildBlock(ifList, loc);
    auto ifElseStmt = builder.buildIfElseStmt(logicalExpr, ifBlock, loc);

    // Now place the if condition inside the innermost loop

    // if there is mask expression, put the above if inside the mask.
    if (maskExpr) {
      StmtList maskList;
      maskList.push_back(ifElseStmt);
      auto maskBlock = builder.buildBlock(maskList, loc);
      ifElseStmt = builder.buildIfElseStmt(maskExpr, maskBlock, loc);
    }

    innerBlock->insertFront(ifElseStmt);

    for (auto newStmt : topLevelStmtList) {
      newStmt->setParentNode(currStmt->getParentNode());
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    if (dimVal != -1 && doStmtList.size() > 1) {
      ref->replaceWith(countExpr);
    } else {
      ref->replaceWith(countTempVal->clone());
    }
    return true;
  }

  bool expandMaxval(FunctionReference *ref, bool isMin = false) {
    auto loc = ref->getSourceLoc();
    if (ref->getNumOperands() > 2) {
      assert(false && "unhandled intrinic");
      return true;
    }
    auto expr = ref->getOperand(0);
    Expr *maskExpr = nullptr;
    int dimVal = -1;
    if (ref->getNumOperands() == 2) {
      auto assignExpr = llvm::cast<AssignmentExpr>(ref->getOperand(1));
      if (assignExpr->getName() == "dim") {
        auto dimExpr = llvm::cast<ConstantVal>(assignExpr->getExpr());
        dimVal = dimExpr->getInt();
      } else if (assignExpr->getName() == "mask") {
        maskExpr = assignExpr->getExpr();
      } else {
        assert(false && "unknown argument type for intrinsic");
      }
    }

    ArraySectionHelper arrySecHelper(builder, currSymTable, C);
    ExprRangeInfoMap map;
    bool hasRange = false;
    arrySecHelper.collectRangeInfoListFor(expr, map, hasRange);
    if (maskExpr)
      arrySecHelper.collectRangeInfoListFor(maskExpr, map, hasRange);

    RangeInfoList *referenceInfoList = nullptr;
    Expr *referenceExpr = nullptr;
    for (auto &val : map) {
      for (auto rangeInfo : val.second.list) {
        if (rangeInfo.isRange()) {
          if (!llvm::isa<ArraySection>(val.first))
            continue;
          referenceInfoList = &val.second.list;
          referenceExpr = val.first;
        }
      }
    }

    StmtVecList topLevelStmtList;
    auto sym =
        llvm::cast<ArraySection>(referenceExpr)->getSymbol()->getOrigSymbol();
    auto arrType = llvm::cast<ArrayType>(sym->getType());

    if (!referenceInfoList) {
      assert(false && "handle scalar count");
    }

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrySecHelper.getLoopNestFor(*referenceInfoList, doStmtList);

    auto val = isMin ? getMaxValueForType(arrType->getElementTy())
                     : getMinValueForType(arrType->getElementTy());
    auto minVal = builder.buildConstantVal(std::to_string(val),
                                           arrType->getElementTy(), loc);

    // Build a variable to count the max loc values.
    Symbol *tempMaxSym = nullptr;
    Expr *maxValExpr = nullptr;
    Expr *maxValObj = nullptr;
    if (dimVal == -1 || doStmtList.size() == 1) {

      ArrBoundsList list;
      auto tempArrTy = Type::getInt32Ty(C);
      if (doStmtList.size() != 1) {
        list.push_back(std::make_pair(1, doStmtList.size()));
        tempArrTy = ArrayType::get(C, Type::getInt32Ty(C), list);
      }
      tempMaxSym = currSymTable->getTempSymbol(arrType->getElementTy(), loc);
      maxValObj = builder.buildObjectName(tempMaxSym, loc);
    } else {
      tempMaxSym = arrySecHelper.getTempArray(
          static_cast<ArraySection *>(referenceExpr), arrType->getElementTy(),
          topLevelStmtList, dimVal);

      maxValObj = builder.buildObjectName(tempMaxSym, loc);
      maxValObj = builder.buildArraySection(tempMaxSym, maxValObj, loc);
      maxValExpr = maxValObj;
      arrySecHelper.collectRangeInfoListFor(maxValObj, map, hasRange);
    }

    // Assign the min value to temp array.
    auto minValAssign =
        builder.buildAssignmentStmt(maxValObj->clone(), minVal, loc);
    topLevelStmtList.push_back(minValAssign);

    // Top level stmt list to add in place of current statement.
    arrySecHelper.insertOtherRangeInfo(map, referenceExpr, doStmtList,
                                       topLevelStmtList, dimVal, tempMaxSym);

    // Now insert the top-level do loop to top level stmt list.
    topLevelStmtList.push_back(doStmtList.back());

    // Now insert the assignment statement in the innermost loop.
    // and change all the expression values to scalar.
    auto innerBlock = doStmtList[0]->getBlock();

    // Now handle the count part.

    // 2. Build the if statement for storing the count.
    // COnver the expr to scalar one.
    // Replace all the expressions in statements with the scalar ones.
    for (auto &val : map) {
      auto expr = val.first;
      auto finalExpr = val.second.finalExpr;
      if (expr == maxValObj) {
        continue;
      }

      expr->replaceWith(finalExpr);
    }

    if (dimVal != -1 && doStmtList.size() > 1) {
      maxValObj = map[maxValObj].finalExpr;
    }
    if (expr->isDesignator()) {
      expr = map[expr].finalExpr;
    }

    auto logicalExpr = builder.buildRelationalExpr(
        expr, maxValObj->clone(),
        isMin ? RelationalOpKind::LT : RelationalOpKind::GT, loc);

    StmtList ifList;

    // Now Assign the array location in the temp count val.
    // auto refArrEle = llvm::cast<ArrayElement>(map[referenceExpr].finalExpr);
    auto subsList = map[referenceExpr].list;

    // Update the map value in maxValObj.
    auto updateMaxVal =
        builder.buildAssignmentStmt(maxValObj->clone(), expr->clone(), loc);
    ifList.push_back(updateMaxVal);

    // Build the if statement.
    auto ifBlock = builder.buildBlock(ifList, loc);
    auto ifElseStmt = builder.buildIfElseStmt(logicalExpr, ifBlock, loc);

    // Now place the if condition inside the innermost loop

    // if there is mask expression, put the above if inside the mask.
    if (maskExpr) {
      StmtList maskList;
      maskList.push_back(ifElseStmt);
      auto maskBlock = builder.buildBlock(maskList, loc);
      ifElseStmt = builder.buildIfElseStmt(maskExpr, maskBlock, loc);
    }

    innerBlock->insertFront(ifElseStmt);

    for (auto newStmt : topLevelStmtList) {
      newStmt->setParentNode(currStmt->getParentNode());
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    if (dimVal != -1 && doStmtList.size() > 1) {
      ref->replaceWith(maxValExpr);
    } else {
      ref->replaceWith(maxValObj->clone());
    }
    return true;
  }

  bool expandTranspose(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    assert(ref->getNumOperands() == 1);

    auto expr = ref->getOperand(0);

    auto arrSec = llvm::dyn_cast<ArraySection>(expr);
    if (!arrSec) {
      FC_DEBUG(debug() << "Not expanding array section.");
      return true;
    }

    auto origSym = arrSec->getSymbol()->getOrigSymbol();
    auto arrType = llvm::cast<ArrayType>(origSym->getType());
    assert(arrType->getNumDims() == 2 &&
           "Invalid array dimension for transpose");

    ArraySectionHelper arrSecHelper(builder, currSymTable, C);
    StmtVecList newStmtList;

    auto baseEleType = arrType->getElementTy();
    auto tempArr =
        arrSecHelper.getTempArray(arrSec, baseEleType, newStmtList, -1, true);
    assert(tempArr);
    auto tempArrSec = builder.buildArraySection(
        tempArr, builder.buildObjectName(tempArr, loc), loc);

    // Now do the conversion.
    ExprRangeInfoMap map;
    bool hasRange = false;
    arrSecHelper.collectRangeInfoListFor(expr, map, hasRange);
    arrSecHelper.collectRangeInfoListFor(tempArrSec, map, hasRange);

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrSecHelper.getLoopNestFor(map[arrSec].list, doStmtList);
    auto innerBlock = doStmtList[0]->getBlock();

    arrSecHelper.insertOtherRangeInfo(map, arrSec, doStmtList, newStmtList);

    // Now do the scalar conversion.
    auto arrEle = llvm::cast<ArrayElement>(map[arrSec].finalExpr);

    auto tempArrEle = llvm::cast<ArrayElement>(map[tempArrSec].finalExpr);
    assert(tempArrEle);

    auto subsList = tempArrEle->getSubscriptList();
    std::reverse(subsList.begin(), subsList.end());
    tempArrEle = builder.buildArrayElement(
        tempArrEle->getSymbol(), tempArrEle->getOperand(0), subsList, loc);
    innerBlock->insertFront(
        builder.buildAssignmentStmt(tempArrEle, arrEle, loc));

    newStmtList.push_back(doStmtList.back());

    for (auto newStmt : newStmtList) {
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    // now replace the function reference with the arraysecion of temp array.
    ref->replaceWith(tempArrSec->clone());
    return true;
  }

  bool expandTrim(FunctionReference *ref) {
    auto argList = ref->getArgsList();
    assert(argList.size() == 1);
    auto assign = llvm::dyn_cast<AssignmentStmt>(currStmt);

    // Assign emit call to runtime function
    if (!assign) {
      // TODO : Merge repeating code!
      auto call =
          builder.buildCallStmt(ref->getSymbol(), argList, ref->getSourceLoc());
      auto arg1 = llvm::dyn_cast<ArraySection>(argList.front());
      assert(arg1);
      auto res = arg1->clone();
      currBlock->insertStmtBefore(call, currStmt);
      ref->replaceWith(res);
      return true;
    }

    auto lhs = llvm::dyn_cast<ArraySection>(assign->getLHS());
    auto arg1 = llvm::dyn_cast<ArraySection>(argList.front());

    assert(lhs && arg1 && lhs->isFullRange() && arg1->isFullRange());
    if (lhs->getSymbol() == arg1->getSymbol()) {

      // It is being assigned to same array. just call it as subroutine, we will
      // handle it in runtime.
      auto call =
          builder.buildCallStmt(ref->getSymbol(), argList, ref->getSourceLoc());

      currBlock->replaceWith(call, currStmt);
      return true;
    }

    // Use the same symbol as result symbol
    auto res = arg1->clone();
    auto call =
        builder.buildCallStmt(ref->getSymbol(), argList, ref->getSourceLoc());
    currBlock->insertStmtBefore(call, currStmt);
    ref->replaceWith(res);
    return true;
  }

  bool expandSpread(FunctionReference *ref) {
    auto loc = ref->getSourceLoc();
    auto args = ref->getArgsList();
    auto I32 = Type::getInt32Ty(C);
    auto one = builder.getConstantOne(loc, I32);
    assert(args.size() == 3);
    auto dimExpr = llvm::dyn_cast<ConstantVal>(args[1]);
    assert(dimExpr && "Only const dimExpr are supported");
    auto dim = dimExpr->getInt();
    assert((dim == 1 || dim == 2) && "Only 2D spread is supported");

    auto arraySection = llvm::dyn_cast<ArraySection>(args[0]);
    assert(arraySection);

    ArraySectionHelper arrSecHelper(builder, currSymTable, C);
    RangeInfoList list;

    arrSecHelper.computeArrSecRangeInfo(arraySection, list, false);
    assert(list.size() == 1 && "Input should be 1D array");
    assert(list[0].isRange());

    DynArrBoundsList resultBounds;
    ExprList strideList;
    SymbolList indVarList;

    // If spreading across first dimension, repeat count will be number of
    // rows in result else will be number if number columns in result
    if (dim == 1) {
      resultBounds.push_back(std::make_pair(one->clone(), args[2]->clone()));
      strideList.push_back(one->clone());
      resultBounds.push_back(std::make_pair(list[0].bounds.first->clone(),
                                            list[0].bounds.second->clone()));
      strideList.push_back(list[0].stride);

    } else {
      resultBounds.push_back(std::make_pair(list[0].bounds.first->clone(),
                                            list[0].bounds.second->clone()));
      strideList.push_back(list[0].stride);
      resultBounds.push_back(std::make_pair(one->clone(), args[2]->clone()));
      strideList.push_back(one->clone());
    }

    // Create resultant array
    auto arrayTy = llvm::dyn_cast<ArrayType>(arraySection->getType());
    assert(arrayTy);
    auto elementTy = arrayTy->getElementTy();
    auto resultTy = ArrayType::get(C, elementTy, resultBounds.size());
    auto resultSym = currSymTable->getTempSymbol(resultTy, loc);

    StmtVecList stmtList;
    auto arraySpec =
        builder.buildArraySpec(resultBounds, resultBounds.size(), loc);
    ArraySpecList specList{arraySpec};
    SymbolList symList{resultSym};

    // Allocate the result symbol
    auto allocateStmt = builder.buildAllocateStmt(symList, specList, loc);
    stmtList.push_back(allocateStmt);

    auto resultObj = builder.buildObjectName(resultSym, loc);
    unsigned numDims = resultBounds.size();

    ExprList subsList;
    for (int i = 0; i < numDims; ++i) {
      indVarList.push_back(currSymTable->getTempSymbol(I32, loc));
      subsList.push_back(builder.buildObjectName(indVarList[i], loc));
    }

    auto loopNest =
        builder.buildLoopNestFor(indVarList, resultBounds, strideList, loc);
    auto innerBlock = loopNest[numDims - 1]->getBlock();
    auto resultArrEle =
        builder.buildArrayElement(resultSym, resultObj, subsList, loc);

    ExprList rhsSubsList;
    if (dim == 1)
      rhsSubsList.push_back(builder.buildObjectName(indVarList[1], loc));
    else
      rhsSubsList.push_back(builder.buildObjectName(indVarList[0], loc));

    // for a = repeat(b(), dim, repeatCount)
    // createt a(i, j) = b(k)
    // where k = i, if dim = 2 or k = j if dim = 1
    auto rhsSym = arraySection->getSymbol();
    auto rhsObj = builder.buildObjectName(rhsSym, loc);
    auto rhsEle = builder.buildArrayElement(rhsSym, rhsObj, rhsSubsList, loc);

    auto assignment = builder.buildAssignmentStmt(resultArrEle, rhsEle, loc);
    innerBlock->insertFront(assignment);
    stmtList.push_back(loopNest[0]);

    for (auto stmt : stmtList) {
      currBlock->insertStmtBefore(stmt, currStmt);
    }

    auto resultArrSec =
        builder.buildArraySection(resultSym, resultObj->clone(), loc);

    ref->replaceWith(resultArrSec);
    return true;
  }

  bool postFunctionReference(FunctionReference *ref) {

    auto sym = ref->getSymbol()->getOrigSymbol();
    auto loc = ref->getSourceLoc();
    auto intrinKind = fc::intrin::getIntrinsicKind(sym->getName());
    if (intrinKind == fc::intrin::none) {
      return true;
    }

    switch (intrinKind) {
    case fc::intrin::huge: {
      assert(ref->getNumOperands() == 1);
      auto arg = ref->getExpr(0);
      auto ty = arg->getType();

      long int val = -1;
      switch (ty->getTypeID()) {
      case Type::Int32ID:
        val = 2147483647;
        break;
      default:
        llvm_unreachable("Unhandled huge value");
      };
      auto constVal = builder.buildConstantVal(std::to_string(val), ty, loc);
      ref->replaceWith(constVal);
      return true;
    }
    case fc::intrin::sign: {
      assert(ref->getNumOperands() == 2);
      auto arg1 = ref->getOperand(0);
      auto arg2 = ref->getOperand(1);

      Type *arg1Ty = arg1->getType();
      if (arg1Ty->isArrayTy()) {
        arg1Ty = static_cast<ArrayType *>(arg1Ty)->getElementTy();
      }

      // Build temp symbol for result.
      auto result = currSymTable->getTempSymbol(arg1Ty, loc);
      auto absRef = currSymTable->getOrInsertSymbol(
          "abs", FunctionType::getUndeclaredFuncTy(C), loc);
      // Build if condition.
      auto logicalCond = builder.buildRelationalExpr(
          arg2, builder.buildConstantVal("0", arg2->getType(), loc),
          RelationalOpKind::GE, loc);

      StmtList list;

      // Build if body.
      auto resultVal = builder.buildObjectName(result, loc);
      ExprList exprList;
      exprList.push_back(arg1->clone());
      auto finalVal =
          builder.buildFunctionReference(absRef, exprList, arg1Ty, loc);
      list.push_back(builder.buildAssignmentStmt(resultVal, finalVal, loc));
      auto ifBody = builder.buildBlock(list, loc);
      list.clear();
      auto ifStmt = builder.buildIfStmt(logicalCond, ifBody, loc);

      // Build else body.
      exprList.clear();
      resultVal = static_cast<ObjectName *>(resultVal->clone());
      finalVal = static_cast<FunctionReference *>(finalVal->clone());

      auto negExpr =
          builder.buildExpr(nullptr, finalVal, BinaryOpKind::Subtraction,
                            finalVal->getType(), loc);
      list.push_back(builder.buildAssignmentStmt(resultVal, negExpr, loc));
      auto elseBody = builder.buildBlock(list, loc);

      auto elseStmt = builder.buildIfStmt(nullptr, elseBody, loc);

      // Build if else stmt.
      IfStmtList ifList{ifStmt, elseStmt};
      IfConstructKindList kindList{IfConstructKind::IfKind,
                                   IfConstructKind::ElseKind};
      auto ifElseStmt = builder.buildIfElseStmt(ifList, kindList, loc);
      ifElseStmt->setParentNode(currStmt->getParentNode());
      currBlock->insertStmtBefore(ifElseStmt, currStmt);
      ref->replaceWith(resultVal->clone());
      return visit(ifElseStmt);
    }
    case fc::intrin::abs: {
      assert(ref->getNumOperands() == 1);
      auto arg = ref->getOperand(0);
      auto argTy = arg->getType();
      if (argTy->isArrayTy()) {
        argTy = static_cast<ArrayType *>(argTy)->getElementTy();
      }

      // Handle it as llvm.fabs.*
      if (argTy->isFloatingTy()) {
        return true;
      }
      // Build temp symbol for result.
      auto result = currSymTable->getTempSymbol(argTy, loc);
      // Build if condition.
      auto logicalCond = builder.buildRelationalExpr(
          arg, builder.buildConstantVal("0", argTy, loc), RelationalOpKind::GE,
          loc);

      StmtList list;

      // Build if body.
      auto resultVal = builder.buildObjectName(result, loc);
      list.push_back(builder.buildAssignmentStmt(resultVal, arg->clone(), loc));
      auto ifBody = builder.buildBlock(list, loc);
      list.clear();
      auto ifStmt = builder.buildIfStmt(logicalCond, ifBody, loc);

      // Build else body.
      resultVal = static_cast<ObjectName *>(resultVal->clone());

      auto negExpr = builder.buildExpr(nullptr, arg->clone(),
                                       BinaryOpKind::Subtraction, argTy, loc);
      list.push_back(builder.buildAssignmentStmt(resultVal, negExpr, loc));
      auto elseBody = builder.buildBlock(list, loc);

      auto elseStmt = builder.buildIfStmt(nullptr, elseBody, loc);

      // Build if else stmt.
      IfStmtList ifList{ifStmt, elseStmt};
      IfConstructKindList kindList{IfConstructKind::IfKind,
                                   IfConstructKind::ElseKind};
      auto ifElseStmt = builder.buildIfElseStmt(ifList, kindList, loc);
      ifElseStmt->setParentNode(currStmt->getParentNode());
      currBlock->insertStmtBefore(ifElseStmt, currStmt);
      ref->replaceWith(resultVal->clone());
      return visit(ifElseStmt);
    }
    case fc::intrin::reshape: {
      return expandReshape(ref);
    }
    case fc::intrin::min: {
      return expandMin(ref);
    }
    case fc::intrin::max: {
      return expandMax(ref);
    }
    case fc::intrin::count: {
      return expandCount(ref);
    }
    case fc::intrin::INT: {
      return expandInt(ref);
    }
    case fc::intrin::cshift: {
      return expandCshift(ref);
    }
    case fc::intrin::sum: {
      return expandSum(ref);
    }
    case fc::intrin::any: {
      return expandAny(ref);
    }
    case fc::intrin::all: {
      return expandAll(ref);
    }
    case fc::intrin::maxloc: {
      return expandMaxloc(ref);
    }
    case fc::intrin::minloc: {
      return expandMaxloc(ref, true);
    }
    case fc::intrin::transpose: {
      return expandTranspose(ref);
    }
    case fc::intrin::maxval: {
      return expandMaxval(ref);
    }
    case fc::intrin::minval: {
      return expandMaxval(ref, true);
    }
    case fc::intrin::trim: {
      return expandTrim(ref);
    }
    case fc::intrin::index: {
      return expandIndex(ref);
    }
    case fc::intrin::repeat: {
      return expandRepeat(ref);
    }
    case fc::intrin::size: {
      return expandSize(ref);
    }
    case fc::intrin::adjustl: {
      return expandAdjustl(ref);
    }
    case fc::intrin::spread: {
      return expandSpread(ref);
    }
    default:
      break;
    }
    return true;
  }
}; // namespace fc

class IntrinsicExpanderPass : public ASTBlockPass {
  IntrinsicHandler intrinHandler;

public:
public:
  IntrinsicExpanderPass(ASTContext &C)
      : ASTBlockPass(C, "Intrinsic expander"), intrinHandler(C) {}

  bool runOnBlock(Block *block) override {
    intrinHandler.setBlock(block);
    intrinHandler.setSymbolTable(currPU->getSymbolTable());

    // Quick check to see if the curr PU has intrinsic.
    // If not, quit early.
    // TODO: optimize.
    bool hasIntrinsic = false;
    for (auto sym : currPU->getSymbolTable()->getOrigSymbolList()) {
      if (!sym->getType()->isFunctionTy())
        continue;

      if (intrin::isIntrinsic(sym->getName())) {
        hasIntrinsic = true;
        break;
      }
    }
    if (!hasIntrinsic) {
      return true;
    }

    auto stmtList = block->getStmtList();
    for (auto stmt : stmtList) {
      intrinHandler.setStmt(stmt);
      if (!intrinHandler.visit(stmt)) {
        return false;
      }
    }
    return true;
  }

}; // namespace fc

ASTPass *createIntrinsicExpanderPass(ASTContext &C) {
  return new IntrinsicExpanderPass(C);
}
} // namespace fc
