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
#include "sema/ExpansionUtils.h"

using namespace fc;
using namespace ast;

void ArraySectionHelper::computeArrSecRangeInfo(ArraySection *arrSec,
                                                RangeInfoList &infoList,
                                                bool createIndVar) {
  auto sym = arrSec->getSymbol()->getOrigSymbol();
  assert(sym->getType()->isArrayTy());
  auto arrTy = llvm::cast<ArrayType>(sym->getType());

  auto subsList = arrSec->getSubscriptList();
  auto isFullRange = arrSec->isFullRange();
  auto loc = arrSec->getSourceLoc();
  auto numDims = arrTy->getNumDims();

  for (unsigned I = 0; I < numDims; ++I) {

    RangeInfo info;

    auto isDimFullRange = isFullRange;
    RangeExpr *rangeExpr = nullptr;
    if (!isFullRange) {
      auto subscript = subsList[I];
      rangeExpr = llvm::dyn_cast<RangeExpr>(subscript);
      isDimFullRange = (rangeExpr && rangeExpr->isFullRange());
    }

    // range is equal to the array dimension range.
    if (isDimFullRange) {
      info.bounds.first = builder.buildLBoundIntrin(sym, I + 1, loc);
      info.bounds.second = builder.buildUBoundIntrin(sym, I + 1, loc);
      info.stride = builder.getConstantOne(loc, Type::getInt32Ty(C));
      if (createIndVar) {
        auto tempSym = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
        info.indvar = builder.buildObjectName(tempSym, loc);
      }
    } else if (rangeExpr) {
      // As given by the range expr.
      info.bounds.first = rangeExpr->getLowerBound();
      info.bounds.second = rangeExpr->getUpperBound();
      info.stride = (rangeExpr->getIndex());
      if (!info.stride) {
        info.stride = builder.getConstantOne(loc, Type::getInt32Ty(C));
      }
      if (createIndVar) {
        auto tempSym = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
        info.indvar = builder.buildObjectName(tempSym, loc);
      }
    } else {
      // not a stride. Just a scalar value.
      info.bounds.first = subsList[I];
    }
    infoList.push_back(info);
  }
}

ArrayElement *ArraySectionHelper::getArrayElementFor(ArraySection *arrSec,
                                                     RangeInfoList &list) {
  auto sym = arrSec->getSymbol()->getOrigSymbol();
  auto loc = arrSec->getSourceLoc();

  ExprList subsList;
  for (auto rangeInfo : list) {
    if (rangeInfo.isRange()) {
      subsList.push_back(rangeInfo.indvar->clone());
    } else {
      subsList.push_back(rangeInfo.bounds.first);
    }
  }

  return builder.buildArrayElement(sym, builder.buildObjectName(sym, loc),
                                   subsList, loc);
}

class ArrSecExprVisitor : public StmtVisitor<ArrSecExprVisitor, bool> {
  ArraySectionHelper &helper;
  ExprRangeInfoMap &map;

  void defaultAssign(Expr *expr) {
    ExprRangeInfo info;
    info.finalExpr = expr;
    map[expr] = info;
  }

public:
  bool hasRange{false};

  ArrSecExprVisitor(ArraySectionHelper &helper, ExprRangeInfoMap &map)
      : helper(helper), map(map) {}

  bool visitObjectName(ObjectName *name) override {
    defaultAssign(name);
    return true;
  }

  bool visitConstantVal(ConstantVal *name) override {
    defaultAssign(name);
    return true;
  }

  bool visitArrayElement(ArrayElement *ele) override {
    defaultAssign(ele);
    return true;
  }

  bool visitFunctionReference(FunctionReference *ref) override {
    defaultAssign(ref);
    return true;
  }

  bool visitArraySection(ArraySection *arrSec) override {
    hasRange = true;
    ExprRangeInfo info;
    helper.computeArrSecRangeInfo(arrSec, info.list);
    info.finalExpr = helper.getArrayElementFor(arrSec, info.list);
    map[arrSec] = info;
    return true;
  }

  bool visitArrayConstructor(ArrayConstructor *arrCon) override {
    auto spec = arrCon->getSpec();
    // TODO: there might be multiple quad expressions.
    // How to handle it?
    assert(spec->getNumOperands() == 1);
    auto quadExpr = llvm::dyn_cast<QuadExpr>(spec->getValue(0));
    assert(quadExpr && "only quad expr is being handled now.");
    RangeInfo info;
    info.bounds.first = quadExpr->getInit();
    info.bounds.second = quadExpr->getEnd();
    info.stride = quadExpr->getIncr();
    info.indvar = llvm::dyn_cast<ObjectName>(quadExpr->getVar());
    assert(info.indvar);

    ExprRangeInfo exprRangeInfo;
    exprRangeInfo.list.push_back(info);
    exprRangeInfo.finalExpr = info.indvar;
    map[arrCon] = exprRangeInfo;
    hasRange = true;
    return true;
  }
};

bool ArraySectionHelper::collectRangeInfoListFor(Expr *expr,
                                                 ExprRangeInfoMap &map,
                                                 bool &hasRange) {
  ArrSecExprVisitor visitor(*this, map);
  if (!visitor.visit(expr)) {
    return false;
  }
  hasRange = visitor.hasRange;
  return true;
}

bool ArraySectionHelper::getLoopNestFor(
    RangeInfoList &list, llvm::SmallVector<DoStmt *, 2> &doStmtList) {

  DoStmt *innerDo = nullptr;
  for (auto &info : list) {
    if (!info.isRange()) {
      continue;
    }
    // Build quadExpr
    auto loc = info.indvar->getSourceLoc();
    Expr *ops[4] = {info.indvar->clone(), info.bounds.first, info.bounds.second,
                    info.stride};
    auto quadExpr = builder.buildQuadExpr(ops, info.indvar->getType(), loc);
    StmtList list;
    if (innerDo) {
      list.push_back(innerDo);
    }
    auto block = builder.buildBlock(list, loc);
    auto doStmt = builder.buildDoStmt(quadExpr, block, "", loc);
    innerDo = doStmt;
    doStmtList.push_back(doStmt);
  }
  return true;
}

// Insert the indvars for other ranges in other Expressions.
bool ArraySectionHelper::insertOtherRangeInfo(
    ExprRangeInfoMap &map, Expr *referenceExpr,
    llvm::SmallVector<DoStmt *, 2> &doStmtList, StmtVecList &topStmtList,
    int ignoreDim, llvm::ArrayRef<Symbol *> tempSymbolList) {

  // For every expression with range info.
  for (auto &val : map) {
    auto expr = val.first;
    bool isTempSymbol = false;

    // This is the part of the loop indvar. Ignore
    if (expr == referenceExpr) {
      continue;
    }
    // Check if the current expression is temp symbol.
    if (auto arrSec = llvm::dyn_cast<ArraySection>(expr)) {
      for (auto tempSymbol : tempSymbolList) {
        if (arrSec->getSymbol() == tempSymbol) {
          isTempSymbol = true;
          break;
        }
      }
    }

    auto &exprRangeInfo = val.second;
    // Empty list. nothing required.
    if (exprRangeInfo.list.empty()) {
      continue;
    }

    auto &list = exprRangeInfo.list;
    if (ignoreDim == -1)
      assert(list.size() >= doStmtList.size());
    else if (isTempSymbol) {
      // assert(list.size() == doStmtList.size() - 1);
    }
    unsigned I = 0;
    unsigned J = 0;
    auto currDim = 1;
    // For every RangeInfo in the rangeList.
    for (; I < list.size(); ++I) {
      if (!list[I].isRange())
        continue;
      auto &rangeInfo = list[I];
      // Update the indvar for every loop nest now.
      assert(J < doStmtList.size());

      // If this is the temp symbol, ignore the dimension mentioned.
      if (isTempSymbol && ignoreDim == currDim) {
        J = J + 1;
      }

      auto currBlock = doStmtList[J]->getBlock();
      auto loc = rangeInfo.indvar->getSourceLoc();

      // Initialize the indvar in the parent loop/block.
      auto indVarAssign = builder.buildAssignmentStmt(
          rangeInfo.indvar, rangeInfo.bounds.first, loc);

      // If it is the topmost loop, add it to the
      // top stmt list.
      if (J == doStmtList.size() - 1) {
        topStmtList.push_back(indVarAssign);
      } else {
        auto parentBlock = doStmtList[J + 1]->getBlock();
        parentBlock->insertFront(indVarAssign);
      }

      // Now update the indvar in the current block.
      auto incr = builder.buildExpr(rangeInfo.indvar->clone(), rangeInfo.stride,
                                    BinaryOpKind::Addition,
                                    rangeInfo.indvar->getType(), loc);
      auto incrAssign =
          builder.buildAssignmentStmt(rangeInfo.indvar->clone(), incr, loc);

      currBlock->addStmt(incrAssign);
      J++;
      currDim++;
    }
  }
  return true;
}

Symbol *ArraySectionHelper::getTempArray(ArraySpec *spec, unsigned numDims,
                                         Type *elementTy,
                                         StmtVecList &newStmtList) {
  auto loc = spec->getSourceLoc();
  auto newArrayTy = ArrayType::get(C, elementTy, numDims);
  auto tempArr = currSymTable->getTempSymbol(newArrayTy, loc);

  SymbolAttributes attributes;
  attributes.isAllocatable = true;
  attributes.allocKind = StaticLocal;
  tempArr->setAttributes(attributes);

  SymbolList symList{tempArr};
  ArraySpecList specList{spec};
  auto allocateStmt = builder.buildAllocateStmt(symList, specList, loc);
  newStmtList.push_back(allocateStmt);
  return tempArr;
}

Symbol *ArraySectionHelper::getTempArray(ArraySection *referenceArray,
                                         Type *arrBaseEleTy,
                                         StmtVecList &newStmtList, int dimVal,
                                         bool isTranspose) {
  auto sym = referenceArray->getSymbol()->getOrigSymbol();
  auto arrTy = llvm::cast<ArrayType>(sym->getType());
  auto loc = referenceArray->getSourceLoc();
  if (arrBaseEleTy == nullptr) {
    arrBaseEleTy = arrTy->getElementTy();
  }
  auto numDims = arrTy->getNumDims();

  auto newArrDims = (dimVal == -1) ? numDims : numDims - 1;

  ArrayType *newArrayTy = nullptr;

  if (!arrTy->isDynArrayTy()) {
    ArrBoundsList list;
    list = arrTy->getBoundsList();
    if (isTranspose) {
      assert(list.size() == 2);
      std::reverse(list.begin(), list.end());
    }
    assert(list.size());
    if (dimVal != -1) {
      assert(list.size() > 1 && "Reducing to scalar value?");
      list.erase(&list[dimVal - 1]);
    }
    newArrayTy = ArrayType::get(C, arrBaseEleTy, list);
  } else {
    newArrayTy = ArrayType::get(C, arrBaseEleTy, newArrDims);
  }
  auto tempArr = currSymTable->getTempSymbol(newArrayTy, loc);

  if (arrTy->isDynArrayTy()) {
    SymbolAttributes attributes;
    attributes.isAllocatable = true;
    attributes.allocKind = StaticLocal;
    tempArr->setAttributes(attributes);

    // Now allocate the array.
    DynArrBoundsList list;

    for (auto I = 0; I < numDims; ++I) {
      if (I + 1 == dimVal)
        continue;
      auto lb = builder.buildLBoundIntrin(sym, I + 1, loc);
      auto ub = builder.buildUBoundIntrin(sym, I + 1, loc);
      list.push_back(std::make_pair(lb, ub));
    }

    if (isTranspose) {
      assert(list.size() == 2);
      std::reverse(list.begin(), list.end());
    }

    auto spec = builder.buildArraySpec(list, newArrDims, loc);

    SymbolList symList{tempArr};
    ArraySpecList specList{spec};
    auto allocateStmt = builder.buildAllocateStmt(symList, specList, loc);
    newStmtList.push_back(allocateStmt);
  }
  return tempArr;
}
