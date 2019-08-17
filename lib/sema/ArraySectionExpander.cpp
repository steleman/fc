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
#include "AST/StmtVisitor.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"
#include "sema/ExpansionUtils.h"
#include "sema/Intrinsics.h"

using namespace fc;
using namespace ast;
namespace fc {

static bool isResultArray(FunctionReference *ref) {
  auto intrinKind = intrin::getIntrinsicKind(ref->getSymbol()->getName());

  // Currently only handling sqrt!
  if (intrinKind != intrin::sqrt)
    return true;

  class ArgVisitor : public StmtVisitor<ArgVisitor, bool> {
  public:
    bool arrayFound{false};
    bool postArraySection(ArraySection *sec) {
      arrayFound = true;
      return true;
    }

    bool postObjectName(ObjectName *objName) {
      if (llvm::isa<ArrayType>(objName->getType()))
        arrayFound = true;
      return true;
    }
  };

  assert(ref->getArgsList().size() == 1);
  auto arg = ref->getExpr(0);

  ArgVisitor visitor;
  visitor.visit(arg);

  if (visitor.arrayFound)
    return true;
  return false;
}

// TODO should be an utility function
static void markLoopsAsParallel(llvm::SmallVector<DoStmt *, 2> doStmtList) {
  for (auto stmt : doStmtList) {
    stmt->markAsParallel();
  }
}

class ArraySecExpander : public StmtVisitor<ArraySecExpander, bool> {
  ParseTreeBuilder builder;
  Block *currBlock{nullptr};
  SymbolTable *currSymTable{nullptr};
  ProgramUnit *currPU{nullptr};
  ASTContext &C;

  bool handleArrayConstructorInit(Stmt *stmt, ArraySection *arrSec,
                                  ArrayConstructor *arrConst) {

    auto acSpec = arrConst->getSpec();
    auto loc = arrSec->getSourceLoc();
    // Only full range is handled for now.
    assert(arrSec->isFullRange());

    auto sym = arrSec->getSymbol()->getOrigSymbol();
    auto arrTy = llvm::cast<ArrayType>(sym->getType());

    // Handle only single dimension for now.
    assert(arrTy->getNumDims() == 1);

    // Only simple expressions in acspec are handled for now.
    for (unsigned I = 0; I < acSpec->getNumOperands(); ++I) {
      auto expr = static_cast<Expr *>(acSpec->getOperand(I));
      if (!expr->isDesignator())
        return false;
    }

    // Emit series of stores for each of the values.
    StmtVecList list;

    auto indexSym = currSymTable->getTempSymbol(Type::getInt32Ty(C), loc);
    // Assign with the lower bound of the array.
    auto lbound = builder.buildLBoundIntrin(sym, 1, loc);
    auto initValStmt = builder.buildAssignmentStmt(
        builder.buildObjectName(indexSym, loc), lbound, loc);
    list.push_back(initValStmt);

    // for each of the value in acSpec, emit the store.
    for (unsigned I = 0; I < acSpec->getNumOperands(); ++I) {
      auto expr = static_cast<Expr *>(acSpec->getOperand(I));
      assert(expr->isDesignator());

      if (I != 0) {
        // increment the index object by one.
        auto add =
            builder.buildExpr(builder.buildObjectName(indexSym, loc),
                              builder.getConstantOne(loc, indexSym->getType()),
                              BinaryOpKind::Addition, indexSym->getType(), loc);
        auto store = builder.buildAssignmentStmt(
            builder.buildObjectName(indexSym, loc), add, loc);
        list.push_back(store);
      }

      ExprList subsList;
      subsList.push_back(builder.buildObjectName(indexSym, loc));

      auto arrEle = builder.buildArrayElement(
          sym, builder.buildObjectName(sym, loc), subsList, loc);
      auto assignStmt = builder.buildAssignmentStmt(arrEle, expr, loc);
      list.push_back(assignStmt);
      // Construct array store for this.
    }

    for (auto newStmt : list) {
      currBlock->insertStmtBefore(newStmt, stmt);
      newStmt->setParentNode(stmt->getParentNode());
    }
    return true;
  }

public:
  ArraySecExpander(ASTContext &C) : builder(C), C(C) {}

  void setBlock(Block *b) { this->currBlock = b; }

  void setSymTable(SymbolTable *sym) { this->currSymTable = sym; }

  void setCurrPU(ProgramUnit *PU) { this->currPU = PU; }

  bool areSameExpr(Expr *expr1, Expr *expr2) {
    assert(expr1 && expr2);

    if (expr1 == expr2)
      return true;

    auto const1 = llvm::dyn_cast<ConstantVal>(expr1);
    auto const2 = llvm::dyn_cast<ConstantVal>(expr2);

    if (const1 && const2) {
      if (const1->getValue() == const2->getValue()) {
        return true;
      }
    }

    auto obj1 = llvm::dyn_cast<ObjectName>(expr1);
    auto obj2 = llvm::dyn_cast<ObjectName>(expr2);

    if (obj1 && obj2) {
      if (obj1->getName() == obj2->getName())
        return true;
    }
    return false;
  }

  bool postArraySection(ArraySection *section) override {
    // If an array section can be reduced to array element, reduce it.

    if (section->isFullRange())
      return true;

    auto subsList = section->getSubscriptList();
    ExprList newSubs;

    for (auto sub : subsList) {
      if (!sub)
        return true;
      auto rangeExpr = llvm::dyn_cast<RangeExpr>(sub);
      if (!rangeExpr) {
        return true;
      }

      if (rangeExpr->isFullRange())
        return true;

      /*
       * FIXME: Comment this currently
       * Uncommnting this might expose a bug!
      if (rangeExpr->getIndex()) {
        newSubs.push_back(rangeExpr->getIndex()->clone());
        continue;
      }
      */

      if (!areSameExpr(rangeExpr->getLowerBound(), rangeExpr->getUpperBound()))
        return true;
      newSubs.push_back(rangeExpr->getLowerBound()->clone());
    }

    auto arrayTy = llvm::dyn_cast<ArrayType>(section->getType());
    assert(arrayTy);

    assert(arrayTy->getNumDims() == newSubs.size());
    auto sym = section->getSymbol();

    auto loc = section->getSourceLoc();
    auto base = builder.buildObjectName(sym, loc);
    auto arrayElement = builder.buildArrayElement(sym, base, newSubs, loc);
    section->replaceWith(arrayElement);
    return true;
  }

  bool postAssignmentStmt(AssignmentStmt *stmt) override {
    auto lhs = stmt->getLHS();
    auto rhs = stmt->getRHS();
    ArraySectionHelper arrySecHelper(builder, currSymTable, C);
    ArraySection *arrSec = nullptr;

    // In case of struct-comp, we progress iff it has an ArraySection.
    if (auto structComp = llvm::dyn_cast<StructureComponent>(lhs)) {
      if ((arrSec = structComp->getArraySection())) {
        lhs = arrSec;
      } else {
        return true;
      }
    } else {
      arrSec = llvm::dyn_cast<ArraySection>(lhs);
    }

    if (!arrSec) {
      return true;
    }

    class ArrSecVisitor : public StmtVisitor<ArrSecVisitor, bool> {
    public:
      bool foundIntrinisc{false};
      FunctionReference *funcRef{nullptr};
      bool postFunctionReference(FunctionReference *ref) {
        foundIntrinisc = intrin::isIntrinsic(ref->getSymbol()->getName());
        if (foundIntrinisc)
          funcRef = ref;
        return true;
      }
    };

    ArrSecVisitor visitor;
    visitor.visit(rhs);
    // Ignore function reference in RHS
    // For example a = foo()  ! where a an array
    if (llvm::isa<FunctionReference>(rhs)) {
      return true;
    }

    if (visitor.foundIntrinisc && isResultArray(visitor.funcRef))
      return true;

    // For where statements.
    // TODO: remove once they are blocks.
    if (!llvm::isa<Block>(stmt->getParent())) {
      return false;
    }

    auto sym = arrSec->getSymbol()->getOrigSymbol();
    assert(sym->getType()->isArrayTy());

    auto arrTy = llvm::cast<ArrayType>(sym->getType());
    // Do not worry about strings.
    if (arrTy->getElementTy()->isStringCharTy()) {
      auto rhsArraySec = llvm::dyn_cast<ArraySection>(rhs);
      if (!rhsArraySec)
        return true;
      if (rhsArraySec->isFullRange() && arrSec->isFullRange())
        return true;
    }

    if (auto arrConst = llvm::dyn_cast<ArrayConstructor>(rhs)) {
      if (arrConst->getSpec()->getNumOperands() != 1) {
        auto val = handleArrayConstructorInit(stmt, arrSec, arrConst);
        if (val) {
          currBlock->removeStmt(stmt);
        }
        return true;
      }
    }

    // Only full range is handled for now.
    auto SecSubsList = arrSec->getSubscriptList();

    DynArrBoundsList loopBoundsList;
    SymbolList indVarList;
    ExprList strideList;

    // Compute the range info for the arrsection and other
    // similar types.
    // TODO: Only Designators are handled for rhs. Need to
    // handle Binary,Logical expressions, etc.

    ExprRangeInfoMap map;
    bool hasRange = false;
    arrySecHelper.collectRangeInfoListFor(lhs, map, hasRange);
    arrySecHelper.collectRangeInfoListFor(rhs, map, hasRange);

    auto lhsInfoList = map[lhs].list;

    llvm::SmallVector<DoStmt *, 2> doStmtList;
    arrySecHelper.getLoopNestFor(lhsInfoList, doStmtList);
    assert(doStmtList.size());

    // Top level stmt list to add in place of current statement.
    StmtVecList topLevelStmtList;
    arrySecHelper.insertOtherRangeInfo(map, lhs, doStmtList, topLevelStmtList);

    // Now insert the top-level do loop to top level stmt list.
    topLevelStmtList.push_back(doStmtList.back());

    // Now insert the assignment statement in the innermost loop.
    // and change all the expression values to scalar.
    auto innerBlock = doStmtList[0]->getBlock();
    // insert the current statement.
    innerBlock->insertFront(stmt);

    // Replace all the expressions in statements with the scalar ones.
    for (auto &val : map) {
      auto expr = val.first;
      auto finalExpr = val.second.finalExpr;
      expr->replaceWith(finalExpr);
    }

    // Place all the new instructions in the curr block.
    for (auto newStmt : topLevelStmtList) {
      currBlock->insertStmtBefore(newStmt, stmt);
    }
    currBlock->removeStmt(stmt);
    markLoopsAsParallel(doStmtList);
    return true;
  }

  bool setArrayConstantInit(Symbol *arrSym, EntityDecl *entity) {
    auto init = entity->getInit();
    assert(init);
    auto validInit = init->getStmtType() == StmtType::ArrayConstructorKind;
    assert(validInit);
    assert(arrSym->getType()->isArrayTy());

    auto arrTy = llvm::cast<ArrayType>(arrSym->getType());

    auto arrCon = llvm::cast<ArrayConstructor>(init);

    llvm::SmallVector<std::string, 2> ConstantValList;

    for (auto op : arrCon->getSpec()->getOperands()) {
      if (auto constVal = llvm::dyn_cast<ConstantVal>(op)) {
        std::string val = constVal->getValue();
        ConstantValList.push_back(val);
      } else {
        error() << "\n Array Initialization failed!";
        return false;
      }
    }

    auto newConstant = Constant::Create(ConstantValList, arrTy);
    auto newConstVal =
        builder.buildConstantVal(newConstant, entity->getSourceLoc());
    init->replaceWith(newConstVal);
    return true;
  }
  bool visitEntityDecl(EntityDecl *stmt) override {
    auto arrSym = stmt->getSymbol()->getOrigSymbol();
    auto rhs = stmt->getInit();
    if (!rhs)
      return true;
    auto loc = stmt->getSourceLoc();

    auto validRHS = rhs->getStmtType() == StmtType::ArrayConstructorKind;

    if (!validRHS || !arrSym) {
      return true;
    }

    assert(arrSym->getType()->isArrayTy());

    auto arrTy = llvm::cast<ArrayType>(arrSym->getType());
    // Do not worry about strings.
    if (arrTy->getElementTy()->isStringCharTy()) {
      return true;
    }

    if (arrSym->getAllocKind() == AllocationKind::StaticGlobal) {
      return setArrayConstantInit(arrSym, stmt);
    }

    auto execPart = currPU->getExecPart();
    assert(execPart);
    auto execBlock = execPart->getBlock();
    currBlock = execBlock;
    auto insertPt = execBlock->getStmtList().front();
    if (auto arrConst = llvm::dyn_cast<ArrayConstructor>(rhs)) {
      if (arrConst->getSpec()->getNumOperands() != 1) {
        auto arrSec = builder.buildArraySection(
            arrSym, builder.buildObjectName(arrSym, loc), loc);
        auto val = handleArrayConstructorInit(insertPt, arrSec, arrConst);
        if (val) {
          stmt->setInit(nullptr);
        }
        return true;
      }
    }

    return true;
  }
}; // namespace fc

// A pass to replace all the function calls to array element.
// Note that SymbolResolver pass would have changed the unresolved
// types and we would not resolve arrays.
// TODO: It is un-necessary to run this pass. optimize.
class ArraySecExpanderPass : public ASTBlockPass {
  ArraySecExpander expander;

public:
  ArraySecExpanderPass(ASTContext &C)
      : ASTBlockPass(C, "Array section Expander Pass"), expander(C) {}

  bool runOnBlock(Block *block) override {
    expander.setBlock(block);
    expander.setSymTable(currPU->getSymbolTable());
    expander.setCurrPU(currPU);

    auto stmtList = block->getStmtList();

    for (auto stmt : stmtList) {
      if (!expander.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
}; // namespace fc

ASTPass *createArraySecExpanderPass(ASTContext &C) {
  return new ArraySecExpanderPass(C);
}
} // namespace fc
