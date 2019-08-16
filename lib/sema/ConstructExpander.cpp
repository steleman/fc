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

static void populateArraysInExpr(Expr *expr, SymbolList &list,
                                 ExprList &exprList,
                                 bool allowElement = false) {
  if (!expr)
    return;

  if (auto arraySec = llvm::dyn_cast<ArraySection>(expr)) {
    assert(arraySec);
  }
  if (expr->getOperands().size() != 2)
    return;
  auto lhs = static_cast<Expr *>(expr->getOperand(0));
  auto rhs = static_cast<Expr *>(expr->getOperand(1));

  if (auto arraySec = llvm::dyn_cast<ArraySection>(lhs)) {
    list.push_back(arraySec->getSymbol()->getOrigSymbol());
    exprList.push_back(lhs);
  } else {
    populateArraysInExpr(lhs, list, exprList);
  }

  if (auto arraySec = llvm::dyn_cast<ArraySection>(rhs)) {
    list.push_back(arraySec->getSymbol()->getOrigSymbol());
    exprList.push_back(rhs);
  } else {
    populateArraysInExpr(rhs, list, exprList);
  }

  if (allowElement) {
    if (auto arrayEle = llvm::dyn_cast<ArrayElement>(lhs)) {
      list.push_back(arrayEle->getSymbol()->getOrigSymbol());
      exprList.push_back(lhs);
    } else {
      populateArraysInExpr(lhs, list, exprList);
    }

    if (auto arrayEle = llvm::dyn_cast<ArrayElement>(rhs)) {
      list.push_back(arrayEle->getSymbol()->getOrigSymbol());
      exprList.push_back(rhs);
    } else {
      populateArraysInExpr(rhs, list, exprList);
    }
  }
  return;
}

bool allArraySecFullRange(ExprList &exprList) {

  for (auto expr : exprList) {
    ArraySection *arraySec = llvm::dyn_cast<ArraySection>(expr);
    if (!arraySec->isFullRange())
      return false;
  }
  return true;
}

static bool isSameDimension(SymbolList list) {
  Symbol *firstSym = list[0];
  auto firstTy = firstSym->getType();
  for (unsigned i = 1; i < list.size(); ++i) {
    if (firstTy != list[i]->getType())
      return false;
  }
  return true;
}

class ConstructExpander : public StmtVisitor<ConstructExpander, bool> {
  ParseTreeBuilder builder;
  Block *currBlock{nullptr};
  SymbolTable *currSymTable{nullptr};
  Stmt *currStmt{nullptr};
  ASTContext &C;

public:
  ConstructExpander(ASTContext &C) : builder(C), C(C) {}

  void setBlock(Block *b) { this->currBlock = b; }

  void setSymTable(SymbolTable *sym) { this->currSymTable = sym; }

  void setCurrStmt(Stmt *curr) { currStmt = curr; }

  void
  populateArrayElementsInExpr(Expr *expr,
                              llvm::SmallVector<ArrayElement *, 2> &elements) {
    if (auto arrayEle = llvm::dyn_cast<ArrayElement>(expr)) {
      elements.push_back(arrayEle);
      return;
    }

    if (auto binaryExpr = llvm::dyn_cast<BinaryExpr>(expr)) {
      populateArrayElementsInExpr(binaryExpr->getLHS(), elements);
      populateArrayElementsInExpr(binaryExpr->getRHS(), elements);
      return;
    }

    if (auto funcRef = llvm::dyn_cast<FunctionReference>(expr)) {
      for (auto arg : funcRef->getArgsList()) {
        populateArrayElementsInExpr(arg, elements);
      }
      return;
    }
  }

  void populateBoundsList(RangeInfoList &infoList, ExprList &exprList) {
    assert(!exprList.empty());
    ArraySection *firstArraySec = llvm::dyn_cast<ArraySection>(exprList[0]);
    assert(firstArraySec);
    if (firstArraySec->isFullRange()) {
      assert(allArraySecFullRange(exprList) &&
             "Combination of full range and partial range is not handled");
    }

    ArraySectionHelper arrySecHelper(builder, currSymTable, C);
    arrySecHelper.computeArrSecRangeInfo(firstArraySec, infoList);
    return;
  }

  Block *replaceAllExprInBlock(Block *block, ArraySectionList &arraySections,
                               SymbolList indVarList, SourceLoc loc) {
    StmtList stmtList;
    ArraySectionHelper arrySecHelper(builder, currSymTable, C);

    for (auto arraySec : arraySections) {
      RangeInfoList infoList;
      RangeInfoList rhsInfoList;
      ExprList subsList;
      int j = 0;

      arrySecHelper.computeArrSecRangeInfo(arraySec, infoList, false);
      for (auto info : infoList) {
        if (info.isRange()) {
          subsList.push_back(builder.buildObjectName(indVarList[j++], loc));
        } else
          subsList.push_back(info.bounds.first);
      }
      auto sym = arraySec->getSymbol();
      auto type = sym->getType();
      auto arrTy = static_cast<ArrayType *>(type);
      assert(!arrTy->getElementTy()->isDerivedTy());
      auto base = builder.buildObjectName(sym, sym->getSourceLoc());
      auto arrEle =
          builder.buildArrayElement(arraySec->getSymbol(), base, subsList, loc);
      arraySec->replaceWith(arrEle);
    }
    return block;
  }

  class ArrSecVisitor : public StmtVisitor<ArrSecVisitor, bool> {
  public:
    ArraySectionList arraySectionList;
    bool visitArraySection(ArraySection *section) override {
      arraySectionList.push_back(section);
      return true;
    }
  };

  bool populateArraysInBlock(Block *block, ArraySectionList &arraySections) {

    ArrSecVisitor visitor;
    visitor.arraySectionList.clear();
    for (auto stmt : block->getStmtList()) {
      if (!visitor.visit(stmt))
        return false;
    }

    arraySections.append(visitor.arraySectionList.begin(),
                         visitor.arraySectionList.end());
    return true;
  }

  bool postWhereConstruct(WhereConstruct *construct) override {
    auto numOperands = construct->getOperands().size();
    SymbolList symbolList;
    ExprList exprList;
    ArraySectionList arraySections;

    // Populate all the arraysections used in this wherestmt along with their
    // corresponding symbols
    for (int i = 0; i < numOperands; ++i) {
      auto whereStmt = construct->getWhereStmt(i);
      auto maskEpr = whereStmt->getMaskExpr();
      populateArraysInExpr(maskEpr, symbolList, exprList);
      if (!populateArraysInBlock(whereStmt->getBlock(), arraySections))
        return false;
    }

    assert(isSameDimension(symbolList) &&
           "Expecting same dimension for all symbols");

    auto sym = symbolList[0];
    assert(sym->getType()->isArrayTy());
    auto arrTy = llvm::cast<ArrayType>(sym->getType());

    // Dynamic arrays are not handled yet.
    if (arrTy->getElementTy()->isStringCharTy()) {
      llvm_unreachable("Not handled");
      return true;
    }

    auto loc = construct->getSourceLoc();

    // Get the bounds list.
    RangeInfoList infoList;
    DynArrBoundsList boundsList;
    ExprList strideList;
    SymbolList indVarList;

    populateBoundsList(infoList, exprList);

    for (auto info : infoList) {
      if (info.isRange()) {
        boundsList.push_back(info.bounds);
        strideList.push_back(info.stride);
        indVarList.push_back(info.indvar->getSymbol());
      }
    }

    auto numDims = boundsList.size();

    auto loopNest =
        builder.buildLoopNestFor(indVarList, boundsList, strideList, loc);
    auto innermostBlock = loopNest[numDims - 1]->getBlock();
    assert(innermostBlock);

    ArraySectionHelper arrySecHelper(builder, currSymTable, C);

    assert(exprList.size() == symbolList.size());

    // Replace all the captured arraysections with array elements
    for (unsigned i = 0; i < exprList.size(); ++i) {
      auto type = symbolList[i]->getType();
      auto arrTy = static_cast<ArrayType *>(type);
      assert(!arrTy->getElementTy()->isDerivedTy());

      auto arraySec = llvm::dyn_cast<ArraySection>(exprList[i]);
      RangeInfoList infoList;
      arrySecHelper.computeArrSecRangeInfo(arraySec, infoList, false);
      ExprList subsList;

      int j = 0;

      for (auto info : infoList) {
        if (info.isRange()) {
          subsList.push_back(builder.buildObjectName(indVarList[j++], loc));
        } else
          subsList.push_back(info.bounds.first);
      }

      auto base =
          builder.buildObjectName(symbolList[i], symbolList[i]->getSourceLoc());
      auto arrEle =
          builder.buildArrayElement(symbolList[i], base, subsList, loc);

      exprList[i]->replaceWith(arrEle);
    }

    IfStmtList ifList;
    IfConstructKindList kindList;

    // Build an IfConstruct inside loop
    for (int i = 0; i < numOperands; ++i) {
      auto whereStmt = construct->getWhereStmt(i);
      auto maskExpr = whereStmt->getMaskExpr();
      auto block = whereStmt->getBlock();
      auto newBlock =
          replaceAllExprInBlock(block, arraySections, indVarList, loc);
      ifList.push_back(builder.buildIfStmt(maskExpr, newBlock, loc));
      if (construct->getKind(i) == WhereElseConstructKind::WhereKind) {
        kindList.push_back(IfConstructKind::IfThenKind);
      } else if (construct->getKind(i) ==
                 WhereElseConstructKind::ElseWhereWhereKind) {
        kindList.push_back(IfConstructKind::ElseIfKind);
      } else if (construct->getKind(i) ==
                 WhereElseConstructKind::ElseWhereKind) {
        kindList.push_back(IfConstructKind::ElseKind);
      } else {
        llvm_unreachable("Undefined where kind");
      }
    }

    auto ifElseStmt = builder.buildIfElseStmt(ifList, kindList, loc);
    innermostBlock->addStmt(ifElseStmt);
    currBlock->replaceWith(loopNest[0], construct);
    return true;
  }

  bool postSelectCaseStmt(SelectCaseStmt *select) override {
    SourceLoc loc = select->getSourceLoc();
    Expr *selectExpr = select->getSelectExpr();

    IfStmtList ifList;
    IfConstructKindList kindList;

    auto numOperands = select->getNumOperands();

    if (numOperands == 1) {
      return true;
    }

    bool first = true;
    for (int i = 1; i < numOperands; ++i) {
      auto caseStmt = select->getCaseStmt(i);
      auto caseLoc = caseStmt->getSourceLoc();
      Expr *newSelectExpr = selectExpr->clone();
      Expr *lastExpr = nullptr;
      for (auto expr : caseStmt->getExprList()) {

        Expr *currExpr = nullptr;

        if (auto rangeExpr = llvm::dyn_cast<RangeExpr>(expr)) {
          auto lb = rangeExpr->getLowerBound();
          auto ub = rangeExpr->getUpperBound();
          auto index = rangeExpr->getIndex();
          assert(index == nullptr && "Index not handled yet.");

          currExpr = builder.buildRelationalExpr(newSelectExpr, lb,
                                                 RelationalOpKind::GE, caseLoc);
          auto newExpr = builder.buildRelationalExpr(
              newSelectExpr, ub, RelationalOpKind::LE, caseLoc);
          currExpr = builder.buildLogicalExpr(currExpr, newExpr,
                                              LogicalOpKind::AND, caseLoc);

        } else {
          currExpr = builder.buildRelationalExpr(newSelectExpr, expr,
                                                 RelationalOpKind::EQ, caseLoc);
        }
        if (!lastExpr) {
          lastExpr = currExpr;
        } else {
          lastExpr = builder.buildLogicalExpr(lastExpr, currExpr,
                                              LogicalOpKind::OR, caseLoc);
        }
      }

      ifList.push_back(
          builder.buildIfStmt(lastExpr, caseStmt->getBlock(), caseLoc));

      if (first) {
        kindList.push_back(IfConstructKind::IfThenKind);
        first = false;
        continue;
      }

      if (!lastExpr) {
        kindList.push_back(IfConstructKind::ElseKind);
        continue;
      }

      kindList.push_back(IfConstructKind::ElseIfKind);
    }

    auto ifElseStmt = builder.buildIfElseStmt(ifList, kindList, loc);
    currBlock->replaceWith(ifElseStmt, select);
    return true;
  }

  bool postForAllStmt(ForAllStmt *stmt) override {
    auto loc = stmt->getSourceLoc();
    ExprList argList = stmt->getExprList();
    unsigned size = argList.size();
    assert(size >= 2);
    Expr *condition = argList[size - 1];
    assert(condition);

    DynArrBoundsList boundsList;
    ExprList strideList;
    auto one = builder.getConstantOne(loc, Type::getInt32Ty(C));
    std::map<std::string, int> NameIndexMap;
    SymbolList indVarList;

    // Collect all bounds
    for (unsigned i = 0; i < size - 1; ++i) {

      auto *expr = static_cast<AssignmentExpr *>(argList[i]);

      RangeExpr *rangeExpr = llvm::dyn_cast<RangeExpr>(expr->getExpr());

      boundsList.push_back(std::make_pair(rangeExpr->getLowerBound(),
                                          rangeExpr->getUpperBound()));

      indVarList.push_back(currSymTable->getOrigSymbol(expr->getName()));
      // TODO : Derive from quad expression
      strideList.push_back(one);
    }

    unsigned numDims = strideList.size();
    auto loopNest =
        builder.buildLoopNestFor(indVarList, boundsList, strideList, loc);

    auto innermostBlock = loopNest[numDims - 1]->getBlock();
    assert(innermostBlock);

    IfStmtList ifList;
    IfConstructKindList kindList;
    ifList.push_back(builder.buildIfStmt(condition, stmt->getBlock(), loc));
    kindList.push_back(IfConstructKind::IfThenKind);
    auto ifElseStmt = builder.buildIfElseStmt(ifList, kindList, loc);
    innermostBlock->addStmt(ifElseStmt);
    currBlock->replaceWith(loopNest[0], stmt);
    return true;
  }

  // TODO currently emiting loops, which prints each element in a line
  //      Change this to print as per format
  bool postIOImpliedDo(IOImpliedDo *impliedDo) override {
    auto loc = impliedDo->getSourceLoc();
    auto parentStmt = impliedDo->getParent();
    auto writeStmt = llvm::dyn_cast<WriteStmt>(parentStmt);
    assert(writeStmt);
    ExprList exprs = impliedDo->getExprList();
    ExprList exprList = impliedDo->getImpliedDos();
    unsigned numDims = exprList.size();

    DynArrBoundsList boundsList;
    ExprList strideList;

    for (auto expr : exprList) {
      auto quadExpr = llvm::dyn_cast<QuadExpr>(expr);
      assert(quadExpr);
      strideList.push_back(quadExpr->getOperand(3));
      boundsList.push_back(
          std::make_pair(quadExpr->getOperand(1), quadExpr->getOperand(2)));
    }

    // Create and get n temporaries from the curr PU symbol table.
    auto indVarList =
        currSymTable->getTempSymbols(numDims, Type::getInt32Ty(C), loc);

    auto loopNest =
        builder.buildLoopNestFor(indVarList, boundsList, strideList, loc);

    auto innermostBlock = loopNest[numDims - 1]->getBlock();
    assert(innermostBlock);

    ExprList printExprs;
    for (auto expr : exprs) {
      if (auto oldElement = llvm::dyn_cast<ArrayElement>(expr)) {
        assert(oldElement);
        ExprList subsList;
        for (unsigned I = 0; I < numDims; ++I) {
          subsList.push_back(builder.buildObjectName(indVarList[I], loc));
        }

        auto arraySym = oldElement->getSymbol();
        auto base = builder.buildObjectName(arraySym, loc);
        auto arrEle = builder.buildArrayElement(arraySym, base, subsList, loc);
        printExprs.push_back(arrEle);
      } else {
        llvm::SmallVector<ArrayElement *, 2> elements;
        populateArrayElementsInExpr(expr, elements);
        for (auto oldElement : elements) {
          ExprList subsList;
          for (unsigned I = 0; I < numDims; ++I) {
            subsList.push_back(builder.buildObjectName(indVarList[I], loc));
          }

          auto arraySym = oldElement->getSymbol();
          auto base = builder.buildObjectName(arraySym, loc);
          auto arrEle =
              builder.buildArrayElement(arraySym, base, subsList, loc);
          oldElement->replaceWith(arrEle);
        }
        printExprs.push_back(expr);
      }
    }

    // Collect all the required expressions
    Expr *unit = writeStmt->getUnit();
    Format *format = writeStmt->getFormat();
    ConstantVal *advance = writeStmt->getAdvance();
    Expr *iostat = writeStmt->getIostat();
    auto newWrite =
        builder.buildWriteStmt(printExprs, loc, unit, format, advance, iostat);

    // Collect any other expressions other than implieddo remaining in writestmt
    ExprList remainingExprs;
    for (auto expr : writeStmt->getExprList()) {
      if (impliedDo == expr)
        continue;
      remainingExprs.push_back(expr);
    }

    innermostBlock->addStmt(newWrite);

    if (!remainingExprs.empty()) {
      Expr *newUnit = unit ? unit->clone() : nullptr;

      // adavance is a constant, can be reused
      auto newWrite = builder.buildWriteStmt(remainingExprs, loc, newUnit,
                                             format, advance, iostat);

      // TODO Insert before and after should be decided based on the
      // position of implieddo
      currBlock->insertStmtAfter(newWrite, parentStmt);
    }
    currBlock->replaceWith(loopNest[0], parentStmt);
    return false;
  }

  bool postArrayConstructor(ArrayConstructor *arrCon) override {
    auto loc = arrCon->getSourceLoc();
    auto acSpec = arrCon->getSpec();

    if (auto entity = llvm::dyn_cast<EntityDecl>(currStmt)) {

      // TODO : Currently emitting assignemts inside first executable block.
      //        Is this right way to do this?
      //        Other way to deal with this is emit one long string consiting of
      //        all const strings with appropriate paddings!

      auto sym = entity->getSymbol();
      switch (sym->getSymTable()->getScopeKind()) {
      case GlobalScope:
      case ModuleScope:
        return true;
      default:
        break;
      }

      auto arrayTy = llvm::dyn_cast<ArrayType>(sym->getType());
      assert(arrayTy);

      // Currently only handling string char types
      if (!arrayTy->getElementTy()->isStringCharTy())
        return true;

      if (arrayTy->isDynArrayTy())
        return true;

      auto bounds = arrayTy->getBoundsList();

      // Only hanlding 2d arrays
      assert(bounds.size() == 2);

      auto size = bounds[0].first = bounds[0].second;
      assert(size == acSpec->getNumOperands());

      StmtVecList stmtList;

      auto arrayObject = builder.buildObjectName(sym, loc);
      for (unsigned i = 0; i < size; ++i) {
        auto index = builder.buildConstantVal(std::to_string(i + 1),
                                              Type::getInt32Ty(C), loc);
        ExprList exprList{index};
        auto arrEle =
            builder.buildArrayElement(sym, arrayObject->clone(), exprList, loc);

        auto assignStmt =
            builder.buildAssignmentStmt(arrEle, acSpec->getValue(i), loc);
        stmtList.push_back(assignStmt);
      }

      auto firstBlock =
          currSymTable->getProgramUnit()->getExecPart()->getBlock();
      for (auto stmt : stmtList) {
        firstBlock->insertFront(stmt);
      }
      arrCon->eraseFromParent();
      return true;
    }
    auto &stmtList = arrCon->getSpec()->getOperands();

    // Implied DO's are not handled here yet.
    if (arrCon->getSpec()->getNumOperands() == 1) {
      if (llvm::isa<QuadExpr>(stmtList[0])) {
        return true;
      }
    }

    ExprList list;
    for (auto stmt : stmtList) {
      auto expr = static_cast<Expr *>(stmt);
      // TODO: QuadExpr is not handled right now.
      if (llvm::isa<QuadExpr>(expr))
        return true;
      list.push_back(expr);
    }

    assert(list.size());

    // Create a temp array of size of expression list
    ArraySectionHelper helper(builder, currSymTable, C);
    ArrBoundsList arrBoundsList{std::make_pair(1, list.size())};
    auto arrTy = ArrayType::get(C, list[0]->getType(), arrBoundsList);
    auto tempArr = currSymTable->getTempSymbol(arrTy, loc);

    StmtVecList newStmtList;
    // Now store those values in the array.
    auto arrObj = builder.buildObjectName(tempArr, loc);
    for (unsigned I = 0; I < list.size(); ++I) {

      auto index = builder.buildConstantVal(std::to_string(I + 1),
                                            Type::getInt32Ty(C), loc);
      ExprList exprList{index};
      auto arrEle =
          builder.buildArrayElement(tempArr, arrObj->clone(), exprList, loc);

      auto assignStmt = builder.buildAssignmentStmt(arrEle, list[I], loc);
      newStmtList.push_back(assignStmt);
    }

    for (auto newStmt : newStmtList) {
      currBlock->insertStmtBefore(newStmt, currStmt);
    }

    arrCon->replaceWith(builder.buildArraySection(tempArr, arrObj, loc));
    return true;
  }

  bool postFunctionReference(FunctionReference *ref) override {
    auto sym = ref->getSymbol()->getOrigSymbol();
    auto loc = ref->getSourceLoc();
    auto FTy = llvm::cast<FunctionType>(sym->getType());
    if (!FTy->getReturnType()->isArrayTy()) {
      return true;
    }
    auto arrTy = static_cast<FunctionType *>(FTy->getReturnType());
    if (arrTy->isDynArrayTy()) {
      return true;
    }
    // Create the temporary symbol with the same type and assign
    // the function reference value.
    auto tempSym = currSymTable->getTempSymbol(arrTy, loc);

    auto arrSec = builder.buildArraySection(
        tempSym, builder.buildObjectName(tempSym, loc), loc);

    auto assignStmt = builder.buildAssignmentStmt(arrSec, ref->clone(), loc);
    currBlock->insertStmtBefore(assignStmt, currStmt);

    ref->replaceWith(arrSec->clone());
    return true;
  }
}; // namespace fc

// Pass to expand where construct to loop
// TODO Handle all cases
class ConstructExpanderPass : public ASTBlockPass {
  ConstructExpander expander;

public:
  ConstructExpanderPass(ASTContext &C)
      : ASTBlockPass(C, "Where Expander pass"), expander(C) {}

  bool runOnBlock(Block *block) override {
    expander.setBlock(block);
    expander.setSymTable(currPU->getSymbolTable());

    auto &stmtList = block->getStmtList();

    for (auto stmt : stmtList) {
      expander.setCurrStmt(stmt);
      if (!expander.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
};

ASTPass *createConstructExpanderPass(ASTContext &C) {
  return new ConstructExpanderPass(C);
}
} // namespace fc
