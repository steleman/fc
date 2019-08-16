#include "AST/ASTContext.h"
#include "AST/ASTPass.h"

#include "AST/ParseTreeBuilder.h"
#include "AST/ProgramUnit.h"
#include "AST/StmtVisitor.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"
#include "sema/Intrinsics.h"

using namespace fc;
using namespace ast;
namespace fc {

class ExprTypeUpdater : public StmtVisitor<ExprTypeUpdater, bool> {
  ParseTreeBuilder builder;
  Block *currBlock{nullptr};
  SymbolTable *currSymTable{nullptr};
  Stmt *currStmt{nullptr};

  Type *getFinalType(Type *type1, Type *type2) {
    auto id1 = type1->getTypeID();
    auto id2 = type2->getTypeID();

    if (id1 == id2) {
      return type1;
    }

    // Floating vs integer, always return floating.
    if (type1->isIntegralTy() && type2->isFloatingTy()) {
      return type2;
    }
    if (type1->isFloatingTy() && type2->isIntegralTy()) {
      return type1;
    }

    if ((type1->isIntegralTy() && type2->isIntegralTy()) ||
        (type1->isFloatingTy() && type2->isFloatingTy())) {
      return type1->getSizeInBits() > type2->getSizeInBits() ? type1 : type2;
    }
    llvm_unreachable("\nUnhandled cast case!");
  }

  Expr *castExprToType(Expr *old, Type *toType) {
    // If, this is the constant, just replace the constant value to
    // new type.
    Expr *newValue = nullptr;
    SourceLoc loc = old->getSourceLoc();
    auto oldType = old->getType();
    if (oldType->isArrayTy()) {
      oldType = ((ArrayType *)(old))->getElementTy();
    }
    if (oldType == toType) {
      return old;
    }
    auto constVal = llvm::dyn_cast<ConstantVal>(old);
    auto oldTypeSize = oldType->getSizeInBits();
    auto toTypeSize = toType->getSizeInBits();
    if (constVal && oldTypeSize <= toTypeSize) {
      newValue = builder.buildConstantVal(constVal->getValue(), toType, loc);
      old->replaceWith(newValue);
    } else {
      // If this is not a constant, then create a cast node.
      auto parent = old->getParentNode();
      newValue = builder.buildCastExpr(old, toType, loc);
      newValue->setParentNode(parent, true);
    }
    return newValue;
  }

public:
  // Match the type of two expressions.
  bool updateTypeForExpr(Expr *expr1, Expr *expr2) {
    if (expr1->getType() == expr2->getType()) {
      return false;
    }

    auto type1 = expr1->getType();
    auto type2 = expr2->getType();

    if (type1->isArrayTy()) {
      type1 = static_cast<ArrayType *>(type1)->getElementTy();
    }
    if (type2->isArrayTy()) {
      type2 = static_cast<ArrayType *>(type2)->getElementTy();
    }

    if (type1 == type2) {
      return false;
    }

    if (!type1->isNumericalTy() || !type2->isNumericalTy()) {
      return false;
    }

    auto getTypeAndValueToChange = [this, expr1, expr2, type1, type2]() {
      auto finalTy = getFinalType(type1, type2);
      if (finalTy == type1) {
        return std::make_tuple(expr2, finalTy);
      }
      return std::make_tuple(expr1, finalTy);
    };

    Expr *finalVal;
    Type *finalTy;
    std::tie(finalVal, finalTy) = getTypeAndValueToChange();
    Expr *newValue = castExprToType(finalVal, finalTy);
    assert(newValue);
    return true;
  }

  void setBlock(Block *block) { currBlock = block; }

  void setStmt(Stmt *stmt) { currStmt = stmt; }

  void setSymbolTable(SymbolTable *symTable) { currSymTable = symTable; }

  ExprTypeUpdater(ASTContext &C) : builder(C) {}

  bool handleAssignment(Type *lhsTy, Expr *rhs) {
    if (lhsTy->isArrayTy()) {
      lhsTy = static_cast<ArrayType *>(lhsTy)->getElementTy();
    }

    auto rhsTy = rhs->getType();
    if (rhsTy->isArrayTy()) {
      rhsTy = static_cast<ArrayType *>(rhsTy)->getElementTy();
    }

    if (rhsTy == lhsTy) {
      return true;
    }
    if (!lhsTy->isNumericalTy() || !rhsTy->isNumericalTy()) {
      return true;
    }

    auto finalTy = getFinalType(rhsTy, lhsTy);
    // Unknown conversion
    if (finalTy != lhsTy) {
      return true;
    }

    castExprToType(rhs, lhsTy);
    return true;
  }

  bool postAssignmentStmt(AssignmentStmt *stmt) override {
    auto lhs = stmt->getLHS();
    auto rhs = stmt->getRHS();
    return handleAssignment(lhs->getType(), rhs);
  }

  bool postBinaryExpr(BinaryExpr *expr) override {

    auto lhs = expr->getLHS();
    auto rhs = expr->getRHS();
    updateTypeForExpr(lhs, rhs);
    return true;
  }

  bool postRelationalExpr(RelationalExpr *expr) override {
    auto lhs = expr->getLHS();
    auto rhs = expr->getRHS();

    updateTypeForExpr(lhs, rhs);
    return true;
  }

  bool postLogicalExpr(LogicalExpr *expr) override {
    auto lhs = expr->getLHS();
    auto rhs = expr->getRHS();
    // Might be unary operation.
    if (!lhs) {
      return true;
    }
    updateTypeForExpr(lhs, rhs);
    return true;
  }

  // Update the rhs types to adjust to the lhs.
  bool postEntityDecl(EntityDecl *stmt) override {

    // Handle constant array inits for now.
    auto arrSym = stmt->getSymbol();
    auto init = stmt->getInit();
    if (!init)
      return true;

    if (auto arrTy = llvm::dyn_cast<ArrayType>(arrSym->getType())) {
      // Handle Arrayconstructor init.
      if (auto arrCon = llvm::dyn_cast<ArrayConstructor>(init)) {
        auto acSpec = arrCon->getSpec();
        if (acSpec->getNumOperands() == 1) {
          return true;
        }
        auto baseTy = arrTy->getElementTy();
        if (!baseTy->isStringCharTy()) {
          for (auto arg : acSpec->getOperands()) {
            auto constVal = llvm::dyn_cast<ConstantVal>(arg);
            if (!constVal) {
              continue;
            }
            if (constVal->getType() == baseTy)
              continue;

            assert(!constVal->getType()->isDerivedTy() &&
                   !baseTy->isDerivedTy());
            auto newConstVal = builder.buildConstantVal(
                constVal->getValue(), baseTy, constVal->getSourceLoc());
            constVal->replaceWith(newConstVal);
          }
        }
        return true;
      }
    }
    return handleAssignment(arrSym->getType(), init);
  }
}; // namespace fc

class ExprTypeUpdaterPass : public ASTBlockPass {
  ExprTypeUpdater exprTypeUpdater;

public:
public:
  ExprTypeUpdaterPass(ASTContext &C)
      : ASTBlockPass(C, "Expr Type Updater Pass"), exprTypeUpdater(C) {}

  bool runOnBlock(Block *block) override {
    exprTypeUpdater.setBlock(block);
    exprTypeUpdater.setSymbolTable(currPU->getSymbolTable());

    auto stmtList = block->getStmtList();
    for (auto stmt : stmtList) {
      exprTypeUpdater.setStmt(stmt);
      if (!exprTypeUpdater.visit(stmt)) {
        return false;
      }
    }
    return true;
  }

}; // namespace fc

ASTPass *createExprTypeUpdaterPass(ASTContext &C) {
  return new ExprTypeUpdaterPass(C);
}
} // namespace fc
