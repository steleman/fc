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
#include "AST/Expressions.h"
#include "AST/ParseTreeBuilder.h"
#include "AST/StmtVisitor.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"
#include "sema/Intrinsics.h"

using namespace fc;
using namespace ast;
namespace fc {

class ConstProp : public StmtVisitor<ConstProp, bool> {
private:
  ParseTreeBuilder builder;
  ASTContext &Context;

public:
  void updateSymbolInit(Expr *expr, ConstantVal *NewConst) {
    EntityDecl *parentDecl;
    if ((parentDecl = llvm::dyn_cast<EntityDecl>(expr->getParent()))) {
      parentDecl->getSymbol()->getOrigSymbol()->setInitConstant(
          NewConst->getConstant());
    }
  }

  ConstantVal *getReducedParamObj(ObjectName *obj) {
    Symbol *sym = obj->getSymbol()->getOrigSymbol();
    Constant *initConstant = nullptr;

    if ((sym->isConstant()) && (initConstant = sym->getInitConstant()))
      return builder.buildConstantVal(initConstant->getValue(),
                                      initConstant->getType(),
                                      obj->getSourceLoc());
    else
      return nullptr;
  }

  void reduceParamExpr(Expr *expr) {
    if (!expr)
      return;

    if (ObjectName *obj = llvm::dyn_cast<ObjectName>(expr)) {
      if (ConstantVal *initConst = getReducedParamObj(obj)) {
        expr->replaceWith(initConst);
        updateSymbolInit(expr, initConst);
      }
      return;
    }

    for (Stmt *operand : expr->getOperands()) {
      // FIXME: I stumbled upon test cases that had operands as nullptr.
      if (!operand)
        continue;

      if (ObjectName *obj = llvm::dyn_cast<ObjectName>(operand)) {
        if (ConstantVal *initConst = getReducedParamObj(obj)) {
          operand->replaceWith(initConst);
        }
      }
    }
  }

  ConstProp(ASTContext &C) : builder(C), Context(C) {}

  template <typename T>
  bool evaluateRelationalExpr(RelationalOpKind OpKind, T lhsVal, T rhsVal) {
    switch (OpKind) {
    case RelationalOpKind::EQ:
      return lhsVal == rhsVal;
    case RelationalOpKind::NE:
      return lhsVal != rhsVal;
    case RelationalOpKind::LT:
      return lhsVal < rhsVal;
    case RelationalOpKind::LE:
      return lhsVal <= rhsVal;
    case RelationalOpKind::GT:
      return lhsVal > rhsVal;
    case RelationalOpKind::GE:
      return lhsVal >= rhsVal;
    };

    llvm_unreachable("unknown relation op");
  }

  bool postRelationalExpr(RelationalExpr *relationalExpr) {
    reduceParamExpr(relationalExpr);

    auto lhs = relationalExpr->getLHS();
    auto rhs = relationalExpr->getRHS();

    assert(lhs && rhs);
    ConstantVal *lhsConst = llvm::dyn_cast<ConstantVal>(lhs);
    ConstantVal *rhsConst = llvm::dyn_cast<ConstantVal>(rhs);

    if (!lhsConst || !rhsConst)
      return true;

    bool result = false;

    if (lhsConst->getType()->isIntegralTy() ||
        rhsConst->getType()->isIntegralTy()) {
      result = evaluateRelationalExpr<long>(
          relationalExpr->getOpKind(), lhsConst->getInt(), rhsConst->getInt());

    } else if (lhsConst->getType()->isFloatingTy() ||
               rhsConst->getType()->isFloatingTy()) {
      result = evaluateRelationalExpr<double>(relationalExpr->getOpKind(),
                                              lhsConst->getFloat(),
                                              rhsConst->getFloat());

    } else {
      return true;
    }

    auto NewConst = builder.buildConstantVal(result ? ".true." : ".false.",
                                             Type::getLogicalTy(Context),
                                             relationalExpr->getSourceLoc());

    updateSymbolInit(relationalExpr, NewConst);
    relationalExpr->replaceWith(NewConst);

    return true;
  }

  bool postLogicalExpr(LogicalExpr *logicalExpr) {
    reduceParamExpr(logicalExpr);

    auto lhs = logicalExpr->getLHS();
    auto rhs = logicalExpr->getRHS();
    bool lhsVal, rhsVal;

    lhsVal = rhsVal = false;

    assert(rhs);
    auto rhsConst = llvm::dyn_cast<ConstantVal>(rhs);
    if (rhsConst)
      rhsVal = rhsConst->getBool();
    else
      return true;

    ConstantVal *lhsConst = nullptr;
    if (lhs) {
      lhsConst = llvm::dyn_cast<ConstantVal>(lhs);
      if (lhsConst)
        lhsVal = lhsConst->getBool();
      else
        return true;
    } else {
      assert(logicalExpr->getOpKind() == LogicalOpKind::NOT);
    }

    bool result = false;
    switch (logicalExpr->getOpKind()) {
    case LogicalOpKind::AND:
      result = lhsVal & rhsVal;
      break;
    case LogicalOpKind::OR:
      result = lhsVal | rhsVal;
      break;
    case LogicalOpKind::EQV:
      result = !(lhsVal ^ rhsVal);
      break;
    case LogicalOpKind::NEQV:
      result = lhsVal ^ rhsVal;
      break;
    case LogicalOpKind::NOT:
      result = !rhsVal;
      break;
    default:
      llvm_unreachable("unknown binary op");
    }

    auto NewConst =
        builder.buildConstantVal(result ? ".true." : ".false.", rhs->getType(),
                                 logicalExpr->getSourceLoc());

    updateSymbolInit(logicalExpr, NewConst);
    logicalExpr->replaceWith(NewConst);
    return true;
  }

  template <typename T>
  static T evaluateArithExpr(BinaryOpKind opKind, T lhsVal, T rhsVal,
                             bool isUnary) {
    T result = lhsVal;
    switch (opKind) {
    case BinaryOpKind::Addition:
      result += rhsVal;
      break;
    case BinaryOpKind::Subtraction:
      if (isUnary)
        result = -rhsVal;
      else
        result -= rhsVal;
      break;
    case BinaryOpKind::Multiplication:
      result *= rhsVal;
      break;
    case BinaryOpKind::Division:
      result /= rhsVal;
      break;
    case BinaryOpKind::Power:
      result = pow(result, rhsVal);
      break;
    case BinaryOpKind::Concat:
      result += rhsVal;
    }
    return result;
  }

  bool postBinaryExpr(BinaryExpr *binaryExpr) {
    reduceParamExpr(binaryExpr);

    auto lhs = binaryExpr->getLHS();
    auto rhs = binaryExpr->getRHS();
    assert(rhs);

    long int rhsVal, lhsVal;
    lhsVal = rhsVal = 0;

    auto rhsConst = llvm::dyn_cast<ConstantVal>(rhs);
    if (!rhsConst)
      return true;

    ConstantVal *lhsConst = nullptr;
    if (lhs) {
      lhsConst = llvm::dyn_cast<ConstantVal>(lhs);
      if (!lhsConst)
        return true;
    } else {
      assert(binaryExpr->getOpKind() == BinaryOpKind::Subtraction);
    }

    ConstantVal *NewConst;
    if (lhsConst->getType()->isIntegralTy() &&
        rhsConst->getType()->isIntegralTy()) {
      long result = evaluateArithExpr<long>(
          binaryExpr->getOpKind(), lhsConst->getInt(), rhsConst->getInt(),
          /* isUnary = */ !lhsConst);

      NewConst = builder.buildConstantVal(
          std::to_string(result), rhs->getType(), binaryExpr->getSourceLoc());

    } else if (lhsConst->getType()->isFloatingTy() &&
               rhsConst->getType()->isFloatingTy()) {
      double result = evaluateArithExpr<double>(
          binaryExpr->getOpKind(), lhsConst->getFloat(), rhsConst->getFloat(),
          /* isUnary = */ !lhsConst);

      NewConst = builder.buildConstantVal(
          std::to_string(result), rhs->getType(), binaryExpr->getSourceLoc());

    } else {
      return true;
    }

    updateSymbolInit(binaryExpr, NewConst);
    binaryExpr->replaceWith(NewConst);

    return true;
  }

  bool postObjectName(ObjectName *obj) {
    reduceParamExpr(obj);
    return true;
  }

  // TODO: move all intrinsic evaluation at initialization to a common file that
  // can be used by const-prop or related passes.
  bool postFunctionReference(FunctionReference *func) {

    // FIXME: The evaluated results here doesn't conform with gfortran results.
    // Need to find a proper documentation that details the algorithm for
    // evaluating this. For now, it works as gfortran for fotonik cases.
    if (intrin::getIntrinsicKind(func->getSymbol()->getName()) ==
        intrin::selected_real_kind) {
      ExprList argList = func->getArgsList();
      assert(argList.size() == 1 || argList.size() == 2);

      ConstantVal *precision = llvm::dyn_cast<ConstantVal>(argList[0]);
      assert(precision);
      Constant *precisionConst = precision->getConstant();

      assert(precisionConst->getType()->isIntegralTy());
      ConstantVal *exponentRange = nullptr;
      Constant *exponentRangeConst = nullptr;

      if (argList.size() == 2) {
        exponentRange = llvm::dyn_cast<ConstantVal>(argList[1]);
        assert(exponentRange);
        exponentRangeConst = exponentRange->getConstant();
        assert(exponentRangeConst->getType()->isIntegralTy());
      }

      unsigned kind = 0;
      if (exponentRangeConst) {
        long exponentRangeValue = exponentRangeConst->getInt();
        if (exponentRangeValue <= 37)
          kind = 4;
        else
          kind = 8;

      } else {
        long precisionVal = precisionConst->getInt();
        if (precisionVal <= 6)
          kind = 4;
        else
          kind = 8;
      }

      ConstantVal *NewConst = builder.buildConstantVal(
          std::to_string(kind), Type::getInt32Ty(Context),
          func->getSourceLoc());

      updateSymbolInit(func, NewConst);
      func->replaceWith(NewConst);

    } else if (intrin::getIntrinsicKind(func->getSymbol()->getName()) ==
               intrin::selected_int_kind) {
      ExprList argList = func->getArgsList();
      assert(argList.size() == 1);

      ConstantVal *exponent = llvm::dyn_cast<ConstantVal>(argList[0]);
      assert(exponent);
      Constant *exponentConst = exponent->getConstant();
      assert(exponentConst->getType()->isIntegralTy());
      long exponentVal = exponentConst->getInt();
      unsigned kind = 0;

      assert(exponentVal > 0 && exponentVal < 19);
      if (exponentVal <= 2) {
        kind = 1;
      } else if (exponentVal <= 4) {
        kind = 2;
      } else if (exponentVal <= 9) {
        kind = 4;
      } else {
        kind = 8;
      }
      // TODO: exponentVal >= 19, ie support kind = 16

      ConstantVal *NewConst = builder.buildConstantVal(
          std::to_string(kind), Type::getInt32Ty(Context),
          func->getSourceLoc());

      updateSymbolInit(func, NewConst);
      func->replaceWith(NewConst);
    }

    return true;
  }
};

class ConstPropPass : public ASTBlockPass {
  ConstProp visitor;

public:
  explicit ConstPropPass(ASTContext &C)
      : ASTBlockPass(C, "Constant propagation pass"), visitor(C) {}

  bool runOnBlock(Block *block) override {
    for (auto stmt : block->getStmtList()) {
      if (!visitor.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
}; // namespace fc

ASTPass *createConstPropPass(ASTContext &C) { return new ConstPropPass(C); }
} // namespace fc
