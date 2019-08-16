#include "AST/ASTContext.h"
#include "AST/ASTPass.h"

#include "AST/ParseTreeBuilder.h"
#include "AST/StmtVisitor.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"
#include "sema/Intrinsics.h"

using namespace fc;
using namespace ast;
namespace fc {

class StmtTypeUpdater : public StmtVisitor<StmtTypeUpdater, bool> {
  ParseTreeBuilder builder;
  ASTContext &C;

public:
  StmtTypeUpdater(ASTContext &C) : builder(C), C(C) {}

  bool updateTypeFromKind(EntityDecl *entityDecl) {
    auto decl = entityDecl->getDeclTypeSpec();

    if (!decl->getKind()) {
      return true;
    }

    auto kind = decl->getKind();

    auto kindVal = llvm::dyn_cast<ConstantVal>(kind);
    if (!kindVal)
      return true;

    assert(kindVal->getType() == Type::getInt32Ty(C));

    auto kindConst = kindVal->getInt();
    auto type = decl->getTypeSpec()->getBaseType();
    bool isInt = false;
    if (type->isIntegralTy()) {
      isInt = true;
    } else if (type->isFloatingTy()) {
      isInt = false;
    } else {
      llvm_unreachable("Unhandled type with kind");
    }

    Type *newType = nullptr;
    switch (kindConst) {
    case 2:
      assert(isInt);
      newType = Type::getInt16Ty(C);
      break;
    case 4:
      if (isInt)
        newType = Type::getInt32Ty(C);
      else
        newType = Type::getRealTy(C);
      break;
    case 8:
      if (isInt)
        newType = Type::getInt64Ty(C);
      else
        newType = Type::getDoubleTy(C);
      break;
    default:
      llvm_unreachable("Unhandled kind value");
    }

    assert(newType);
    decl->getTypeSpec()->setBaseType(newType);

    auto oldEntityType = entityDecl->getSymbol()->getType();

    assert(!oldEntityType->isUndeclaredTy() &&
           (!oldEntityType->isDerivedTy() || oldEntityType->isArrayTy()));

    Type *newEntityType = nullptr;
    if (oldEntityType->isArrayTy()) {
      auto arrayTy = llvm::dyn_cast<ArrayType>(oldEntityType);
      assert(arrayTy);

      if (arrayTy->isDynArrayTy()) {
        newEntityType = ArrayType::get(C, newType, arrayTy->getNumDims());
      } else {
        newEntityType = ArrayType::get(C, newType, arrayTy->getBoundsList());
      }
    } else {
      newEntityType = newType;
    }

    entityDecl->getSymbol()->setType(newEntityType);
    return true;
  }

  // Convert function reference to arrayElement.
  bool postFunctionReference(FunctionReference *ref) override {
    auto sym = ref->getSymbol();
    sym = sym->getOrigSymbol();

    Type *type = sym->getType();
    if (auto ptrType = llvm::dyn_cast<PointerType>(type))
      type = ptrType->getElementType();

    if (!type->isArrayTy()) {
      return true;
    }

    ExprList list;
    for (auto op : ref->getOperands()) {
      list.push_back(static_cast<Expr *>(op));
    }

    /* auto arrTy = static_cast<ArrayType *>(type); */
    /* assert(!arrTy->getElementTy()->isDerivedTy()); */

    bool isArraySection = false;
    for (auto expr : list) {
      if (llvm::dyn_cast<RangeExpr>(expr)) {
        isArraySection = true;
      }
    }

    auto base = builder.buildObjectName(sym, ref->getSourceLoc());

    Expr *arrEle;
    if (isArraySection)
      arrEle = builder.buildArraySection(ref->getSymbol()->getOrigSymbol(),
                                         base, list, ref->getSourceLoc());
    else
      arrEle = builder.buildArrayElement(ref->getSymbol(), base, list,
                                         ref->getSourceLoc());
    ref->replaceWith(arrEle);
    return visit(arrEle);
  }

  // Convert object name to array section.
  bool postObjectName(ObjectName *stmt) override {
    Symbol *sym = stmt->getSymbol()->getOrigSymbol();
    Type *type = sym->getType();
    if (auto ptrType = llvm::dyn_cast<PointerType>(type))
      type = ptrType->getElementType();

    if (!type->isArrayTy())
      return true;

    auto subsExpr = builder.buildRangeExpr(stmt->getSourceLoc());
    ExprList list;
    list.push_back(subsExpr);

    // auto type = sym->getType();
    // auto arrTy = static_cast<ArrayType *>(type);
    // assert(!arrTy->getElementTy()->isDerivedTy());
    auto base = builder.buildObjectName(sym, stmt->getSourceLoc());
    auto arrSec =
        builder.buildArraySection(sym, base, list, stmt->getSourceLoc());
    stmt->replaceWith(arrSec);
    return true;
  }

  long int getConstantValue(Expr *value, bool &found) {
    if (auto val = llvm::dyn_cast<ConstantVal>(value)) {
      found = true;
      return val->getInt();
    }
    if (auto objName = llvm::dyn_cast<ObjectName>(value)) {
      auto sym = objName->getSymbol()->getOrigSymbol();
      if (sym->getAttr().isConst) {
        auto constant = sym->getInitConstant();
        if (constant) {
          found = true;
          return constant->getInt();
        }
      }
    }
    found = false;
    return NULL;
  }

  // TODO: should be done in const prop.
  bool postEntityDecl(EntityDecl *decl) override {

    updateTypeFromKind(decl);
    auto symbol = decl->getSymbol();
    auto arrTy = llvm::dyn_cast<ArrayType>(symbol->getType());
    if (!arrTy || !arrTy->getBoundsList().empty())
      return true;

    if (arrTy->isStringArrTy()) {
      return true;
    }

    auto spec = decl->getArraySpec();

    ArrBoundsList boundsList;
    for (auto dynBounds : spec->getBoundsList()) {
      ArrayBounds bounds;
      bool found = false;
      bounds.first = getConstantValue(dynBounds.first, found);
      if (!found) {
        return true;
      }
      bounds.second = getConstantValue(dynBounds.second, found);
      if (!found) {
        return true;
      }
      boundsList.push_back(bounds);
    }
    if (!boundsList.empty())
      symbol->setType(ArrayType::get(C, arrTy->getElementTy(), boundsList));
    return true;
  }

  // Check if array section can be converted to arrayelement
  // Example, arr[1:1] -> arr[1]
  bool postArraySection(ArraySection *arrSec) override {
    auto subsList = arrSec->getSubscriptList();
    if (arrSec->isFullRange()) {
      return true;
    }

    bool isArrayEle = true;
    for (unsigned I = 0; I < subsList.size(); ++I) {
      auto rangeExpr = llvm::dyn_cast<RangeExpr>(subsList[I]);
      if (!rangeExpr) {
        continue;
      }
      auto lb = rangeExpr->getLowerBound();
      auto ub = rangeExpr->getUpperBound();
      if (!ub || !lb) {
        isArrayEle = false;
        continue;
      }
      auto index = rangeExpr->getIndex();
      if (index) {
        isArrayEle = false;
        continue;
      }
      auto lbVal = llvm::dyn_cast<ConstantVal>(lb);
      auto ubVal = llvm::dyn_cast<ConstantVal>(ub);
      if (!lbVal || !ubVal || (lbVal->getInt() != ubVal->getInt())) {
        isArrayEle = false;
        continue;
      }
      subsList[I] = lb;
      rangeExpr->replaceWith(lb);
      isArrayEle &= true;
    }
    if (isArrayEle) {
      auto arrEle =
          builder.buildArrayElement(arrSec->getSymbol(), arrSec->getBaseExpr(),
                                    subsList, arrSec->getSourceLoc());
      arrSec->replaceWith(arrEle);
    }
    return true;
  }
};
// A pass to update the stmt types after resolving all the symbols from
// the symbol resolver pass.
class StmtTypeUpdaterPass : public ASTBlockPass {
  StmtTypeUpdater stmtVisitor;

public:
  StmtTypeUpdaterPass(ASTContext &C)
      : ASTBlockPass(C, "Statement Type Updater Pass"), stmtVisitor(C) {}

  bool runOnBlock(Block *block) override {
    for (auto stmt : *block) {
      if (!stmtVisitor.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
};

ASTPass *createStmtTypeUpdaterPass(ASTContext &C) {
  return new StmtTypeUpdaterPass(C);
}
} // namespace fc
