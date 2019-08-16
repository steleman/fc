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

class ArrBoundsIntrinExpander
    : public StmtVisitor<ArrBoundsIntrinExpander, bool> {
  ParseTreeBuilder builder;

public:
  ArrBoundsIntrinExpander(ASTContext &C) : builder(C) {}

  bool postFunctionReference(FunctionReference *funcRef) override {
    auto sym = funcRef->getSymbol();
    sym = sym->getOrigSymbol();

    auto funcName = sym->getName();
    enum IntrinType {
      LB,
      UB,
      SizeVal,
    };
    IntrinType intrinType;
    if (funcName == "lbound") {
      intrinType = LB;
    } else if (funcName == "ubound") {
      intrinType = UB;
    } else if (funcName == "size") {
      intrinType = SizeVal;
      if (funcRef->getNumOperands() != 2)
        return true;
    } else {
      return true;
    }

    // Get the array type. If it is static replace it with the static value.
    auto arrSec = llvm::cast<ArraySection>(funcRef->getExpr(0));
    auto arrSym = arrSec->getSymbol()->getOrigSymbol();
    auto arrTy = llvm::cast<ArrayType>(arrSym->getType());

    // Dynamic bounds are not yet handled.
    if (arrTy->boundsEmpty()) {
      return true;
    }

    // Get the dimension number. It should be a constant value.
    auto dimVal = llvm::cast<ConstantVal>(funcRef->getExpr(1));
    long dim = dimVal->getInt();

    auto &boundsList = arrTy->getBoundsList();
    auto &bound = boundsList[dim - 1];

    int boundVal;
    switch (intrinType) {
    case LB:
      boundVal = bound.first;
      break;
    case UB:
      boundVal = bound.second;
      break;
    case SizeVal:
      boundVal = bound.second - bound.first + 1;
      break;
    };
    auto boundConstVal = builder.buildConstantVal(
        std::to_string(boundVal), dimVal->getType(), funcRef->getSourceLoc());

    funcRef->replaceWith(boundConstVal);
    return true;
  }
};

// Pass to replace all lbound/ubounds intrinsics for the static size array
// with the corresponding constant values.
class ArrBoundsIntrinExpanderPass : public ASTBlockPass {
  ArrBoundsIntrinExpander expander;

public:
  ArrBoundsIntrinExpanderPass(ASTContext &C)
      : ASTBlockPass(C, "Function Reference resolver"), expander(C) {}

  bool runOnBlock(Block *block) override {
    for (auto stmt : *block) {
      if (!expander.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
};

ASTPass *createArrBoundsIntrinExpanderPass(ASTContext &C) {
  return new ArrBoundsIntrinExpanderPass(C);
}
} // namespace fc
