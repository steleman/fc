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

class ParamConstProp : public StmtVisitor<ParamConstProp, bool> {
  ParseTreeBuilder builder;

public:
  ParamConstProp(ASTContext &C) : builder(C) {}

  bool postObjectName(ObjectName *obj) override {
    auto sym = obj->getSymbol()->getOrigSymbol();
    if (!sym->isConstant()) {
      return true;
    }
    auto constant = sym->getInitConstant();
    if (!constant) {
      // error() << "\n Constant value not found for symbol :" <<
      // sym->getName();
      return true;
    }
    auto constVal = builder.buildConstantVal(
        constant->getValue(), sym->getType(), obj->getSourceLoc());
    obj->replaceWith(constVal);
    return true;
  }
};

class ParamConstPropPass : public ASTBlockPass {
  ParamConstProp stmtVisitor;

public:
  explicit ParamConstPropPass(ASTContext &C)
      : ASTBlockPass(C, "Parameter value updater pass"), stmtVisitor(C) {}

  virtual bool runOnBlock(Block *block) override {

    for (auto stmt : *block) {
      if (!stmtVisitor.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
}; // namespace fc

ASTPass *createParamConstPropPass(ASTContext &C) {
  return new ParamConstPropPass(C);
}
} // namespace fc
