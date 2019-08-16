#include "AST/ASTContext.h"
#include "AST/ASTPass.h"
#include "AST/ProgramUnit.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"

using namespace fc;
using namespace ast;
namespace fc {

class FunctionTypeUpdaterPass : public ASTPUPass {
public:
  explicit FunctionTypeUpdaterPass(ASTContext &C)
      : ASTPUPass(C, "Function Type Updater Pass") {}

  virtual bool runOnProgramUnit(ProgramUnit *PU) {

    // After resolving the symbols, now update the function type of the current
    // program unit in its parent symbol table.
    if (!PU->isSubroutine() && !PU->isFunction()) {
      return true;
    }

    auto currSymTable = PU->getSymbolTable();
    auto parentSymTable = currSymTable->getParent();
    auto sym = parentSymTable->getSymbol(PU->getName());
    assert(sym);

    // Check each of the arguments and update the new type.
    auto Ty = sym->getType();
    auto funcTy = llvm::cast<FunctionType>(Ty);
    ArgsList argList;
    argList = static_cast<Function *>(PU)->getArgsList();

    TypeList typeList = funcTy->getArgList();
    bool hasChange = false;
    for (unsigned I = 0; I < argList.size(); ++I) {
      auto argSym = currSymTable->getSymbol(argList[I]);
      if (argSym->getType() != typeList[I]) {
        typeList[I] = argSym->getType();
        hasChange = true;
        continue;
      }
    }

    auto returnTy = funcTy->getReturnType();
    if (PU->isFunction()) {
      auto returnSym = currSymTable->getSymbol(PU->getName());
      if (returnSym) {
        if (returnTy != returnSym->getType()) {
          returnTy = returnSym->getType();
          auto func = static_cast<Function *>(PU);
          func->setReturnType(returnTy);
        }
      }
    }

    if (!hasChange)
      return true;

    // Update the function type.
    auto newFuncTy =
        FunctionType::get(Context, funcTy->getReturnType(), typeList);
    sym->setType(newFuncTy);
    return true;
  }
}; // namespace fc

ASTPass *createFunctionTypeUpdaterPass(ASTContext &C) {
  return new FunctionTypeUpdaterPass(C);
}
} // namespace fc
