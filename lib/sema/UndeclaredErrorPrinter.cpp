#include "AST/ASTContext.h"
#include "AST/ASTPass.h"
#include "AST/ProgramUnit.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"

using namespace fc;
using namespace ast;
namespace fc {

// Check for any undeclarared symbols which are not
// resolved and throw error.
class UndeclaredErrorPrinter : public ASTPUPass {
public:
  explicit UndeclaredErrorPrinter(ASTContext &C)
      : ASTPUPass(C, "Sema Undeclared Error Printer") {}
  virtual bool runOnProgramUnit(ProgramUnit *PU) {
    bool success = true;

    for (auto sym : PU->getSymbolTable()->getOrigSymbolList()) {
      if (sym->getType()->isUndeclaredTy()) {
        Context.Diag.printError(sym->getSourceLoc(), diag::undeclared_variable);
        success = false;
      }
    }
    return success;
  }
};

ASTPass *createUndeclaredErrorPrinter(ASTContext &C) {
  return new UndeclaredErrorPrinter(C);
}
} // namespace fc
