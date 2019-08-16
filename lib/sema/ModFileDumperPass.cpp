#include "AST/ASTContext.h"
#include "AST/ASTPass.h"
#include "AST/Declaration.h"
#include "AST/ProgramUnit.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Debug.h"
#include "common/Diagnostics.h"
#include "sema/ModFileHandler.h"

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringRef.h"

using namespace fc;
using namespace ast;
namespace fc {

class ModFileDumper : public ASTProgramPass {

  bool processModule(Module *module) {
    return ModFileHandler::dumpModFile(module);
  }

public:
  virtual bool runOnProgram(ParseTree *parseTree) override {

    // Dump the remaining modules which are not procesed yet.
    for (auto PU : parseTree->getProgramUnitList()) {
      if (!PU->isModule())
        continue;

      auto module = static_cast<Module *>(PU);
      if (!processModule(module)) {
        return false;
      }
    }
    return true;
  }
  explicit ModFileDumper(ASTContext &C)
      : ASTProgramPass(C, "Use Stmt Handler Pass") {}
};

ASTPass *createModFileDumperPass(ASTContext &C) { return new ModFileDumper(C); }

} // namespace fc
