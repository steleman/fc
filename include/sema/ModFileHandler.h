
#ifndef FC_MOD_FILE_HANDLER_H
#define FC_MOD_FILE_HANDLER_H

#include "llvm/ADT/StringRef.h"

namespace fc {
class SymbolTable;
class ASTContext;

namespace ast {
class Module;
class ProgramUnit;
class UseStmt;
} // namespace ast

struct ModFileHandler {
  static bool dumpModFile(ast::Module *);

  static bool readModFile(llvm::StringRef name, ast::UseStmt *useStmt,
                          ast::ProgramUnit *PU, ASTContext &FC);
};

} // namespace fc

#endif
