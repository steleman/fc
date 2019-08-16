#ifndef FC_CG_DEBUG_INFO_H
#define FC_CG_DEBUG_INFO_H

#include "AST/ParserTreeCommon.h"
#include "codegen/CGASTHelper.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"

namespace fc {
class CGDebugInfo {

private:
  llvm::DIBuilder *builder;
  CGASTHelper *cgHelper;
  llvm::DICompileUnit *currCU;
  llvm::SmallVector<llvm::DIScope *, 2> ScopeStack;

public:
  llvm::DICompileUnit *getCurrCU() { return currCU; }

  void pushScope(llvm::DIScope *scope) { ScopeStack.push_back(scope); }

  void popScope() { ScopeStack.pop_back(); }

  llvm::DIScope *getCurrScope() { return ScopeStack.back(); }

  CGDebugInfo(llvm::DIBuilder *builder, CGASTHelper *cgHelper,
              llvm::DICompileUnit *currCU)
      : builder(builder), cgHelper(cgHelper), currCU(currCU) {}
  llvm::DIType *getDIType(llvm::Type *type);

  llvm::DIType *getDIType(fc::Type *type);

  llvm::DISubprogram *getSubProgram(llvm::Function *func, SourceLoc loc);

  llvm::DILocalVariable *getLocalVariable(llvm::Value *arg, fc::Symbol *sym,
                                          int ArgNum, llvm::BasicBlock *BB);

  llvm::DebugLoc getLoc(SourceLoc loc, llvm::DIScope *scope = nullptr);
};

} // namespace fc

#endif
