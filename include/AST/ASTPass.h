#ifndef FC_AST_PASS
#define FC_AST_PASS

#include "llvm/ADT/StringRef.h"
namespace fc {
class ASTContext;
class SymbolTable;

namespace ast {

class ProgramUnit;
class Module;
class ParseTree;
class Block;
class ParseTreeBuilder;

class ASTPass {
public:
  enum PassKind {
    ProgramPassKind,     // Run on whole program
    ProgramUnitPassKind, // Run on all the program units.
    ModulePassKind,      // Run on Modules/Sub only.
    BlockPassKind,
  };

  PassKind getPassKind() const { return kind; }

  virtual ~ASTPass() {}

protected:
  ASTContext &Context;
  friend class ASTPassManager;

  explicit ASTPass(ASTContext &Context, PassKind kind, llvm::StringRef name)
      : Context(Context), kind(kind), name(name) {}
  ASTPass(const ASTPass &) = delete;

  llvm::StringRef getName() const { return name; }

private:
  PassKind kind;
  llvm::StringRef name;
};

// This recursively traverses all the internal subprograms.
class ASTPUPass : public ASTPass {
public:
  ASTPUPass(ASTContext &Context, llvm::StringRef name)
      : ASTPass(Context, ProgramUnitPassKind, name) {}
  virtual bool runOnProgramUnit(ProgramUnit *) = 0;
  virtual ~ASTPUPass() {}
};

class ASTProgramPass : public ASTPass {
public:
  ASTProgramPass(ASTContext &Context, llvm::StringRef name)
      : ASTPass(Context, ProgramPassKind, name) {}
  virtual bool runOnProgram(ParseTree *parseTree) = 0;
  virtual ~ASTProgramPass() {}
};

class ASTModulePass : public ASTPass {
public:
  ASTModulePass(ASTContext &Context, llvm::StringRef name)
      : ASTPass(Context, ModulePassKind, name) {}
  virtual bool runOnModule(Module *) = 0;
  virtual ~ASTModulePass() {}
};

// Runs on all the blocks in all the program units.
class ASTBlockPass : public ASTPass {
protected:
  ProgramUnit *currPU;

public:
  void setProgramUnit(ProgramUnit *PU) { currPU = PU; }

  ASTBlockPass(ASTContext &Context, llvm::StringRef name)
      : ASTPass(Context, BlockPassKind, name) {}

  virtual bool runOnBlock(Block *) = 0;

  virtual ~ASTBlockPass() {}
};

class ASTPassManager {
  std::unique_ptr<ParseTree> &parseTree;
  std::unique_ptr<ParseTreeBuilder> builder;
  llvm::SmallVector<ASTPass *, 2> passList;
  llvm::StringRef name;

public:
  explicit ASTPassManager(std::unique_ptr<ParseTree> &parseTree,
                          llvm::StringRef name);

  void addPass(ASTPass *pass);

  bool runOnProgramUnit(ASTPUPass *pass);

  bool runOnProgramUnit(ASTBlockPass *pass);

  bool runOnModule(ASTModulePass *pass);

  llvm::StringRef getName() { return name; }

  ASTContext &getContext();

  bool run();

  ~ASTPassManager();
};
} // namespace ast
} // namespace fc
#endif