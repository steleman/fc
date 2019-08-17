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