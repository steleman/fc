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
