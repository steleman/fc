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
