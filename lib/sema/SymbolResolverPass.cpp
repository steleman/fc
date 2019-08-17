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

// Check for any undeclarared symbols which are not
// resolved and try it resolve it.
class SymbolResolverPass : public ASTPUPass {

private:
  FunctionType *undeclaredFunc;
  bool createUndeclaredFunction;
  ProgramUnit *currPU;
  std::map<std::string, SymbolTable *> usedSymTabs;

  bool checkSymbolTable(SymbolTable *symTable, Symbol *symbol) {
    auto usedSym = symTable->getSymbol(symbol->getName());

    while (usedSym) {
      auto parent = usedSym->getParentSymbol();
      if (parent) {
        usedSym = parent;
        continue;
      }
      break;
    }

    if (usedSym && !usedSym->getType()->isUndeclared()) {
      if (usedSym->getType() != undeclaredFunc) {
        symbol->setParentSymbol(usedSym);
        return true;
      }
    }
    return false;
  }

  bool checkUsedSymbolTables(ProgramUnit *PU, Symbol *sym) {
    for (auto symTable : PU->getUsedSymTables()) {
      if (checkSymbolTable(symTable, sym))
        return true;
    }
    return false;
  }

  // Recursively checks in parent symbols..
  bool checkInParentSymbolTable(Symbol *sym) {
    auto parent = currPU->getParent();

    while (parent) {
      // 1. First check in parent symbol table.
      auto parentSymbolTable = parent->getSymbolTable();
      if (checkSymbolTable(parentSymbolTable, sym))
        return true;

      // Inter-changed 1 and 2 for handling following case
      // Mod A
      //  integer v1
      // Mod B
      //  integer v2
      //
      // Mod C
      //  use A
      //  use B
      //
      // v1 = ..
      //
      // In this Mod C's usedSymbol list has sym table of A and B.
      // We can not use sym table of B to update type of v1 unless B's symbol
      // table has updated copy of v1 where v1's parent is A::v1.
      // 2. Then check in the parent's used table.
      if (checkUsedSymbolTables(parent, sym)) {
        return true;
      }

      parent = parent->getParent();
    }

    // 1. First check in global symbol table.
    auto currSymTable = currPU->getSymbolTable();
    while (!currSymTable->isGlobalScope())
      currSymTable = currSymTable->getParent();

    // 2. Then check in the parent's used table.
    if (checkSymbolTable(currSymTable, sym)) {
      return true;
    }

    return false;
  }

public:
  explicit SymbolResolverPass(ASTContext &C, bool createUndeclaredFunction)
      : ASTPUPass(C, "Symbol Resolver Pass"),
        createUndeclaredFunction(createUndeclaredFunction) {
    undeclaredFunc = FunctionType::getUndeclaredFuncTy(Context);
  }

  // Checks if any of the symbol tables in the current unit's scope
  // has definition for it.
  virtual bool runOnProgramUnit(ProgramUnit *PU) override {
    this->currPU = PU;

    for (auto sym : PU->getSymbolTable()->getSymbolList()) {
      if (!sym->getType()->isUndeclared())
        continue;

      auto parentSym = sym->getParentSymbol();
      // Found type.
      if (parentSym)
        continue;

      // 1. Now search in parent symbol table / parent's use table recursively
      // till global scope.
      if (checkInParentSymbolTable(sym))
        continue;

      // 2. Now look in used symbol tables to resolve the symbol.
      if (checkUsedSymbolTables(PU, sym))
        continue;

      // If the symbol is still not resolved. It should be an external function
      // reference, emit var arg.
      if (createUndeclaredFunction) {
        if (sym->getType()->isUndeclaredFnTy()) {
          auto funcTy = FunctionType::getUndeclaredFuncTy(Context);
          sym->setType(funcTy);
        }
      }
    }

    return true;
  }
}; // namespace fc

ASTPass *createSymbolResolverPass(ASTContext &C,
                                  bool createUndeclaredFunction) {
  return new SymbolResolverPass(C, createUndeclaredFunction);
}
} // namespace fc
