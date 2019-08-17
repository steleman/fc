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
#ifndef FC_AST_COMMON_H
#define FC_AST_COMMON_H

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace fc {
class Symbol;
class SymbolTable;
struct SymbolAttributes;

enum IntentKind {
  Intent_None = 0,
  InOut, // Default if intent is specified
  In,
  Out,
};

enum AllocationKind {
  Alloc_None = 0,
  StaticLocal,  // in stack
  StaticGlobal, // in global space
  Argument,     // mostly, no allocation just getting the right argument.
};

enum LinkageKind {
  Link_None = 0,
  Link_Extern = 1,
  Link_Internal = 2,
};

enum ScopeKind {
  GlobalScope = 0,
  MainProgramScope,
  SubroutineScope,
  ModuleScope,
  FunctionScope,
};

typedef llvm::SmallVector<Symbol *, 2> SymbolList;
typedef llvm::SmallVector<SymbolTable *, 2> SymbolTableList;
typedef llvm::SmallPtrSet<SymbolTable *, 2> SymbolTableSet;
} // namespace fc
#endif