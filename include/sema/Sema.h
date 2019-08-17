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
#ifndef FC_SEMA_H
#define FC_SEMA_H

#include "AST/ParserTreeCommon.h"
#include "common/Diagnostics.h"

namespace fc {
class ASTContext;
class TypeUpdaterPass;

namespace ast {
class ASTPass;
}

using namespace ast;

class Sema {
  std::unique_ptr<ParseTree> &parseTree;

public:
  explicit Sema(std::unique_ptr<ParseTree> &parseTree) : parseTree(parseTree) {}
  bool run();
};

// Pass to update the various types in symbol table
// like array types, intent arguments, etc.
ASTPass *createTypeUpdaterPass(ASTContext &C, bool onlyUpdateSpec = false);

// Pass to adjust the Expression types. Implicit type conversions, etc.
ASTPass *createExprTypeUpdaterPass(ASTContext &C);

// Pass to resolve variables, function calls, module accesses, etc.
ASTPass *createSymbolResolverPass(ASTContext &C,
                                  bool createUndeclaredFunction = true);

// Pass to evaluate and propagate all the constants expressions.
ASTPass *createConstPropPass(ASTContext &C);

// Pass to print the error for unresolved symbols.
ASTPass *createUndeclaredErrorPrinter(ASTContext &C);

// Pass to emit the module files.
ASTPass *createUseStmtHandlerPass(ASTContext &C);

// Update all the types after the symbol resolver pass.
ASTPass *createStmtTypeUpdaterPass(ASTContext &C);

// Pass to expand the Array Section Statements.
ASTPass *createArraySecExpanderPass(ASTContext &C);

// Pass to expand the Where Statement
ASTPass *createConstructExpanderPass(ASTContext &C);

// Pass to expand intrinics.
ASTPass *createIntrinsicExpanderPass(ASTContext &C);

// Pass to expand lbound/ubound intrinsics for static size arrays.
ASTPass *createArrBoundsIntrinExpanderPass(ASTContext &C);

ASTPass *createFunctionTypeUpdaterPass(ASTContext &C);

ASTPass *createParamConstPropPass(ASTContext &C);

// Pass to convert entity-decl to assignment statements
// in non-module program-units.
ASTPass *createDeclEliminatorPass(ASTContext &C);

ASTPass *createFormatHandlerPass(ASTContext &C);

ASTPass *createBinaryOpHandlerPass(ASTContext &C);

ASTPass *createModFileDumperPass(ASTContext &C);

// Pass to replace non full range arraysections in function call
ASTPass *createArraySectionReplacerPass(ASTContext &C);

// Pass to bind symbols in part-refs to correct symtab (after use-stmt handler)
ASTPass *createPartRefSymResolverPass(ASTContext &C);
} // namespace fc

#endif
