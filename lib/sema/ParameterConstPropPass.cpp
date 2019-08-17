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
#include "AST/ParseTreeBuilder.h"
#include "AST/ProgramUnit.h"
#include "AST/StmtVisitor.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"
#include "sema/Intrinsics.h"

using namespace fc;
using namespace ast;
namespace fc {

class ParamConstProp : public StmtVisitor<ParamConstProp, bool> {
  ParseTreeBuilder builder;

public:
  ParamConstProp(ASTContext &C) : builder(C) {}

  bool postObjectName(ObjectName *obj) override {
    auto sym = obj->getSymbol()->getOrigSymbol();
    if (!sym->isConstant()) {
      return true;
    }
    auto constant = sym->getInitConstant();
    if (!constant) {
      // error() << "\n Constant value not found for symbol :" <<
      // sym->getName();
      return true;
    }
    auto constVal = builder.buildConstantVal(
        constant->getValue(), sym->getType(), obj->getSourceLoc());
    obj->replaceWith(constVal);
    return true;
  }
};

class ParamConstPropPass : public ASTBlockPass {
  ParamConstProp stmtVisitor;

public:
  explicit ParamConstPropPass(ASTContext &C)
      : ASTBlockPass(C, "Parameter value updater pass"), stmtVisitor(C) {}

  virtual bool runOnBlock(Block *block) override {

    for (auto stmt : *block) {
      if (!stmtVisitor.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
}; // namespace fc

ASTPass *createParamConstPropPass(ASTContext &C) {
  return new ParamConstPropPass(C);
}
} // namespace fc
