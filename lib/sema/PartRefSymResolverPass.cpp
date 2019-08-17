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
#include "AST/ASTPass.h"
#include "AST/ParseTreeBuilder.h"
#include "AST/StmtVisitor.h"

using namespace fc;
using namespace ast;
namespace fc {

class PartRefVisitor : public StmtVisitor<PartRefVisitor, bool> {
  ParseTreeBuilder builder;

public:
  explicit PartRefVisitor(ASTContext &C) : builder(C) {}

  // FIXME!!! This OrigSymbol vs Symbol is causing a lot of confusion! See below
  // for a scenario where we get a nullified PU.
  Symbol *getOrigSymbol(const Expr *expr) const {
    if (auto obj = llvm::dyn_cast<ObjectName>(expr))
      return obj->getSymbol()->getOrigSymbol();
    else if (auto arrElm = llvm::dyn_cast<ArrayElement>(expr))
      return arrElm->getSymbol()->getOrigSymbol();
    else if (auto arrSec = llvm::dyn_cast<ArraySection>(expr))
      return arrSec->getSymbol()->getOrigSymbol();
    // This might be an ArrayElement in disguise that StmtTypeUpdater missed.
    else if (auto funcRef = llvm::dyn_cast<FunctionReference>(expr))
      return funcRef->getSymbol()->getOrigSymbol();

    return nullptr;
  }

  Symbol *getSymbol(const Expr *expr) const {
    if (auto obj = llvm::dyn_cast<ObjectName>(expr))
      return obj->getSymbol();
    else if (auto arrElm = llvm::dyn_cast<ArrayElement>(expr))
      return arrElm->getSymbol();
    else if (auto arrSec = llvm::dyn_cast<ArraySection>(expr))
      return arrSec->getSymbol();
    // This might be an ArrayElement in disguise that StmtTypeUpdater missed.
    else if (auto funcRef = llvm::dyn_cast<FunctionReference>(expr))
      return funcRef->getSymbol();

    return nullptr;
  }

  void setSymbol(Expr *expr, Symbol *sym) const {
    if (auto obj = llvm::dyn_cast<ObjectName>(expr))
      obj->setSymbol(sym);
    else if (auto arrElm = llvm::dyn_cast<ArrayElement>(expr))
      arrElm->setSymbol(sym);
    else if (auto arrSec = llvm::dyn_cast<ArraySection>(expr))
      arrSec->setSymbol(sym);
    else if (auto funcRef = llvm::dyn_cast<FunctionReference>(expr))
      funcRef->setSymbol(sym);
    else
      llvm_unreachable("bad expr");
  }

  bool postStructureComponent(StructureComponent *structComp) override {
    if (structComp->isResolved())
      return true;

    ExprList partRefs = structComp->getPartRefs();
    Symbol *outerStruct = getOrigSymbol(partRefs[0]);

    StructType *outerStructType = llvm::dyn_cast<StructType>(
        Type::getCoreElementType(outerStruct->getType()));
    assert(outerStructType);

    ProgramUnit *structPU = outerStruct->getSymTable()->getProgramUnit();
    // FIXME! This is weird! A symbol should always have a PU. This happens when
    // the symbol is imported from a .mod. We need to unify this origSymbol vs
    // Symbol differnce.
    if (!structPU) {
      outerStruct = getSymbol(partRefs[0]);
      structPU = outerStruct->getSymTable()->getProgramUnit();
    }

    assert(structPU);
    DerivedTypeDef *outerStructDTD = structPU->getDTD(outerStructType);
    assert(outerStructDTD);

    for (unsigned i = 1, e = partRefs.size(); i != e; ++i) {
      Expr *partRef = partRefs[i];

      Symbol *partRefSym = getSymbol(partRef);
      assert(partRefSym);
      std::string partRefName = partRefSym->getName().str();

      // If this partRef's symbol is anonymous
      if (partRefSym->getSymTable()->isAnon()) {
        FC_DEBUG(debug() << "PartRefSymResolverPass: was: "
                         << getSymbol(partRef)->dump(debug()) << "\n");
        setSymbol(partRef,
                  outerStructDTD->getSymbolTable()->getSymbol(partRefName));
        FC_DEBUG(debug() << "PartRefSymResolverPass: now: "
                         << getSymbol(partRef)->dump(debug()) << "\n\n");
      }

      // If not the last part-ref, then set the outerStructDTD as that of the
      // last part-ref.
      if (i != (e - 1)) {
        Type *partRefType = outerStructType->getContainedType(partRefName);

        outerStructType =
            llvm::dyn_cast<StructType>(Type::getCoreElementType(partRefType));
        assert(outerStructType);
        outerStructDTD = structPU->getDTD(outerStructType);
        assert(outerStructDTD);
      }
    }

    structComp->setResolved(true);
    return true;
  }
};

class PartRefSymResolverPass : public ASTBlockPass {
  PartRefVisitor partRefVisitor;

public:
  explicit PartRefSymResolverPass(ASTContext &C)
      : ASTBlockPass(C, "PartRef Symbol Resolver pass"), partRefVisitor(C) {}

  bool runOnBlock(Block *block) override {
    for (auto stmt : *block) {
      if (!partRefVisitor.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
};

ASTPass *createPartRefSymResolverPass(ASTContext &C) {
  return new PartRefSymResolverPass(C);
}
} // namespace fc
