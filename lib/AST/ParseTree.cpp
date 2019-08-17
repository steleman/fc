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
#include "AST/Declaration.h"
#include "AST/Expressions.h"
#include "AST/ParseTreeBuilder.h"
#include "AST/ParserTreeCommon.h"
#include "AST/ProgramUnit.h"
#include "AST/Statements.h"
#include "AST/Stmt.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Util.h"

using namespace fc;
using namespace ast;

using llvm::isa;

ParseTree::~ParseTree() {}

ProgramUnit::ProgramUnit(SymbolTable *symbolTable, ProgramUnitKind kind,
                         llvm::StringRef &name, SpecificationPart *specPart,
                         ExecutionPart *execPart, ProgramUnitList &programList)
    : name(name.lower()), specPart(specPart), execPart(execPart), kind(kind),
      symbolTable(symbolTable), parent(nullptr), programList(programList) {

  this->symbolTable->setProgramUnit(this);

  for (auto subPU : programList) {
    subPU->setParent(this);
  }
}

Block *Stmt::getParentBlock() {
  Stmt *parentStmt = this->getParent();
  Block *block = nullptr;
  while (parentStmt) {
    if (isa<Block>(parentStmt)) {
      block = static_cast<Block *>(parentStmt);
      break;
    }
    parentStmt = parentStmt->getParent();
  }
  return block;
}

void Stmt::setOperand(Stmt *stmt, int index) {
  assert(index >= 0 && index < (int)operands.size());
  operands[index] = stmt;
}

void Stmt::setParentForOperands() {
  for (unsigned I = 0; I < operands.size(); ++I) {
    if (operands[I])
      operands[I]->setParentNode(ParentNode(this, I));
  }
}

void Stmt::setOperands(llvm::ArrayRef<Stmt *> ops) {
  for (auto op : ops) {
    operands.push_back(op);
  }
  setParentForOperands();
}

void Stmt::setOperands(ExprList &list) {
  for (Expr *const op : list) {
    operands.push_back((llvm::cast<Stmt>(op)));
  }
  setParentForOperands();
}

void Stmt::eraseFromParent() {
  auto node = getParentNode();
  assert(node.index != -1 && node.parent);
  node.parent->setOperand(nullptr, node.index);
  this->markAsInvalid();
}

void Block::replaceWith(Stmt *newStmt, Stmt *existingStmt) {
  assert(newStmt && existingStmt);
  auto II = std::find(stmtList.begin(), stmtList.end(), existingStmt);
  if (II == stmtList.end()) {
    llvm_unreachable("statement not found in block");
  }
  existingStmt->markAsInvalid();
  *II = newStmt;
  newStmt->setParentNode(ParentNode(this, -1));
}

Block::Block(StmtList &stmts, SourceLoc loc) : Stmt(BlockKind, loc) {
  for (auto stmt : stmts) {
    /// Paarent of expression nodes are the actual
    // instructions.
    if (llvm::isa<Expr>(stmt))
      continue;
    stmtList.push_back(stmt);
    stmt->setParentNode(ParentNode(this, -1));
  }
}

void Block::addStmt(Stmt *newStmt) {
  stmtList.push_back(newStmt);
  newStmt->setParentNode(ParentNode(this, -1));
}

void Block::insertFront(Stmt *newStmt) {
  stmtList.push_front(newStmt);
  newStmt->setParentNode(ParentNode(this, -1));
}

UseStmtList SpecificationPart::getUseStmts() const {
  UseStmtList list;
  for (auto stmt : block->getStmtList()) {
    if (auto useStmt = llvm::dyn_cast<UseStmt>(stmt)) {
      list.push_back(useStmt);
    }
  }
  return list;
}

void Stmt::setParentNode(ParentNode parent, bool updateParent) {
  this->parentNode.index = parent.index;
  this->parentNode.parent = parent.parent;
  if (updateParent) {
    assert(isExpression());
    assert(parentNode.parent);
    auto parentStmt = parentNode.parent;
    assert(parentNode.index != -1 && parentStmt);
    parentStmt->setOperand(this, parentNode.index);
  }
}

void Stmt::replaceWith(Stmt *newStmt) {
  assert(isExpr && newStmt->isExpression());
  assert(newStmt);
  assert(parentNode.parent);
  auto parentStmt = parentNode.parent;
  assert(parentNode.index != -1 && parentStmt);
  parentStmt->setOperand(newStmt, parentNode.index);
  newStmt->setParentNode(this->parentNode);
}

void Block::insertStmtBefore(Stmt *newStmt, Stmt *existingStmt) {
  assert(newStmt && existingStmt);
  auto II = std::find(stmtList.begin(), stmtList.end(), existingStmt);
  if (II == stmtList.end()) {
    llvm_unreachable("statement not found in block");
  }
  stmtList.insert(II, newStmt);
  newStmt->setParentNode(existingStmt->getParentNode());
}

void Block::insertStmtAfter(Stmt *newStmt, Stmt *existingStmt) {
  assert(newStmt && existingStmt);
  auto II = std::find(stmtList.begin(), stmtList.end(), existingStmt);
  if (II == stmtList.end()) {
    llvm_unreachable("statement not found in block");
  }
  stmtList.insert(++II, newStmt);
}

void Block::removeStmt(Stmt *stmt) {
  assert(stmt);
  auto II = std::find(stmtList.begin(), stmtList.end(), stmt);
  if (II == stmtList.end()) {
    llvm_unreachable("statement not found in block");
  }
  stmtList.erase(II);
}

ParseTree::ParseTree(llvm::StringRef name, ASTContext &Context,
                     SymbolTable *symTable, ProgramUnitList &programList)
    : ProgramUnit(symTable, ProgramKind, name, nullptr, nullptr, programList),
      Context(Context) {}

bool ProgramUnit::hasUsedSymbolTable(SymbolTable *symTable) {
  return usedSymTables.count(symTable) != 0;
}

void ProgramUnit::addUsedSymTable(SymbolTable *symTable) {
  assert((usedSymTables.find(symTable) == usedSymTables.end()));
  usedSymTables.insert(symTable);
}

DerivedTypeDef *ProgramUnit::getDTD(llvm::StringRef name) {
  ProgramUnit *walkPU = this;

  while (walkPU) {
    SpecificationPart *specPart = nullptr;
    if ((specPart = walkPU->getSpec())) {
      DerivedTypeDef *dtd = specPart->getDTD(name);
      if (dtd)
        return dtd;
    }
    walkPU = walkPU->getParent();
  }

  return nullptr;
}

DerivedTypeDef *ProgramUnit::getDTD(StructType *structType) {
  ProgramUnit *walkPU = this;

  while (walkPU) {
    SpecificationPart *specPart = nullptr;
    if ((specPart = walkPU->getSpec())) {
      DerivedTypeDef *dtd = specPart->getDTD(structType);
      if (dtd)
        return dtd;
    }
    walkPU = walkPU->getParent();
  }

  return nullptr;
}

DerivedTypeDefList ProgramUnit::getDTDs() {
  ProgramUnit *walkPU = this;
  DerivedTypeDefList dtds;

  while (walkPU) {
    SpecificationPart *specPart = nullptr;
    if ((specPart = walkPU->getSpec())) {
      for (DerivedTypeDef *dtd : specPart->getDTDs()) {
        dtds.push_back(dtd);
      }
    }
    walkPU = walkPU->getParent();
  }

  return dtds;
}

void ProgramUnit::addDTD(DerivedTypeDef *dtd) {
  SpecificationPart *specPart = getSpec();
  assert(specPart);
  specPart->addDTD(dtd);
}

Type *ObjectName::getType() const { return sym->getOrigType(); }

Type *ArrayElement::getType() const { return symbol->getOrigType(); }

Type *ArrayElement::getElementType() const {
  auto Ty = llvm::dyn_cast<ArrayType>(getType());
  assert(Ty);
  return Ty->getElementTy();
}

Type *ArraySection::getType() const { return symbol->getOrigType(); }

// Get the symbols used in children.
// NOTE: this only works one level now. and not yet supported
// for module.
void ProgramUnit::getUsedSymbolsInChildren(SymbolSet &set) {
  assert(kind != ModuleKind);

  for (auto subPU : programList) {
    auto subSymTable = subPU->getSymbolTable();

    for (auto sym : subSymTable->getSymbolList()) {
      auto parentSym = sym->getParentSymbol();
      if (!parentSym)
        continue;

      //  Ignore functions..
      if (parentSym->getType()->isFunctionTy() ||
          parentSym->getType()->isUndeclaredFnTy())
        continue;
      // Any symbol which refers to the parent symbol table
      // is what we need.
      assert(parentSym->getParentSymbol() == nullptr);
      if (parentSym->getSymTable() == this->symbolTable) {
        set.insert(parentSym);
      }
    }
  }
}

Type *ProgramUnit::getType() const {
  auto ParentSym = symbolTable->getParent();
  assert(ParentSym);
  auto PUSym = ParentSym->getSymbol(getName());
  assert(PUSym);
  return PUSym->getType();
}

void ProgramUnit::setType(Type *type) {
  auto ParentSym = symbolTable->getParent();
  assert(ParentSym);
  auto PUSym = ParentSym->getSymbol(getName());
  assert(PUSym);
  PUSym->setType(type);
}

Type *Function::getReturnType() const {
  auto ty = getType();
  auto FTy = llvm::cast<FunctionType>(ty);
  return FTy->getReturnType();
}

void Function::setReturnType(Type *returnTy) {
  auto ty = getType();
  auto FTy = llvm::cast<FunctionType>(ty);

  auto newFTy =
      FunctionType::get(returnTy->getContext(), returnTy, FTy->getArgList());
  setType(newFTy);
}

ObjectName::ObjectName(Symbol *sym, SourceLoc loc)
    : Expr(ObjectNameKind, sym->getType(), loc), sym(sym) {}

ArrayElement::ArrayElement(Symbol *sym, Expr *baseExpr, ExprList &subs,
                           SourceLoc loc)
    : Expr(ArrayElementKind, sym->getType(), loc), symbol(sym) {
  setOperands({baseExpr});
  setOperands(subs);
}

std::string ArrayElement::getName() const { return symbol->getName(); }

std::string ObjectName::getName() const { return sym->getName(); }

std::string ArraySection::getName() const { return symbol->getName(); }

bool ArraySection::isFullRange() const {
  for (unsigned I = 1; I < operands.size(); ++I) {
    auto subsExpr = static_cast<Expr *>(operands[I]);
    auto rangeExpr = llvm::dyn_cast<RangeExpr>(subsExpr);
    if (!rangeExpr || !rangeExpr->isFullRange())
      return false;
  }
  return true;
}

RangeExpr::RangeExpr(Expr *lb, Expr *ub, Expr *_index, Type *baseTy,
                     SourceLoc loc)
    : Expr(RangeExprKind, baseTy, loc) {
  setOperands({lb, ub, _index});
}

ArraySection::ArraySection(Symbol *sym, Expr *baseExpr, ExprList &list,
                           SourceLoc loc)
    : Expr(ArraySectionKind, sym->getType(), loc), symbol(sym) {
  setOperands({baseExpr});
  setOperands(list);
}

SpecificationPart ::~SpecificationPart() { delete block; }

UseStmt::~UseStmt() { delete modSymTable; }
