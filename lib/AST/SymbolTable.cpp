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
#include "AST/SymbolTable.h"
#include "AST/ASTContext.h"
#include "AST/ASTContextImpl.h"
#include "AST/Type.h"

using namespace fc;

unsigned long Symbol::SymbolID = 0;

Symbol::~Symbol() {}

bool Symbol::isArray() const { return type->isArrayTy(); }

Symbol::Symbol(unsigned long id, llvm::StringRef &name, Type *type,
               SymbolTable *symTable, SourceLoc loc,
               SymbolAttributes &attributes)
    : id(id), name(name), type(type), symTable(symTable), loc(loc),
      attributes(attributes), parentSymbol(nullptr),
      orignalModName(symTable->getName()), initConstant(nullptr) {}

Symbol *Symbol::Create(llvm::StringRef name, Type *type, SymbolTable *symTable,
                       SourceLoc _sourceLoc, SymbolAttributes &attributes) {
  assert(type);
  return new Symbol(++SymbolID, name, type, symTable, _sourceLoc, attributes);
}

void Symbol::setType(Type *newType) { type = newType; }

void Symbol::setIntentKind(IntentKind kind) { attributes.intentKind = kind; }

// Symbol Table constructors

Symbol *SymbolTable::getSymbol(llvm::StringRef name) {
  // TODO: remove this. costly?
  auto findName = name.lower();

  auto val = map.find(findName);
  if (val == map.end())
    return nullptr;
  return val->second;
}

Symbol *SymbolTable::getOrigSymbol(llvm::StringRef name) {
  auto sym = getSymbol(name);
  if (!sym)
    return nullptr;

  auto symParent = sym->getParentSymbol();
  return symParent ? symParent : sym;
}

bool SymbolTable::isSymbolDeclared(llvm::StringRef name) const {
  // TODO: remove this. costly?
  auto findName = name.lower();

  auto val = map.find(findName);
  if (val == map.end())
    return false;
  return true;
}

SymbolList SymbolTable::getSymbolList() {
  SymbolList list;
  for (auto &pair : map) {
    list.push_back(pair.second);
  }
  return list;
}

SymbolList SymbolTable::getOrderedSymbolList() { return orderedSyms; }

SymbolList SymbolTable::getOrigSymbolList() {
  SymbolList list;
  for (auto &pair : map) {
    auto parent = pair.second->getParentSymbol();
    list.push_back(parent ? parent : pair.second);
  }
  return list;
}

SymbolTable::~SymbolTable() {
  for (auto pair : map) {
    delete pair.second;
  }
  map.clear();
}

Symbol *SymbolTable::addNewSymbol(llvm::StringRef name, Type *type,
                                  SourceLoc _sourceLoc,
                                  SymbolAttributes &attributes) {
  assert(!isSymbolDeclared(name.str()));

  // TODO: remove this. costly?
  auto lowerName = name.lower();
  auto symbol = Symbol::Create(lowerName, type, this, _sourceLoc, attributes);
  map[lowerName] = symbol;
  orderedSyms.push_back(symbol);
  return symbol;
}

Symbol *SymbolTable::getOrInsertSymbol(llvm::StringRef name, Type *type,
                                       SourceLoc _sourceLoc) {
  auto sym = getSymbol(name);
  if (sym) {
    return sym;
  }
  SymbolAttributes attr;
  return addNewSymbol(name, type, _sourceLoc, attr);
}

SymbolTable::SymbolTable(ASTContext &Context, ScopeKind scopeKind,
                         llvm::StringRef name, SymbolTable *parent)
    : scopeKind(scopeKind), symName(name), parent(parent), Context(Context),
      loadedFromModFile(false) {}

SymbolTable *SymbolTable::get(ASTContext &Context, ScopeKind scopeKind,
                              llvm::StringRef name, SymbolTable *parent) {
  return Context.impl->get(name, scopeKind, parent);
}

Symbol *SymbolTable::getTempSymbol(Type *type, SourceLoc loc) {
  SymbolAttributes attributes;
  attributes.allocKind = StaticLocal;
  return addNewSymbol(getTempName(), type, loc, attributes);
}

SymbolList SymbolTable::getTempSymbols(unsigned count, Type *type,
                                       SourceLoc loc) {
  SymbolList list;
  SymbolAttributes attributes;
  attributes.allocKind = StaticLocal;
  for (unsigned I = 0; I < count; ++I) {
    list.push_back(getTempSymbol(type, loc));
  }
  return list;
}
