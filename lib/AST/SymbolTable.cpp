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
