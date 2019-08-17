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
#ifndef FC_SYMBOL_TABLE
#define FC_SYMBOL_TABLE

#include "AST/SymbolTableCommon.h"
#include "AST/Type.h"
#include "common/Source.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include <map>

namespace fc {

class ASTContextImpl;
class ASTContext;
class Constant;

namespace ast {
class ProgramUnit;
} // namespace ast

// TODO: move to packed vector?
struct SymbolAttributes {
  bool isConst;
  bool isAllocatable;
  // To distinguish between subroutine and function.
  bool isSubroutine;
  bool isTarget;
  bool isPointer;
  bool isOptional;
  IntentKind intentKind;
  AllocationKind allocKind;
  LinkageKind linkKind;

  SymbolAttributes()
      : isConst(false), isAllocatable(false), isSubroutine(false),
        isTarget(false), isPointer(false), isOptional(false),
        intentKind(Intent_None), allocKind(Alloc_None), linkKind(Link_Extern) {}
};
class Symbol {

private:
  static unsigned long SymbolID;
  unsigned long id;
  std::string name;

  Type *type;

  // Symboltable in which the Symbol resides in.
  SymbolTable *symTable;

  // Location of the current symbol.
  SourceLoc loc;

  // Attributes of the current symbol.
  SymbolAttributes attributes;

  // Refers to the original symbol.
  // Valid only if the curent symbol type is undeclared.
  Symbol *parentSymbol;

  // Refers to the name of the original module
  // in which the symbol is declared in.
  // Valid only in module scope.
  std::string orignalModName;

  // Tracks the literal constant value for this symbol if any. (const-prop will
  // set this for non-trivial rhs constant expressions)
  Constant *initConstant;

protected:
  Symbol(unsigned long id, llvm::StringRef &name, Type *type,
         SymbolTable *symTable, SourceLoc sourceLoc,
         SymbolAttributes &attributes);

public:
  static Symbol *Create(llvm::StringRef name, Type *type, SymbolTable *symTable,
                        SourceLoc _sourceLoc, SymbolAttributes &attributes);

  ~Symbol();

  inline bool isConstant() const { return attributes.isConst; }

  inline bool isPointer() const { return attributes.isPointer; }

  inline bool isTarget() const { return attributes.isTarget; }

  inline llvm::StringRef getName() const { return name; }

  inline Type *getType() const { return type; }

  inline Type *getOrigType() const {
    if (parentSymbol)
      return parentSymbol->getType();
    return getType();
  }

  inline bool isArray() const;

  unsigned long getID() const { return id; }

  SymbolTable *getSymTable() { return symTable; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  void setIntentKind(IntentKind kind);

  AllocationKind getAllocKind() const { return attributes.allocKind; }

  bool isStaticGlobalAlloc() const {
    return attributes.allocKind == StaticGlobal;
  }

  bool isStaticLocalAlloc() const {
    return attributes.allocKind == StaticLocal;
  }

  SymbolAttributes &getAttr() { return attributes; }

  void setAllocKind(AllocationKind kind) { attributes.allocKind = kind; }

  void setAttributes(SymbolAttributes &attr) { attributes = attr; }

  void setConstantAttribute() { attributes.isConst = true; }

  void setAllocatableAttribute() { attributes.isAllocatable = true; }

  void setInitConstant(Constant *constant) { initConstant = constant; }

  Constant *getInitConstant() { return initConstant; }

  inline bool hasIntent() const { return attributes.intentKind != Intent_None; }

  void setParentSymbol(Symbol *parentSym) { this->parentSymbol = parentSym; }

  Symbol *getOrigSymbol() { return (parentSymbol ? parentSymbol : this); }

  Symbol *getParentSymbol() { return parentSymbol; }

  SourceLoc getSourceLoc() { return loc; }

  SourceLoc getSourceLoc() const { return loc; }

  std::string getOriginalModName() const { return orignalModName; }

  void setOrigModName(std::string name) { orignalModName = name; }

  void setType(Type *newType);
};

// Keep track of all the variables
// TODO More efficient way to do this?
// TODO Move definitions to cpp file?
class SymbolTable {

public:
  Symbol *addNewSymbol(llvm::StringRef name, Type *type, SourceLoc _sourceLoc,
                       SymbolAttributes &attributes);
  Symbol *getOrInsertSymbol(llvm::StringRef name, Type *type,
                            SourceLoc _sourceLoc);

  Symbol *getSymbol(llvm::StringRef name);

  ScopeKind getScopeKind() const { return scopeKind; }

  bool isScope(ScopeKind kind) const { return kind == scopeKind; }

  std::string getName() const { return symName; }

  SymbolTable *getParent() const { return parent; }

  bool isSymbolDeclared(llvm::StringRef name) const;

  constexpr bool isMainProgramScope() const {
    return scopeKind == MainProgramScope;
  }

  constexpr bool isSubroutineScope() const {
    return scopeKind == SubroutineScope;
  }

  constexpr bool isFunctionScope() const { return scopeKind == FunctionScope; }

  constexpr bool isModuleScope() const { return scopeKind == ModuleScope; }

  constexpr bool isGlobalScope() const { return scopeKind == GlobalScope; }

  bool isLoadedFromModFile() const { return loadedFromModFile; }

  void setLoadedFromModFile(bool value) { loadedFromModFile = value; }

  Symbol *getOrigSymbol(llvm::StringRef name);

  ast::ProgramUnit *getProgramUnit() { return PU; }

  SymbolList getSymbolList();

  // returns a list of symbols in the order they were inserted.
  SymbolList getOrderedSymbolList();

  SymbolList getOrigSymbolList();

  std::string getTempName() {
    std::string str = symName + ".tmp." + std::to_string(tmpSymCount++);
    return str;
  }

  bool isAnon() { return anon; }

  // Set this true as an "FYI" for parser/sema that this symtab doesn't belong
  // the current compilation-unit like other symtabs
  bool setAnon(bool val) { return anon = val; }

  ~SymbolTable();

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  static SymbolTable *get(ASTContext &Context, ScopeKind scopeKind,
                          llvm::StringRef name, SymbolTable *parent);

  SymbolList getTempSymbols(unsigned count, Type *type, SourceLoc loc);
  Symbol *getTempSymbol(Type *type, SourceLoc loc);

protected:
  friend class ASTContextImpl;
  explicit SymbolTable(ASTContext &Context, ScopeKind scopeKind,
                       llvm::StringRef name, SymbolTable *parent);

  friend class ast::ProgramUnit;
  void setProgramUnit(ast::ProgramUnit *PU) { this->PU = PU; }

private:
  bool anon; // If this symtab is anonymous
  ScopeKind scopeKind;
  std::string symName;
  std::map<std::string, Symbol *> map;
  SymbolList orderedSyms;
  SymbolTable *parent;
  ASTContext &Context;
  ast::ProgramUnit *PU;
  bool loadedFromModFile;
  unsigned int tmpSymCount{0};
};

} // namespace fc

#endif
