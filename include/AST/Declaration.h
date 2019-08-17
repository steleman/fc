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
#ifndef FC_AST_DECL_H
#define FC_AST_DECL_H

#include "AST/Expressions.h"
#include "AST/ParserTreeCommon.h"
#include "common/Source.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Casting.h"

namespace fc {
namespace ast {

class TypeSpec {

public:
  enum TypeSpecKind {
    IntrinsicSpecKind,
    DerivedSpecKind,
  };
  virtual std::string dump(llvm::raw_ostream &OS, int level = 0) const = 0;

  virtual Type *getBaseType() = 0;

  virtual void setBaseType(Type *ty) = 0;

  virtual ~TypeSpec() {}

  constexpr TypeSpecKind getSpecKind() const { return kind; }

  explicit TypeSpec(TypeSpecKind kind) : kind(kind) {}

private:
  TypeSpecKind kind;
};

class IntrinsicTypeSpec : public TypeSpec {

public:
  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  ArraySpec *getArraySpec() { return arrSpec; }

  Type *getBaseType() override { return type; };

  void setBaseType(Type *ty) override { type = ty; }

  constexpr static bool classof(const TypeSpec *spec) {
    return (spec->getSpecKind() == TypeSpec::IntrinsicSpecKind);
  }

protected:
  friend class ParseTreeBuilder;

  IntrinsicTypeSpec(Type *type, ArraySpec *length)
      : TypeSpec(TypeSpec::IntrinsicSpecKind), type(type), arrSpec(length) {}

private:
  Type *type;
  // Required in case of character arrays.
  ArraySpec *arrSpec;
};

class DerivedTypeSpec : public TypeSpec {
private:
  // Can only be filled once the derived-type's definition is known.
  Type *type;

  // The standard allows derived-type-spec pointer/allocatable
  // before a derived-type-def.  In those cases, the parser is oblivious of the
  // type's structure and only knows it's name. We track it here so that
  // semantics can resolve them once the corresponding type is known.
  std::string typeName;

public:
  // This cannot work like other TypeSpecs since we can have a pointer to a type
  // that we haven't seen yet. Currently we are setting this as UndeclaredTy().
  Type *getBaseType() override { return type; };

  void setBaseType(Type *ty) override { type = ty; }

  llvm::StringRef getName() const { return llvm::StringRef(typeName); }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  constexpr static bool classof(const TypeSpec *spec) {
    return (spec->getSpecKind() == TypeSpec::DerivedSpecKind);
  }

protected:
  friend class ParseTreeBuilder;

  DerivedTypeSpec(Type *type, llvm::StringRef &tyName)
      : TypeSpec(TypeSpec::DerivedSpecKind), type(type),
        typeName(tyName.lower()) {}
};

class DeclarationTypeSpec : public Stmt {
  TypeSpec *typeSpec;
  Expr *kind;

protected:
  friend class ParseTreeBuilder;

  DeclarationTypeSpec(TypeSpec *typeSpec, SourceLoc loc, Expr *kind = nullptr)
      : Stmt(DeclarationTypeSpecKind, loc), typeSpec(typeSpec) {
    setOperands({kind});
  }

public:
  constexpr static bool classof(const Stmt *stmt) {
    return stmt->getStmtType() == DeclarationTypeSpecKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  TypeSpec *getTypeSpec() const { return typeSpec; }

  ~DeclarationTypeSpec() { delete typeSpec; }

  Expr *getKind() { return static_cast<Expr *>(operands[0]); }
};

class AttrSpec {
public:
  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  constexpr AttrSpecKind getKind() { return kind; }

  IntentKind getIntentKind() {
    assert(kind == Intent);
    return intentKind;
  }
  ArraySpec *getArraySpec() { return arrSpec; }

protected:
  friend class ParseTreeBuilder;

  AttrSpec(AttrSpecKind kind, ArraySpec *arrSpec = nullptr)
      : kind(kind), arrSpec(arrSpec) {}
  AttrSpec(AttrSpecKind kind, IntentKind intentKind)
      : kind(kind), intentKind(intentKind), arrSpec(nullptr) {}

private:
  AttrSpecKind kind;
  IntentKind intentKind;
  ArraySpec *arrSpec;
};

class EntityDecl : public Stmt {
  Symbol *sym;

protected:
  friend class ParseTreeBuilder;

  EntityDecl(Symbol *sym, DeclarationTypeSpec *declTypeSpec, ArraySpec *arrSpec,
             Expr *init, SourceLoc loc)
      : Stmt(EntityDeclKind, loc), sym(sym) {
    setOperands({arrSpec, init, declTypeSpec});
  }

public:
  constexpr static bool classof(const Stmt *stmt) {
    return stmt->getStmtType() == EntityDeclKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  Symbol *getSymbol() const { return sym; }

  inline ArraySpec *getArraySpec() const {
    return static_cast<ArraySpec *>(operands[0]);
  }

  void setArraySpec(ArraySpec *arrSpec) { operands[0] = arrSpec; }

  inline Expr *getInit() const { return static_cast<ArraySpec *>(operands[1]); }

  inline void setInit(Expr *initVal) { operands[1] = initVal; }

  inline DeclarationTypeSpec *getDeclTypeSpec() {
    return static_cast<DeclarationTypeSpec *>(operands[2]);
  }

  inline void setDeclTypeSpec(DeclarationTypeSpec *declTypeSpec) {
    operands[2] = declTypeSpec;
  }

  ~EntityDecl() {}
};

class ImplicitPart {};

class ImportStmt : public Stmt {};

class UseStmt : public Stmt {
  SymbolTable *modSymTable;
  DerivedTypeDefList dtds;
  bool intrinsic;

protected:
  friend class ParseTreeBuilder;
  explicit UseStmt(SymbolTable *modSymTable, SourceLoc sourceLoc,
                   bool _intrinsic)
      : Stmt(UseStmtKind, sourceLoc), modSymTable(modSymTable),
        intrinsic(_intrinsic) {}

public:
  // Is this safe ? Ie. relying on the symbol-table's name for getting the used
  // module name ? Shouldn't we build UseStmt with only the module name and then
  // attach it with a symbol-table at sema, not during parsing ?
  std::string getModuleName() { return modSymTable->getName(); }

  SymbolTable *getSymbolTable() { return modSymTable; }

  DerivedTypeDefList getDTDs() { return dtds; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  constexpr static bool classof(const Stmt *stmt) {
    return (stmt->getStmtType() == UseStmtKind);
  }

  bool isIntrinsic() { return intrinsic; }

  void addDTD(DerivedTypeDef *dtd) { dtds.push_back(dtd); }

  ~UseStmt();
};

} // namespace ast
} // namespace fc

#endif
