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
#ifndef FC_AST_EXPR_H
#define FC_AST_EXPR_H

#include "AST/ASTContext.h"
#include "AST/ParserTreeCommon.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "Stmt.h"
#include "common/Source.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"

namespace fc {
namespace ast {
class Expr : public Stmt {

private:
  Type *type;

protected:
  ASTContext &Context;

  Expr(StmtType exprType, Type *ty, SourceLoc loc)
      : Stmt(exprType, loc, true), type(ty), Context(ty->getContext()) {}

public:
  virtual std::string dump(llvm::raw_ostream &OS, int level = 0) const = 0;

  virtual Type *getType() const { return type; }

  void setType(Type *ty) { type = ty; }

  virtual ~Expr() {}

  constexpr static bool classof(const Stmt *expr) {
    return expr->isExpression();
  }

  constexpr ASTContext &getContext() { return Context; }

  inline Expr *getOperand(unsigned I) const {
    return static_cast<Expr *>(Stmt::getOperand(I));
  }

  virtual Expr *clone() {
    llvm_unreachable("Clone not implemented for this class");
    return nullptr;
  }

  constexpr bool isDesignator() const {
    auto stmtType = getStmtType();

    switch (stmtType) {
    case ObjectNameKind:
    case ConstantValKind:
    case ArrayElementKind:
    case ArraySectionKind:
    case FunctionReferenceKind:
    case StructureComponentKind:
      return true;
    default:
      return false;
    };
  }
};

// The part within (/ and /) of an array-constructor
class ACSpec : public Expr {
public:
  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getValue(unsigned I) { return static_cast<Expr *>(operands[I]); }

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == ACSpecKind;
  }

protected:
  friend class ParseTreeBuilder;
  ACSpec(Type *type, ACValueList &list, SourceLoc loc)
      : Expr(ACSpecKind, type, loc) {
    for (unsigned I = 0; I < list.size(); I++) {
      operands.push_back(list[I]);
      list[I]->setParentNode(ParentNode(this, I));
    }
  }
};

class ArrayConstructor : public Expr {
protected:
  friend class ParseTreeBuilder;
  ArrayConstructor(ACSpec *spec, SourceLoc loc)
      : Expr(ArrayConstructorKind, spec->getType(), loc) {
    operands = {spec};
    spec->setParentNode(ParentNode(this, 0));
  }

public:
  ACSpec *getSpec() const { return static_cast<ACSpec *>(operands[0]); }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == ArrayConstructorKind;
  }
};

// Holds the constant expression.
class ConstantVal : public Expr {
private:
  Constant *constant;

public:
  Constant *getConstant() const { return constant; }

  std::string getValue() const { return getConstant()->getValue(); }

  llvm::StringRef getValueRef() const { return getConstant()->getValue(); }

  long int getInt() const { return getConstant()->getInt(); }

  double getFloat() const { return getConstant()->getFloat(); }

  bool getBool() const { return getConstant()->getBool(); }

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == ConstantValKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  Expr *clone() override {
    return new (Context, alignof(ConstantVal))
        ConstantVal(this->getConstant(), this->getSourceLoc());
  }

protected:
  ConstantVal(Constant *constant, SourceLoc loc)
      : Expr(ConstantValKind, constant->getType(), loc), constant(constant) {}
  friend class ParseTreeBuilder;
};

// Level2 epxression
// TODO Currently only handling level-2 instructions,
//      rename if separate class is required to represent other levels
class BinaryExpr : public Expr {

public:
  inline Expr *getLHS() const { return static_cast<Expr *>(operands[0]); }

  inline Expr *getRHS() const { return static_cast<Expr *>(operands[1]); }

  inline void setLHS(Expr *lhs) { operands[0] = lhs; }

  inline void setRHS(Expr *rhs) { operands[1] = rhs; }

  inline BinaryOpKind getOpKind() { return opKind; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  ~BinaryExpr() {
    delete operands[0];
    delete operands[1];
  }

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == BinaryExprKind;
  }

  Expr *clone() override {
    return new (Context, alignof(BinaryExpr)) BinaryExpr(
        this->getLHS(), getRHS(), opKind, getType(), this->getSourceLoc());
  }

protected:
  friend class ParseTreeBuilder;

  BinaryExpr(Expr *op1, Expr *op2, BinaryOpKind opKind, Type *type,
             SourceLoc loc)
      : Expr(BinaryExprKind, type, loc), opKind(opKind) {
    operands = {op1, op2};
    setParentForOperands();
  }

private:
  BinaryOpKind opKind;
};

// TODO Currently only handling for subscript ranges
//      rename if separate class is required to represent other levels
class QuadExpr : public Expr {

public:
  void setOperand(Expr *expr, unsigned I) { operands[I] = expr; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  ~QuadExpr() {}

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == QuadExprKind;
  }

  constexpr Expr *getVar() const { return static_cast<Expr *>(operands[0]); }

  constexpr Expr *getInit() const { return static_cast<Expr *>(operands[1]); }

  constexpr Expr *getEnd() const { return static_cast<Expr *>(operands[2]); }

  constexpr Expr *getIncr() const { return static_cast<Expr *>(operands[3]); }

protected:
  friend class ParseTreeBuilder;

  QuadExpr(Expr *op[4], Type *type, SourceLoc loc)
      : Expr(QuadExprKind, type, loc) {
    for (int i = 0; i < 4; ++i) {
      operands.push_back(op[i]);
      setParentForOperands();
    }
  }
};

// For expresions of the form
// ((expr(i, j)..., j = start, end), i = start, end)
class IOImpliedDo : public Expr {
protected:
  friend class ParseTreeBuilder;

  unsigned exprCount;
  unsigned impliedDoCount;

  IOImpliedDo(ExprList exprs, ExprList impliedDos, Type *type, SourceLoc &loc)
      : Expr(IOImpliedDoKind, type, loc) {
    setOperands(exprs);
    setOperands(impliedDos);
    setParentForOperands();
    exprCount = exprs.size();
    impliedDoCount = impliedDos.size();
  }

public:
  Expr *getExpr(unsigned i) const { return static_cast<Expr *>(operands[i]); }

  ExprList getExprList() const {
    ExprList list;
    for (unsigned i = 0; i < exprCount; ++i) {
      list.push_back(getExpr(i));
    }
    return list;
  }

  ExprList getImpliedDos() const {
    ExprList list;
    for (unsigned i = exprCount; i < getNumOperands(); ++i) {
      list.push_back(getExpr(i));
    }
    return list;
  }

  ~IOImpliedDo() {}

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == IOImpliedDoKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;
};

// Level4 Expression
class RelationalExpr : public Expr {
public:
  inline Expr *getLHS() const { return static_cast<Expr *>(operands[0]); }

  inline Expr *getRHS() const { return static_cast<Expr *>(operands[1]); }

  inline void setLHS(Expr *lhs) { operands[0] = lhs; }

  inline void setRHS(Expr *rhs) { operands[1] = rhs; }

  inline RelationalOpKind getOpKind() { return opKind; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  ~RelationalExpr() {}

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == RelationalExprKind;
  }

  Expr *clone() override {
    return new (Context, alignof(RelationalExpr))
        RelationalExpr(this->getLHS(), this->getRHS(), this->getOpKind(),
                       this->getType(), this->getSourceLoc());
  }

private:
  RelationalOpKind opKind;

protected:
  friend class ParseTreeBuilder;

  RelationalExpr(Expr *op1, Expr *op2, RelationalOpKind opKind, Type *type,
                 SourceLoc loc)
      : Expr(RelationalExprKind, type, loc), opKind(opKind) {
    setOperands({op1, op2});
  }
};

// Level 5 Expression
class LogicalExpr : public Expr {
public:
  inline Expr *getLHS() const { return static_cast<Expr *>(operands[0]); }

  inline Expr *getRHS() const { return static_cast<Expr *>(operands[1]); }

  inline void setLHS(Expr *lhs) { operands[0] = lhs; }

  inline void setRHS(Expr *rhs) { operands[1] = rhs; }

  inline LogicalOpKind getOpKind() { return opKind; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  ~LogicalExpr() {}

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == LogicalExprKind;
  }

  Expr *clone() override {
    return new (Context, alignof(LogicalExpr))
        LogicalExpr(this->getLHS(), this->getRHS(), this->getOpKind(),
                    this->getType(), this->getSourceLoc());
  }

private:
  LogicalOpKind opKind;

protected:
  friend class ParseTreeBuilder;

  LogicalExpr(Expr *op1, Expr *op2, LogicalOpKind kind, Type *type,
              SourceLoc loc)
      : Expr(LogicalExprKind, type, loc), opKind(kind) {
    setOperands({op1, op2});
  }
};

class ObjectName : public Expr {
private:
  Symbol *sym;

protected:
  friend class ParseTreeBuilder;

  // TODO Take type in constructor or read from symbol?
  explicit ObjectName(Symbol *sym, SourceLoc loc);

public:
  std::string getName() const;

  inline Symbol *getSymbol() const { return sym; }

  void setSymbol(Symbol *symbol) { sym = symbol; }

  virtual Type *getType() const override;

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == ObjectNameKind;
  }

  Expr *clone() override {
    return new (Context, alignof(ObjectName))
        ObjectName(this->getSymbol(), this->getSourceLoc());
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  ~ObjectName() {}
};

class StructureComponent : public Expr {

  // This Expr is used to represents the "low-level" type of this struct-comp.
  // For eg. "a%b" is a struct-comp, but it's deduced type might be an
  // array-of-int if a is array and b is int. (Will be updated by semantics by
  // examining the part-refs)
  Expr *deducedExpr{nullptr};

  // true iff each symbol that represent the part-refs are resolved.
  bool resolved{false};

protected:
  friend class ParseTreeBuilder;

  explicit StructureComponent(ExprList &partRefs, Type *type, SourceLoc loc)
      : Expr(StructureComponentKind, type, loc) {
    setOperands(partRefs);
  }

public:
  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == StructureComponentKind;
  }

  void setResolved(bool val) { resolved = val; }

  bool isResolved() const { return resolved; }

  ExprList getPartRefs() const {
    ExprList list;
    for (unsigned i = 0; i < operands.size(); ++i) {
      list.push_back(static_cast<Expr *>(operands[i]));
    }
    return list;
  }

  Expr *getPartRefAt(unsigned i) const {
    assert(i < operands.size());
    return static_cast<Expr *>(operands[i]);
  }

  Expr *getLastPartRef() const {
    return static_cast<Expr *>(operands[operands.size() - 1]);
  }

  // C618 states that we can't have more than one array-section part-ref.
  ArraySection *getArraySection() const {
    if (!isResolved())
      return nullptr;

    for (unsigned i = 0; i < operands.size(); ++i) {
      Expr *partRef = static_cast<Expr *>(operands[i]);
      if (auto arrSec = llvm::dyn_cast<ArraySection>(partRef))
        return arrSec;
    }
    return nullptr;
  }

  Expr *clone() override {
    ExprList partRefs = getPartRefs();

    return new (Context, alignof(StructureComponent))
        StructureComponent(partRefs, getType(), getSourceLoc());
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  ~StructureComponent() {}
};

class ArrayElement : public Expr {

private:
  Symbol *symbol;

public:
  constexpr static bool classof(const Stmt *d) {
    return (d->getStmtType() == ArrayElementKind);
  }

  ~ArrayElement() {}

  std::string getName() const;

  inline Symbol *getSymbol() const { return symbol; }

  void setSymbol(Symbol *sym) { symbol = sym; }

  virtual Type *getType() const override;

  Type *getElementType() const;

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getSubsExpr(unsigned I) const {
    return static_cast<Expr *>(getOperand(I + 1));
  }

  ExprList getSubscriptList() const {
    ExprList list;
    for (unsigned I = 1; I < operands.size(); ++I) {
      list.push_back(static_cast<Expr *>(operands[I]));
    }
    return list;
  }

  Expr *clone() override {
    ExprList _exprList;

    for (unsigned i = 1; i < operands.size(); ++i) {
      auto expr = static_cast<Expr *>(operands[i]);
      _exprList.push_back(expr->clone());
    }

    return new (Context, alignof(ArrayElement)) ArrayElement(
        this->symbol, getOperand(0), _exprList, this->getSourceLoc());
  }

  unsigned getNumIndices() const { return operands.size() - 1; }

protected:
  friend class ParseTreeBuilder;

  explicit ArrayElement(Symbol *sym, Expr *baseExpr, ExprList &subs,
                        SourceLoc loc);
};

// Expression of the form a=10 in the expression list for functions
// subroutines.
class AssignmentExpr : public Expr {

private:
  llvm::StringRef name;

protected:
  friend class ParseTreeBuilder;

  explicit AssignmentExpr(llvm::StringRef name, Expr *expr, SourceLoc loc)
      : Expr(AssignmentExprKind, expr->getType(), loc), name(name) {
    setOperands({expr});
  }

public:
  inline std::string getName() const { return name.str(); }

  inline Expr *getExpr() const { return static_cast<Expr *>(operands[0]); }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  ~AssignmentExpr() {}

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == AssignmentExprKind;
  }
};

// Expression of the form a=10 in the expression list for functions
// subroutines.
class CastExpr : public Expr {

private:
  Type *destType;

protected:
  friend class ParseTreeBuilder;

  explicit CastExpr(Expr *from, Type *destType, SourceLoc loc)
      : Expr(CastExprKind, destType, loc) {
    setOperands({from});
  }

public:
  inline Expr *getExpr() const { return static_cast<Expr *>(operands[0]); }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  ~CastExpr() {}

  constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == CastExprKind;
  }
};

class RangeExpr : public Expr {
protected:
  friend class ParseTreeBuilder;

  explicit RangeExpr(Expr *lb, Expr *ub, Expr *_index, Type *type,
                     SourceLoc loc);

public:
  constexpr static bool classof(const Stmt *d) {
    return (d->getStmtType() == RangeExprKind);
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getLowerBound() const { return getOperand(0); }

  inline Expr *getUpperBound() const { return getOperand(1); }

  inline Expr *getIndex() const { return getOperand(2); }

  inline bool isRange() const {
    auto index = getIndex();
    auto lowerBound = getLowerBound();
    auto upperBound = getUpperBound();
    return !index && lowerBound && upperBound;
  }

  inline bool isFullRange() const {
    auto index = getIndex();
    auto lowerBound = getLowerBound();
    auto upperBound = getUpperBound();
    return !index && !lowerBound && !upperBound;
  }

  Expr *clone() override {
    return new (Context, alignof(RangeExpr))
        RangeExpr(this->getLowerBound(), this->getUpperBound(),
                  this->getIndex(), this->getType(), this->getSourceLoc());
  }
};

class ArraySection : public Expr {
private:
  Symbol *symbol;

public:
  constexpr static bool classof(const Stmt *d) {
    return (d->getStmtType() == ArraySectionKind);
  }

  inline Expr *getSubsExpr(unsigned I) const {
    return static_cast<Expr *>(getOperand(I + 1));
  }

  std::string getName() const;

  inline Symbol *getSymbol() const { return symbol; }

  void setSymbol(Symbol *sym) { symbol = sym; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  virtual Type *getType() const override;

  // Checks if whole array is accessed.
  bool isFullRange() const;

  ExprList getSubscriptList() const {
    ExprList list;
    for (unsigned I = 1; I < operands.size(); ++I) {
      list.push_back(static_cast<Expr *>(operands[I]));
    }
    return list;
  }

  Expr *getBaseExpr() const { return static_cast<Expr *>(operands[0]); }

  Expr *clone() override {
    ExprList _exprList;

    for (unsigned i = 1; i < operands.size(); ++i) {
      auto expr = static_cast<Expr *>(operands[i]);
      _exprList.push_back(expr->clone());
    }

    return new (Context, alignof(ArraySection)) ArraySection(
        this->symbol, getOperand(0), _exprList, this->getSourceLoc());
  }

protected:
  friend class ParseTreeBuilder;

  explicit ArraySection(Symbol *sym, Expr *baseExpr, ExprList &list,
                        SourceLoc loc);

  explicit ArraySection(Symbol *sym, Expr *baseExpr, SourceLoc loc)
      : Expr(ArraySectionKind, sym->getType(), loc), symbol(sym) {
    setOperands({baseExpr});
  }
};

class ArraySpec : public Expr {
private:
  unsigned int numDims;
  bool assumedSize;

protected:
  friend class ParseTreeBuilder;

  explicit ArraySpec(DynArrBoundsList &list, unsigned _numDims, Type *baseTy,
                     SourceLoc loc)
      : Expr(ArraySpecKind, baseTy, loc), numDims(_numDims),
        assumedSize(false) {
    for (auto b : list) {
      operands.push_back(b.first);
      operands.push_back(b.second);
    }
    setParentForOperands();
  }

  // Assumed size array
  explicit ArraySpec(Type *baseTy, SourceLoc loc)
      : Expr(ArraySpecKind, baseTy, loc), numDims(1), assumedSize(true) {}

public:
  inline DynArrayBounds getBounds(unsigned I) const {
    DynArrayBounds bounds;
    bounds.first = getOperand(2 * I);
    bounds.second = getOperand(2 * I + 1);
    return bounds;
  }

  // NOTE: costly method. Try to use  getBounds().
  DynArrBoundsList getBoundsList() const {
    DynArrBoundsList list;
    for (unsigned I = 0; I < getNumBounds(); ++I) {
      list.push_back(getBounds(I));
    }
    return list;
  }

  inline unsigned getNumBounds() const { return (getNumOperands() / 2); }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  inline unsigned getNumDims() { return numDims; }

  inline constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == ArraySpecKind;
  }

  bool isAssumedSize() { return assumedSize; }
};

// Function reference
class FunctionReference : public Expr {
private:
  Symbol *sym;

protected:
  friend class ParseTreeBuilder;

  FunctionReference(Symbol *_sym, ExprList &_exprList, Type *_type,
                    SourceLoc _loc)
      : Expr(FunctionReferenceKind, _type, _loc), sym(_sym) {
    setOperands(_exprList);
  }

public:
  inline constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == FunctionReferenceKind;
  }

  Expr *clone() override {
    ExprList _exprList;

    for (unsigned i = 0; i < operands.size(); ++i) {
      auto expr = static_cast<Expr *>(operands[i]);
      _exprList.push_back(expr->clone());
    }

    return new (Context, alignof(FunctionReference)) FunctionReference(
        this->sym, _exprList, this->getType(), this->getSourceLoc());
  }

  ~FunctionReference() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Symbol *getSymbol() const { return sym; }

  void setSymbol(Symbol *symbol) { sym = symbol; }

  inline Expr *getExpr(unsigned I) {
    return static_cast<Expr *>(getOperand(I));
  }
  ExprList getArgsList() const {
    ExprList _exprList;

    for (unsigned i = 0; i < operands.size(); ++i) {
      auto expr = static_cast<Expr *>(operands[i]);
      _exprList.push_back(expr);
    }
    return _exprList;
  }
};

class Format : public Expr {

protected:
  friend class ParseTreeBuilder;

  Format(ExprList &_exprList, Type *_type, SourceLoc _loc)
      : Expr(FormatKind, _type, _loc) {
    setOperands(_exprList);
  }

public:
  inline constexpr static bool classof(const Stmt *expr) {
    return expr->getStmtType() == FormatKind;
  }

  ~Format() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getStringFormat(unsigned i) const {
    return static_cast<ConstantVal *>(getOperand(i));
  }

  ExprList getExprList() const {
    ExprList list;
    for (unsigned i = 0; i < getNumOperands(); ++i) {
      list.push_back(static_cast<Expr *>(getOperand(i)));
    }
    return list;
  }
};

} // namespace ast
} // namespace fc

#endif
