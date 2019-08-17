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
#ifndef FC_STATEMENTS_H
#define FC_STATEMENTS_H

#include "AST/ParserTreeCommon.h"
#include "AST/Type.h"
#include "common/Source.h"

#include "AST/Expressions.h"
#include "AST/ProgramUnit.h"
#include "AST/Stmt.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include <map>

namespace fc {
namespace ast {

class AssignmentStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  AssignmentStmt(Expr *lhs, Expr *rhs, SourceLoc _sourceLoc)
      : Stmt(AssignmentStmtKind, _sourceLoc) {
    setOperands({lhs, rhs});
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == AssignmentStmtKind;
  }

  ~AssignmentStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getRHS() const { return static_cast<Expr *>(operands[1]); }

  inline Expr *getLHS() const { return static_cast<Expr *>(operands[0]); }

  inline void setLHS(Expr *lhs) { operands[0] = lhs; }

  inline void setRHS(Expr *rhs) { operands[1] = rhs; }
};

class PointerAssignmentStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  PointerAssignmentStmt(Expr *lhs, Expr *rhs, SourceLoc _sourceLoc)
      : Stmt(PointerAssignmentStmtKind, _sourceLoc) {
    setOperands({lhs, rhs});
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == PointerAssignmentStmtKind;
  }

  ~PointerAssignmentStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getRHS() const { return static_cast<Expr *>(operands[1]); }

  inline Expr *getLHS() const { return static_cast<Expr *>(operands[0]); }

  inline void setLHS(Expr *lhs) { operands[0] = lhs; }

  inline void setRHS(Expr *rhs) { operands[1] = rhs; }
};

// This can be part of IfElse stmt/ while stmt.
class IfStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  explicit IfStmt(Expr *condition, Block *block, SourceLoc loc)
      : Stmt(IfStmtKind, loc) {
    setOperands({condition, block});
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == IfStmtKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getCondition() { return static_cast<Expr *>(operands[0]); }

  inline Block *getBlock() { return static_cast<Block *>(operands[1]); }

  ~IfStmt() {}
};

class IfElseStmt : public Stmt {
private:
  IfConstructKindList kindList;

protected:
  friend class ParseTreeBuilder;

  IfElseStmt(IfStmtList &_list, IfConstructKindList &kindList,
             SourceLoc _sourceLoc)
      : Stmt(IfElseStmtKind, _sourceLoc), kindList(kindList) {
    for (auto ifStmt : _list) {
      operands.push_back(ifStmt);
    }
    setParentForOperands();
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == IfElseStmtKind;
  }

  ~IfElseStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline IfStmt *getIfStmt(unsigned I) const {
    return static_cast<IfStmt *>(getOperand(I));
  }

  IfConstructKindList &getKindList() { return kindList; }
};

class ForAllStmt : public Stmt {
  llvm::StringRef constructName;

public:
  inline constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == ForAllStmtKind;
  }

  ~ForAllStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Block *getBlock() const { return static_cast<Block *>(operands[0]); }

  inline llvm::StringRef getName() const { return constructName; }

  ExprList getExprList() const {
    ExprList list;
    for (unsigned I = 1; I < getNumOperands(); ++I) {
      list.push_back(static_cast<Expr *>(operands[I]));
    }
    return list;
  }

protected:
  friend class ParseTreeBuilder;

  ForAllStmt(ExprList &exprList, Block *action, llvm::StringRef name,
             SourceLoc _sourceLoc)
      : Stmt(ForAllStmtKind, _sourceLoc), constructName(name) {

    operands.push_back(action);
    for (auto expr : exprList) {
      operands.push_back(expr);
    }
    setParentForOperands();
  }
};

class DoStmt : public Stmt {
  llvm::StringRef constructName;

  // TODO may be a struct for loop attributes
  bool parallelLoop;

public:
  inline constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == DoStmtKind;
  }

  ~DoStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline QuadExpr *getQuadExpr() const {
    return static_cast<QuadExpr *>(operands[0]);
  }

  inline Block *getBlock() const { return static_cast<Block *>(operands[1]); }

  inline llvm::StringRef getName() const { return constructName; }

  bool isParallel() { return parallelLoop; }

  void markAsParallel() { parallelLoop = true; }

protected:
  friend class ParseTreeBuilder;

  DoStmt(QuadExpr *_expr, Block *action, llvm::StringRef name,
         SourceLoc _sourceLoc)
      : Stmt(DoStmtKind, _sourceLoc), constructName(name), parallelLoop(false) {
    setOperands({_expr, action});
  }
};

class DoWhileStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  DoWhileStmt(Expr *expr, Block *action, SourceLoc _sourceLoc)
      : Stmt(DoWhileStmtKind, _sourceLoc) {
    setOperands({expr, action});
  }

public:
  inline constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == DoWhileStmtKind;
  }

  ~DoWhileStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getLogicalExpr() const {
    return static_cast<Expr *>(operands[0]);
  }

  inline Block *getBlock() const { return static_cast<Block *>(operands[1]); }

  inline void setLogicalExpr(Expr *expr) { operands[0] = expr; }

  inline void setBlock(Block *block) { operands[1] = block; }
};

// stop-stmt is STOP [stop-code]
// stop-code is constant-int-expr
class StopStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  StopStmt(Expr *stopCode, SourceLoc _sourceLoc)
      : Stmt(StopStmtKind, _sourceLoc) {
    setOperands(stopCode);
  };

public:
  inline constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == StopStmtKind;
  }

  ~StopStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getStopCode() const { return static_cast<Expr *>(operands[0]); }

  inline void setStopCode(Expr *code) { operands[0] = code; }

  inline Type *getType() const { return getStopCode()->getType(); }
};

// ALLOCATE statement
// currently handling ALLOCATE(allocation-list [, stat = static-int-var])
// allocation-list -> allocate-object(shape-spec-list)
class AllocateStmt : public Stmt {
private:
  SymbolList allocateObjList;

protected:
  friend class ParseTreeBuilder;

  AllocateStmt(SymbolList &symbolList, ArraySpecList &arraySpecList,
               SourceLoc loc, Expr *stat)
      : Stmt(AllocateStmtKind, loc) {
    allocateObjList.append(symbolList.begin(), symbolList.end());
    setOperands(stat);
    for (auto spec : arraySpecList) {
      operands.push_back(spec);
    }
    setParentForOperands();
  }

public:
  inline constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == AllocateStmtKind;
  }

  SymbolList getAllocateObjList() { return allocateObjList; }

  Expr *getStat() const { return static_cast<Expr *>(operands[0]); }

  void setStat(Expr *stat) { operands[0] = stat; }

  inline ArraySpec *getAllocateShape(unsigned I) const {
    return static_cast<ArraySpec *>(getOperand(I + 1));
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  ~AllocateStmt() {}
};

// DEALLOCATE statement
// Currently handling deallocate(allocate-list, [, stat = static-int-var])
// allocation-list -> allocate-object
class DeAllocateStmt : public Stmt {
private:
  SymbolList objList;

protected:
  friend class ParseTreeBuilder;

  DeAllocateStmt(SymbolList &symList, SourceLoc loc, Expr *stat)
      : Stmt(DeAllocateStmtKind, loc) {
    objList.append(symList.begin(), symList.end());
    setOperands(stat);
  }

public:
  inline constexpr static bool classof(const Stmt *stmt) {
    return stmt->getStmtType() == DeAllocateStmtKind;
  }

  SymbolList getDeAllocateObjList() { return objList; }

  Expr *getStat() const { return static_cast<Expr *>(operands[0]); }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  ~DeAllocateStmt() {}
};

class NullifyStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;
  explicit NullifyStmt(ExprList &ptrObjList, SourceLoc loc)
      : Stmt(NullifyStmtKind, loc) {
    setOperands(ptrObjList);
  }

public:
  constexpr static bool classof(const Stmt *stmt) {
    return stmt->getStmtType() == NullifyStmtKind;
  }

  std::string dump(llvm::raw_ostream &, int) const;

  ~NullifyStmt() {}
};

// Currently,
// PRINT *,<expr-list> is handled.
class PrintStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  PrintStmt(ExprList &_exprList, SourceLoc _sourceLoc)
      : Stmt(PrintStmtKind, _sourceLoc) {
    setOperands(_exprList);
  };

public:
  inline constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == PrintStmtKind;
  }

  ~PrintStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getExpr(unsigned I) const {
    return static_cast<Expr *>(getOperand(I));
  }
};

// WRITE(unit,format), list
class WriteStmt : public Stmt {
protected:
  llvm::SmallVector<int, 2> spaceList;

  friend class ParseTreeBuilder;

  WriteStmt(ExprList &exprList, SourceLoc _sourceLoc, Expr *_unit,
            Format *format, ConstantVal *advance, Expr *iostat)
      : Stmt(WriteStmtKind, _sourceLoc) {
    operands.push_back(_unit);
    operands.push_back(format);
    operands.push_back(advance);
    operands.push_back(iostat);
    setOperands(exprList);
  };

public:
  constexpr static bool classof(const Stmt *stmt) {
    return stmt->getStmtType() == WriteStmtKind;
  }

  ~WriteStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getExpr(unsigned i) const {
    return static_cast<Expr *>(getOperand(i + 4));
  }

  ExprList getExprList() const {
    ExprList list;
    for (unsigned i = 0; i < getNumOperands() - 4; ++i) {
      list.push_back(getExpr(i));
    }
    return list;
  }

  inline Expr *getUnit() const { return static_cast<Expr *>(operands[0]); }

  inline Format *getFormat() const {
    return static_cast<Format *>(operands[1]);
  }

  inline ConstantVal *getAdvance() const {
    return static_cast<ConstantVal *>(operands[2]);
  }

  inline Expr *getIostat() { return static_cast<Expr *>(operands[3]); }

  void setUnit(Expr *val) { operands[0] = val; }

  void setFormat(Format *format) { operands[1] = format; }

  void setAdvace(ConstantVal *val) { operands[2] = val; }

  void setSpaceList(llvm::SmallVector<int, 2> &list) {
    spaceList.append(list.begin(), list.end());
  }

  void setIostat(Expr *expr) { operands[3] = expr; }

  void getSpaceList(llvm::SmallVector<int, 2> &list) {
    if (!spaceList.empty()) {
      list.append(spaceList.begin(), spaceList.end());
      return;
    }

    unsigned size = getExprList().size();
    for (int i = 0; i < size; ++i) {
      list.push_back(-1);
    }
  }
};

class ReadStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  ReadStmt(ExprList &_exprList, SourceLoc _sourceLoc, Expr *_unit,
           Format *_format, Expr *_iostat)
      : Stmt(ReadStmtKind, _sourceLoc) {
    operands.push_back(_unit);
    operands.push_back(_format);
    operands.push_back(_iostat);
    setOperands(_exprList);
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == ReadStmtKind;
  }

  ~ReadStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getExpr(unsigned I) const {
    return static_cast<Expr *>(getOperand(I + 3));
  }

  ExprList getExprList() const {
    ExprList list;
    for (unsigned I = 0; I < getNumOperands() - 3; ++I) {
      list.push_back(getExpr(I));
    }
    return list;
  }

  inline Expr *getUnit() const { return static_cast<Expr *>(operands[0]); }

  void setUnit(Expr *val) { operands[0] = val; }

  inline Format *getFormat() const {
    return static_cast<Format *>(operands[1]);
  }

  void setFormat(Format *format) { operands[1] = format; }

  inline Expr *getIostat() const { return static_cast<Expr *>(operands[2]); }

  void setIostat(Expr *expr) { operands[2] = expr; }
};

// Currently,
// CALL <sub-Name> <expr-list> is handled.
class CallStmt : public Stmt {
private:
  Symbol *sym;

protected:
  friend class ParseTreeBuilder;

  CallStmt(Symbol *_sym, ExprList &_exprList, SourceLoc _sourceLoc)
      : Stmt(CallStmtKind, _sourceLoc), sym(_sym) {
    setOperands(_exprList);
  };

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == CallStmtKind;
  }

  ~CallStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline void setCalledFn(Symbol *newFn) { sym = newFn; }

  inline Symbol *getCalledFn() const { return sym; }

  inline Expr *getExpr(unsigned I) {
    return static_cast<Expr *>(getOperand(I));
  }

  ExprList getArgsList() const {
    ExprList list;
    for (auto op : operands) {
      list.push_back(static_cast<Expr *>(op));
    }
    return list;
  }
};

// open-stmt is open connect-spec list
class OpenStmt : public Stmt {

private:
  // Status
  StatusKind status;

protected:
  friend class ParseTreeBuilder;

  OpenStmt(SourceLoc _loc, Expr *_unit, Expr *_file, StatusKind _status,
           Expr *_iostat, ExprList &otherSpecs)
      : Stmt(OpenStmtKind, _loc), status(_status) {
    setOperands({_unit, _file, _iostat});
    setOperands(otherSpecs);
  }

public:
  inline constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == OpenStmtKind;
  }

  ~OpenStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getUnit() const { return static_cast<Expr *>(operands[0]); }

  inline Expr *getFile() const { return static_cast<Expr *>(operands[1]); }

  StatusKind getStatus() { return status; }

  Expr *getIostat() const { return static_cast<Expr *>(operands[2]); }

  inline void setUnit(Expr *unit) { operands[0] = unit; }

  inline void setFile(Expr *file) { operands[1] = file; }

  inline void setIostat(Expr *iostat) { operands[2] = iostat; }

  ExprList getConnectSpecList() const {
    ExprList spec;
    for (int i = 3; i < operands.size(); ++i) {
      spec.push_back(static_cast<Expr *>(operands[i]));
    }
    return spec;
  }
};

class ExitStmt : public Stmt {
  llvm::StringRef constructName;

protected:
  friend class ParseTreeBuilder;

  explicit ExitStmt(SourceLoc _loc, llvm::StringRef name)
      : Stmt(ExitStmtKind, _loc), constructName(name) {}

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == ExitStmtKind;
  }

  ~ExitStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline std::string getConstructName() const { return constructName.str(); }

  inline bool hasConstructName() const { return !constructName.empty(); }
};

class ReturnStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  explicit ReturnStmt(Expr *expr, SourceLoc _loc) : Stmt(ReturnStmtKind, _loc) {
    setOperands({expr});
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == ReturnStmtKind;
  }

  ~ReturnStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getExpr() const { return static_cast<Expr *>(operands[0]); }
};

class CycleStmt : public Stmt {
  llvm::StringRef constructName;

protected:
  friend class ParseTreeBuilder;

  explicit CycleStmt(SourceLoc _loc, llvm::StringRef name)
      : Stmt(CycleStmtKind, _loc), constructName(name) {}

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == CycleStmtKind;
  }

  ~CycleStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline std::string getConstructName() const { return constructName.str(); }

  inline bool hasConstructName() const { return !constructName.empty(); }
};

// close-stmt, currently only close ([unit=] expr) is handled
class CloseStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  CloseStmt(SourceLoc _loc, Expr *_unit, Expr *_iostat)
      : Stmt(CloseStmtKind, _loc) {
    setOperands({_unit, _iostat});
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == CloseStmtKind;
  }

  ~CloseStmt() {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  Expr *getUnit() const { return static_cast<Expr *>(operands[0]); }

  void setUnit(Expr *unit) { operands[0] = unit; }

  Expr *getIostat() const { return static_cast<Expr *>(operands[1]); }

  void setIostat(Expr *iostat) { operands[1] = iostat; }
};

class WhereStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;
  explicit WhereStmt(Expr *_maskExpr, Block *whereBlock, SourceLoc _loc)
      : Stmt(WhereStmtKind, _loc) {
    setOperands({_maskExpr, whereBlock});
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == WhereStmtKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getMaskExpr() const { return static_cast<Expr *>(operands[0]); }
  inline Block *getBlock() const { return static_cast<Block *>(operands[1]); }

  ~WhereStmt() {}
};

class WhereConstruct : public Stmt {
private:
  WhereStmtKindList kindList;

protected:
  friend class ParseTreeBuilder;

  WhereConstruct(WhereStmtList &_list, WhereStmtKindList &_kindList,
                 SourceLoc loc)
      : Stmt(WhereConstructKind, loc), kindList(_kindList) {
    for (auto stmt : _list) {
      operands.push_back(stmt);
    }
    setParentForOperands();
  }

public:
  constexpr static bool classof(const Stmt *stmt) {
    return stmt->getStmtType() == WhereConstructKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline WhereStmt *getWhereStmt(unsigned i) const {
    return static_cast<WhereStmt *>(getOperand(i));
  }

  WhereElseConstructKind getKind(unsigned i) { return kindList[i]; }

  ~WhereConstruct() {}
};

class CaseStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  explicit CaseStmt(ExprList &list, Block *block, SourceLoc loc)
      : Stmt(CaseStmtKind, loc) {
    operands.push_back(block);
    for (auto expr : list) {
      operands.push_back(expr);
    }
    setParentForOperands();
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == CaseStmtKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline ExprList getExprList() const {
    ExprList list;
    for (unsigned I = 1; I < operands.size(); ++I) {
      list.push_back(static_cast<Expr *>(operands[I]));
    }
    return list;
  }

  inline Block *getBlock() const { return static_cast<Block *>(operands[0]); }

  ~CaseStmt() {}
};

class SelectCaseStmt : public Stmt {
protected:
  friend class ParseTreeBuilder;

  explicit SelectCaseStmt(Expr *caseExpr, CaseStmtList &list, SourceLoc loc)
      : Stmt(SelectCaseStmtKind, loc) {

    operands.push_back(caseExpr);
    for (auto caseStmt : list) {
      operands.push_back(caseStmt);
    }
    setParentForOperands();
  }

public:
  constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == SelectCaseStmtKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  inline Expr *getSelectExpr() const {
    return static_cast<Expr *>(getOperand(0));
  }

  inline CaseStmt *getCaseStmt(unsigned i) const {
    assert(i > 0);
    return static_cast<CaseStmt *>(getOperand(i));
  }

  inline CaseStmtList getCaseStmtList() const {
    CaseStmtList list;
    for (unsigned I = 1; I < operands.size(); ++I) {
      list.push_back(static_cast<CaseStmt *>(operands[I]));
    }
    return list;
  }

  ~SelectCaseStmt() {}
};

} // namespace ast
} // namespace fc
#endif
