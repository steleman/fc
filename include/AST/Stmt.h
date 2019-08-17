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
#ifndef FC_AST_STMT_H
#define FC_AST_STMT_H

#include "AST/ParserTreeCommon.h"
#include "common/Source.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Casting.h"

namespace fc {
namespace ast {

class Stmt {

protected:
  struct ParentNode {
    Stmt *parent;
    int index;

    ParentNode() : parent(nullptr), index(-1) {}
    ParentNode(Stmt *parent, int index) : parent(parent), index(index) {}
  };

  StmtVecList operands;
  StmtType stmtType;
  SourceLoc sourceLoc;
  bool isExpr;
  bool isInvalid;

  ParentNode parentNode;

  void setOperands(llvm::ArrayRef<Stmt *> ops);

  void setOperands(ExprList &list);

  void setParentForOperands();

  explicit Stmt(StmtType stmtType, SourceLoc _sourceLoc, bool isExpr = false)
      : stmtType(stmtType), sourceLoc(_sourceLoc), isExpr(isExpr),
        isInvalid(false) {}

public:
  inline SourceLoc getSourceLoc() { return sourceLoc; }

  SourceLoc getSourceLoc() const { return sourceLoc; }

  constexpr StmtType getStmtType() const { return stmtType; }

  virtual std::string dump(llvm::raw_ostream &OS, int level = 0) const = 0;

  constexpr Stmt *getParent() const { return parentNode.parent; }

  inline ParentNode getParentNode() const { return parentNode; }

  constexpr bool isExpression() const { return isExpr; }

  void setParentNode(ParentNode parent, bool updateParent = false);

  Block *getParentBlock();

  StmtVecList &getOperands() { return operands; }

  void replaceWith(Stmt *stmt);

  void setOperand(Stmt *stmt, int index);

  inline Stmt *getOperand(unsigned I) const {
    assert(I >= 0 && I < operands.size());
    return operands[I];
  }

  void markAsInvalid() { isInvalid = true; }

  bool isInValid() { return isInvalid; }

  constexpr bool isMarkedInvalid() { return isInvalid; }

  typedef StmtVecList::iterator op_iterator;

  op_iterator begin() { return operands.begin(); }

  op_iterator end() { return operands.end(); }

  void eraseFromParent();

  inline unsigned getNumOperands() const { return operands.size(); }

  virtual ~Stmt() {}
}; // namespace ast

} // namespace ast
} // namespace fc

#endif
