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
#ifndef FC_PARSE_TREE_VISITOR_H
#define FC_PARSE_TREE_VISITOR_H

#include "AST/Declaration.h"
#include "AST/Expressions.h"

#include "AST/ParserTreeCommon.h"
#include "AST/Statements.h"
#include "ProgramUnit.h"

namespace fc {
namespace ast {

// Top down traversal of a Statement.
template <class SubClass, class RetTy = bool> class StmtVisitor {
public:
#define STMT(STMT_NODE)                                                        \
  virtual RetTy post##STMT_NODE(STMT_NODE *stmt) { return true; }              \
  virtual RetTy visit##STMT_NODE(STMT_NODE *stmt) {                            \
    for (auto op : stmt->getOperands()) {                                      \
      (static_cast<SubClass *>(this))->visit(op);                              \
    }                                                                          \
    return (static_cast<SubClass *>(this))->post##STMT_NODE(stmt);             \
  }
#include "AST/statements.def"

  virtual RetTy visit(Stmt *stmt) {
    if (!stmt)
      return true;
    if (stmt->isInValid())
      return true;
    switch (stmt->getStmtType()) {
#define STMT(NODE)                                                             \
  case NODE##Kind:                                                             \
    return (static_cast<SubClass *>(this))                                     \
        ->visit##NODE(static_cast<NODE *>(stmt));
#include "AST/statements.def"
    };
    return false;
  }

}; // namespace ast
} // namespace ast
} // namespace fc
#endif
