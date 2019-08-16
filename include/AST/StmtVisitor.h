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
