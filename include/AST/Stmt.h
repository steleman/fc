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
