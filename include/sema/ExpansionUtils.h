#include "AST/ASTContext.h"
#include "AST/ASTPass.h"

#include "AST/ParseTreeBuilder.h"
#include "AST/StmtVisitor.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"
#include "sema/Intrinsics.h"

#include <unordered_map>

namespace fc {

namespace ast {

// Class to hold the info about the range (can be RangeExpr or
// ArrayConstructor).
struct RangeInfo {
  // Bounds of the range.
  DynArrayBounds bounds{nullptr, nullptr};
  // Stride for the range.
  Expr *stride{nullptr};
  // Indvar used to iterator over range.
  ObjectName *indvar{nullptr};

  constexpr bool isRange() const { return stride != nullptr; }
};

typedef llvm::SmallVector<RangeInfo, 4> RangeInfoList;

struct ExprRangeInfo {
  RangeInfoList list;

  // Final expression that this Expression takes.
  // 1. ArrayElement for ArraySection
  // 2. ObjectName for ArrayConstructor
  // 3. Same value copied for other scalars.
  Expr *finalExpr;
};

typedef std::unordered_map<Expr *, ExprRangeInfo> ExprRangeInfoMap;

class ArraySectionHelper {
  ParseTreeBuilder builder;
  SymbolTable *currSymTable;
  ASTContext &C;

public:
  ArraySectionHelper(ParseTreeBuilder _builder, SymbolTable *_symTable,
                     ASTContext &_C)
      : builder(_builder), currSymTable(_symTable), C(_C) {}

  ArrayElement *getArrayElementFor(ArraySection *arrSec, RangeInfoList &list);

  void computeArrSecRangeInfo(ArraySection *arrSec, RangeInfoList &infoList,
                              bool createIndVar = true);

  bool collectRangeInfoListFor(Expr *expr, ExprRangeInfoMap &map,
                               bool &hasRange);

  bool getLoopNestFor(RangeInfoList &list,
                      llvm::SmallVector<DoStmt *, 2> &doStmtList);

  // Insert the indvars for other ranges in other Expressions.
  bool insertOtherRangeInfo(ExprRangeInfoMap &map, Expr *referenceExpr,
                            llvm::SmallVector<DoStmt *, 2> &doStmtList,
                            StmtVecList &topList, int ignoreDim = -1,
                            llvm::ArrayRef<Symbol *> tempSymList = {});

  // dimVal is the dimension to be excluded in this array.
  Symbol *getTempArray(ArraySection *referenceArray, Type *arrBaseEleTy,
                       StmtVecList &newStmtList, int dimVal = -1,
                       bool isTranspose = false);

  Symbol *getTempArray(ArraySpec *spec, unsigned numDims, Type *elementTy,
                       StmtVecList &newStmtList);
};

} // namespace ast
} // end of namespace fc
