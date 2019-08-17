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
