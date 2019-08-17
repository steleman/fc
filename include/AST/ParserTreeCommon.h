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
#ifndef FC_PARSER_COMMON_H
#define FC_PARSER_COMMON_H

#include "AST/SymbolTableCommon.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

#include <list>

namespace fc {
struct SourceLoc;
class Type;
class FunctionType;
class ASTContext;
class StructType;
class PointerType;

namespace ast {

class ParseTree;
class Function;
class SpecificationPart;
class DerivedTypeDef;
class AttrSpec;
class DeclarationTypeSpec;
class Expr;
class ExecutionPart;
class StringConstant;
class ProgramUnit;
class IntrinsicTypeSpec;
class DerivedTypeSpec;
class TypeSpec;
class Module;
class Stmt;
class ParseTreeBuilder;

#define STMT(X) class X;
#include "AST/statements.def"

enum ProgramUnitKind {
  ProgramKind,
  MainProgramKind,
  SubroutineKind,
  ModuleKind,
  FunctionKind,
  DerivedTypeDefKind,
};

enum StatusKind { OLD, NEW, SCRATCH, REPLACE, UNKNOWN, undefined };

enum AttrSpecKind {
  None,
  Dimension,
  Intent,
  Intrinsic,
  Parameter,
  Pointer,
  Target,
  Allocatable,
  Save,
  Private,
  Optional,
  Public,
};

enum BinaryOpKind {
  Addition,
  Subtraction,
  Multiplication,
  Division,
  Power,
  Concat, // "operator //"
};

enum RelationalOpKind {
  EQ, // .EQ. | ==
  NE, // .NE. | /=
  LT, // .LT. | <
  LE, // .LE. | <=
  GT, // .GT. | >
  GE  // .GE. | >=
};

enum LogicalOpKind {
  NOT, // .NOT.
  AND, // .AND.
  OR,  // .OR.
  EQV, // .EQV.
  NEQV // .NEQV.
};

enum IfConstructKind {
  IfKind = 1,
  IfThenKind,
  ElseIfKind,
  ElseKind,
};

enum WhereElseConstructKind {
  WhereKind = 1,
  ElseWhereWhereKind,
  ElseWhereKind,
};

enum StmtType {
#define STMT(X) X##Kind,
#include "AST/statements.def"
};

typedef llvm::SmallVector<DerivedTypeDef *, 2> DerivedTypeDefList;
typedef llvm::SmallVector<ConstantVal *, 2> ConstantList;
typedef llvm::SmallVector<IfStmt *, 2> IfStmtList;
typedef llvm::SmallVector<IfConstructKind, 2> IfConstructKindList;
typedef llvm::SmallVector<WhereStmt *, 2> WhereStmtList;
typedef llvm::SmallVector<WhereElseConstructKind, 2> WhereStmtKindList;
typedef llvm::SmallVector<Expr *, 2> ExprList;

struct StructAccessInfo {
  int fieldNum;
  llvm::StringRef name;
  ExprList subsList;
};

typedef llvm::SmallVector<StructAccessInfo, 2> StructInfoList;
typedef std::pair<Expr *, Expr *> DynArrayBounds;

// TODO: use llvm ilist.
typedef std::list<Stmt *> StmtList;
typedef StmtList::iterator stmt_iterator;
typedef llvm::SmallVector<Stmt *, 8> StmtVecList;
typedef llvm::SmallVector<llvm::StringRef, 2> ArgsList;
typedef llvm::SmallVector<AttrSpec *, 2> AttrSpecList;
typedef llvm::SmallVector<EntityDecl *, 2> EntityDeclList;
typedef llvm::SmallVector<Symbol *, 2> SymbolList;
typedef llvm::SmallPtrSet<Symbol *, 8> SymbolSet;
typedef llvm::SmallVector<ArraySpec *, 2> ArraySpecList;
typedef llvm::SmallVector<ProgramUnit *, 2> ProgramUnitList;
typedef llvm::SmallVector<Type *, 2> TypeList;
typedef llvm::SmallVector<DynArrayBounds, 2> DynArrBoundsList;
typedef llvm::SmallVector<UseStmt *, 2> UseStmtList;
typedef llvm::SmallVector<RangeExpr *, 2> SubscriptRangeList;
typedef llvm::SmallVector<CaseStmt *, 2> CaseStmtList;
typedef llvm::SmallVector<Expr *, 2> ACValueList;
typedef llvm::SmallVector<ArraySection *, 2> ArraySectionList;

} // namespace ast
} // namespace fc
#endif
