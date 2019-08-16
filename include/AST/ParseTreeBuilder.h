#ifndef FC_PARSE_TREE_BUILDER_H
#define FC_PARSE_TREE_BUILDER_H

#include "AST/ParserTreeCommon.h"
#include "AST/Type.h"

namespace fc {
namespace ast {

class ParseTreeBuilder {

private:
  ASTContext &Context;

public:
  explicit ParseTreeBuilder(ASTContext &Context) : Context(Context) {}

  Function *buildFunction(ProgramUnitKind, llvm::StringRef &name,
                          ArgsList &exprList, SymbolTable *symTable,
                          SpecificationPart *specPart, ExecutionPart *execPart);

  Function *buildFunction(ProgramUnitKind, llvm::StringRef &name,
                          ArgsList &exprList, SymbolTable *symTable,
                          SpecificationPart *specPart, ExecutionPart *execPart,
                          ProgramUnitList &programList);

  Module *buildModule(llvm::StringRef &name, SymbolTable *symTable,
                      SpecificationPart *specPart,
                      ProgramUnitList &programList);

  SymbolTable *buildSymbolTable(ASTContext &FC, ScopeKind scopeKind,
                                llvm::StringRef name, SymbolTable *parent);

  SpecificationPart *buildSpecificationPart(Block *block);

  DerivedTypeDef *buildDerivedTypeDef(llvm::StringRef name,
                                      SymbolTable *symbolTable,
                                      ProgramUnit *parentPU, Block *block,
                                      StructType *type,
                                      ProgramUnitList &programList);

  ExecutionPart *buildExecutionPart(Block *block);

  Block *buildBlock(StmtList &stmtList, SourceLoc loc);

  IntrinsicTypeSpec *buildIntrinsicTypeSpec(Type *type,
                                            ArraySpec *length = nullptr);

  DerivedTypeSpec *buildDerivedTypeSpec(Type *type, llvm::StringRef &name);

  DeclarationTypeSpec *buildDeclaratoinTypeSpec(TypeSpec *typeSpec,
                                                SourceLoc loc,
                                                Expr *kind = nullptr);

  EntityDecl *buildEntityDecl(Symbol *sym, DeclarationTypeSpec *decl,
                              ArraySpec *arrSpec, Expr *init, SourceLoc loc);

  ConstantVal *buildConstantVal(std::string value, Type *type, SourceLoc loc);

  ConstantVal *buildConstantVal(Constant *constant, SourceLoc loc);

  // build action statments
  AssignmentStmt *buildAssignmentStmt(Expr *lhs, Expr *rhs,
                                      SourceLoc _sourceLoc);

  PointerAssignmentStmt *buildPointerAssignmentStmt(Expr *lhs, Expr *rhs,
                                                    SourceLoc _sourceLoc);

  StopStmt *buildStopStmt(Expr *stopCode, SourceLoc _sourceLoc);

  PrintStmt *buildPrintStmt(ExprList &exprList, SourceLoc _sourceLoc);

  ReadStmt *buildReadStmt(ExprList &exprList, SourceLoc sourceLoc, Expr *unit,
                          Format *format, Expr *iostat);

  Format *buildFormat(ExprList &list, Type *type, SourceLoc sourceLoc);

  WriteStmt *buildWriteStmt(ExprList &exprList, SourceLoc sourceLoc, Expr *unit,
                            Format *format, ConstantVal *advance, Expr *iostat);

  FunctionReference *buildFunctionReference(Symbol *sym, ExprList &list,
                                            Type *type, SourceLoc loc);

  CallStmt *buildCallStmt(Symbol *sym, ExprList &exprList,
                          SourceLoc _sourceLoc);

  CycleStmt *buildCycleStmt(llvm::StringRef name, SourceLoc _sourceLoc);

  ExitStmt *buildExitStmt(llvm::StringRef name, SourceLoc _sourceLoc);

  AssignmentExpr *buildAssignmentExpr(llvm::StringRef name, Expr *expr,
                                      SourceLoc loc);

  UseStmt *buildUseStmt(SymbolTable *modSymbolTable, SourceLoc _sourceLoc,
                        bool isIntrinsic);

  IfStmt *buildIfStmt(Expr *logicalExpr, Block *actionStmt,
                      SourceLoc _sourceLoc);

  WhereStmt *buildWhereStmt(Expr *maskExpr, Block *block, SourceLoc sourceLoc);

  WhereConstruct *buildWhereConstruct(WhereStmtList &_list,
                                      WhereStmtKindList &_kindList,
                                      SourceLoc _loc);

  IfElseStmt *buildIfElseStmt(IfStmtList &list, IfConstructKindList &kindList,
                              SourceLoc _sourceLoc);

  IfElseStmt *buildIfElseStmt(Expr *logicalExpr, Block *actionStmt,
                              SourceLoc _sourceLoc);

  OpenStmt *buildOpenStmt(SourceLoc sourceLoc, Expr *unit, Expr *file,
                          StatusKind status, Expr *iostat, ExprList &specList);

  CloseStmt *buildCloseStmt(SourceLoc sourceLoc, Expr *unit, Expr *iostat);

  DoWhileStmt *buildDoWhileStmt(Expr *logicalExpr, Block *block,
                                SourceLoc _sourceLoc);

  DoStmt *buildDoStmt(QuadExpr *expr, Block *block,
                      llvm::StringRef constructName, SourceLoc _sourceLoc);

  ForAllStmt *buildForAllStmt(ExprList &exprList, Block *block,
                              llvm::StringRef constructName,
                              SourceLoc _sourceLoc);

  ReturnStmt *buildReturnStmt(Expr *expr, SourceLoc _sourceLoc);

  ObjectName *buildObjectName(Symbol *sym, SourceLoc loc);

  StructureComponent *buildStructureComponent(ExprList &partRefs, Type *type,
                                              SourceLoc loc);

  ArrayElement *buildArrayElement(Symbol *sym, Expr *baseExpr,
                                  ExprList &subscripts, SourceLoc loc);

  ArraySection *buildArraySection(Symbol *sym, Expr *baseExpr, ExprList &list,
                                  SourceLoc loc);

  ArraySection *buildArraySection(Symbol *sym, Expr *baseExpr, SourceLoc loc);

  RangeExpr *buildRangeExpr(SourceLoc loc, Expr *lb = nullptr,
                            Expr *ub = nullptr, Expr *index = nullptr);

  BinaryExpr *buildExpr(Expr *op1, Expr *op2, BinaryOpKind opKind, Type *type,
                        SourceLoc loc);

  CastExpr *buildCastExpr(Expr *from, Type *type, SourceLoc loc);

  BinaryExpr *buildAddExpr(Expr *op1, Expr *op2);

  IOImpliedDo *buildIOImpliedDo(ExprList exps, ExprList ioDOs, Type *type,
                                SourceLoc loc);

  QuadExpr *buildQuadExpr(Expr *op[4], Type *type, SourceLoc loc);

  RelationalExpr *buildRelationalExpr(Expr *op1, Expr *op2,
                                      RelationalOpKind opKind, SourceLoc loc);

  LogicalExpr *buildLogicalExpr(Expr *op1, Expr *op2, LogicalOpKind opKind,
                                SourceLoc loc);

  AttrSpec *buildAttrSpec(AttrSpecKind kind, ArraySpec *spec = nullptr);

  ArraySpec *buildArraySpec(DynArrBoundsList &boundsList, unsigned int numDims,
                            SourceLoc loc);

  ArraySpec *buildArraySpec(SourceLoc loc);

  ACSpec *buildACSpec(Type *type, ACValueList &ACVals, SourceLoc loc);

  ArrayConstructor *buildArrayConstructor(ACSpec *Spec, SourceLoc loc);

  AttrSpec *buildIntentSpec(IntentKind kind);

  NullifyStmt *buildNullifyStmt(ExprList &ptrObjList, SourceLoc loc);

  AllocateStmt *buildAllocateStmt(SymbolList &symbolList,
                                  ArraySpecList &arraySpecList,
                                  SourceLoc sourceLoc, Expr *stat = nullptr);

  DeAllocateStmt *buildDeAllocateStmt(SymbolList &symList, SourceLoc loc,
                                      Expr *stat = nullptr);

  CaseStmt *buildCaseStmt(ExprList &list, Block *b, SourceLoc loc);

  SelectCaseStmt *buildSelectCaseStmt(Expr *caseExpr, CaseStmtList &list,
                                      SourceLoc loc);

  llvm::SmallVector<DoStmt *, 2>
  buildLoopNestFor(SymbolList &indvars, ArrBoundsList &boundsList,
                   llvm::SmallVector<Expr *, 2> &strideList, SourceLoc loc);

  FunctionReference *buildLBoundIntrin(Symbol *arr, unsigned dim,
                                       SourceLoc loc);

  FunctionReference *buildUBoundIntrin(Symbol *arr, unsigned dim,
                                       SourceLoc loc);

  FunctionReference *buildSizeIntrin(Symbol *arr, unsigned dim, SourceLoc loc);

  FunctionReference *buildModIntrin(Symbol *arr, Expr *lhs, Expr *rhs,
                                    SourceLoc loc);

  ConstantVal *getConstantOne(SourceLoc loc, Type *ty);

  llvm::SmallVector<DoStmt *, 2>
  buildLoopNestFor(SymbolList &indvars, DynArrBoundsList &boundsList,
                   llvm::SmallVector<Expr *, 2> &strideList, SourceLoc loc);
};
} // namespace ast
} // namespace fc
#endif
