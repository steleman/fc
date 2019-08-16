#include "AST/ParseTreeBuilder.h"
#include "AST/ASTContext.h"
#include "AST/Declaration.h"
#include "AST/Expressions.h"
#include "AST/ParserTreeCommon.h"
#include "AST/ProgramUnit.h"
#include "AST/Statements.h"
#include "AST/Stmt.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Util.h"

using namespace fc;
using namespace ast;

Function *ParseTreeBuilder::buildFunction(ProgramUnitKind kind,
                                          llvm::StringRef &name,
                                          ArgsList &exprList,
                                          SymbolTable *symTable,
                                          SpecificationPart *specPart,
                                          ExecutionPart *execPart) {
  ProgramUnitList list;
  Function *program = new (Context, alignof(Function))
      Function(kind, name, exprList, symTable, specPart, execPart, list);
  return program;
}

Function *ParseTreeBuilder::buildFunction(
    ProgramUnitKind kind, llvm::StringRef &name, ArgsList &exprList,
    SymbolTable *symTable, SpecificationPart *specPart, ExecutionPart *execPart,
    ProgramUnitList &programList) {
  Function *program = new (Context, alignof(Function))
      Function(kind, name, exprList, symTable, specPart, execPart, programList);
  return program;
}

Module *ParseTreeBuilder::buildModule(llvm::StringRef &name,
                                      SymbolTable *symTable,
                                      SpecificationPart *specPart,
                                      ProgramUnitList &programList) {
  Module *module = new (Context, alignof(Module))
      Module(name, symTable, specPart, programList);
  return module;
}

SpecificationPart *ParseTreeBuilder::buildSpecificationPart(Block *block) {
  return new (Context, alignof(SpecificationPart)) SpecificationPart(block);
}

DerivedTypeDef *ParseTreeBuilder::buildDerivedTypeDef(
    llvm::StringRef name, SymbolTable *symbolTable, ProgramUnit *parentPU,
    Block *block, StructType *type, ProgramUnitList &programList) {
  assert(!name.empty() && type);
  return new (Context, alignof(DerivedTypeDef))
      DerivedTypeDef(name, symbolTable, parentPU, block, type, programList);
}

IntrinsicTypeSpec *ParseTreeBuilder::buildIntrinsicTypeSpec(Type *type,
                                                            ArraySpec *length) {
  return new (Context, alignof(IntrinsicTypeSpec))
      IntrinsicTypeSpec(type, length);
}

DerivedTypeSpec *ParseTreeBuilder::buildDerivedTypeSpec(Type *type,
                                                        llvm::StringRef &name) {
  return new (Context, alignof(DerivedTypeSpec)) DerivedTypeSpec(type, name);
}

SymbolTable *ParseTreeBuilder::buildSymbolTable(ASTContext &FC,
                                                ScopeKind scopeKind,
                                                llvm::StringRef name,
                                                SymbolTable *parent) {
  return SymbolTable::get(FC, scopeKind, name, parent);
}

DeclarationTypeSpec *
ParseTreeBuilder::buildDeclaratoinTypeSpec(TypeSpec *typeSpec, SourceLoc loc,
                                           Expr *kind) {
  return new (Context, alignof(DeclarationTypeSpec))
      DeclarationTypeSpec(typeSpec, loc, kind);
}

EntityDecl *ParseTreeBuilder::buildEntityDecl(Symbol *sym,
                                              DeclarationTypeSpec *decl,
                                              ArraySpec *arrSpec, Expr *init,
                                              SourceLoc loc) {
  return new (Context, alignof(EntityDecl))
      EntityDecl(sym, decl, arrSpec, init, loc);
}

ConstantVal *ParseTreeBuilder::buildConstantVal(std::string val, Type *type,
                                                SourceLoc loc) {
  assert(type);

  auto constant = Constant::Create(val, type);
  assert(constant);
  return new (Context, alignof(ConstantVal)) ConstantVal(constant, loc);
}

ConstantVal *ParseTreeBuilder::buildConstantVal(Constant *constant,
                                                SourceLoc loc) {
  assert(constant);
  return new (Context, alignof(ConstantVal)) ConstantVal(constant, loc);
}

// AttrSpec
AttrSpec *ParseTreeBuilder::buildAttrSpec(AttrSpecKind kind, ArraySpec *spec) {
  return new (Context, alignof(AttrSpec)) AttrSpec(kind, spec);
}

AttrSpec *ParseTreeBuilder::buildIntentSpec(IntentKind kind) {
  return new (Context, alignof(AttrSpec)) AttrSpec(AttrSpecKind::Intent, kind);
}

AssignmentStmt *ParseTreeBuilder::buildAssignmentStmt(Expr *lhs, Expr *rhs,
                                                      SourceLoc _sourceLoc) {
  return new (Context, alignof(AssignmentStmt))
      AssignmentStmt(lhs, rhs, _sourceLoc);
}

PointerAssignmentStmt *
ParseTreeBuilder::buildPointerAssignmentStmt(Expr *lhs, Expr *rhs,
                                             SourceLoc _sourceLoc) {
  return new (Context, alignof(PointerAssignmentStmt))
      PointerAssignmentStmt(lhs, rhs, _sourceLoc);
}

CastExpr *ParseTreeBuilder::buildCastExpr(Expr *from, Type *type,
                                          SourceLoc loc) {
  return new (Context, alignof(CastExpr)) CastExpr(from, type, loc);
}

BinaryExpr *ParseTreeBuilder::buildExpr(Expr *op1, Expr *op2,
                                        BinaryOpKind opKind, Type *type,
                                        SourceLoc loc) {
  auto Ty = op2->getType();
  if (Ty->isArrayTy()) {
    Ty = static_cast<ArrayType *>(Ty)->getElementTy();
  }
  if (!op1 && op2 && opKind == BinaryOpKind::Subtraction) {
    if (Ty->isUndeclaredFnTy())
      Ty = Type::getRealTy(type->getContext());
    op1 = buildConstantVal("0", Ty, loc);
  }
  return new (Context, alignof(BinaryExpr))
      BinaryExpr(op1, op2, opKind, type, loc);
}

QuadExpr *ParseTreeBuilder::buildQuadExpr(Expr *op[4], Type *type,
                                          SourceLoc loc) {

  return new (Context, alignof(QuadExpr)) QuadExpr(op, type, loc);
}

ObjectName *ParseTreeBuilder::buildObjectName(Symbol *sym, SourceLoc loc) {
  return new (Context, alignof(ObjectName)) ObjectName(sym, loc);
}

StructureComponent *
ParseTreeBuilder::buildStructureComponent(ExprList &partRefs, Type *type,
                                          SourceLoc loc) {
  return new (Context, alignof(ObjectName))
      StructureComponent(partRefs, type, loc);
}

ExecutionPart *ParseTreeBuilder::buildExecutionPart(Block *block) {
  return new (Context, alignof(ExecutionPart)) ExecutionPart(block);
}

Block *ParseTreeBuilder::buildBlock(StmtList &stmtList, SourceLoc loc) {
  return new (Context, alignof(Block)) Block(stmtList, loc);
}

ArrayElement *ParseTreeBuilder::buildArrayElement(Symbol *sym, Expr *baseExpr,
                                                  ExprList &subscrs,
                                                  SourceLoc loc) {
  assert(sym && sym->getType() &&
         (sym->getType()->isArrayTy() || sym->getType()->isUndeclared() ||
          sym->getType()->isCharacterTy()) &&
         "not a array type");
  return new (Context, alignof(ArrayElement))
      ArrayElement(sym, baseExpr, subscrs, loc);
}

ArraySection *ParseTreeBuilder::buildArraySection(Symbol *sym, Expr *baseExpr,
                                                  ExprList &list,
                                                  SourceLoc loc) {
  assert(sym);
  Type *type = sym->getType();

  // There's no "array of pointers" in Fortran (call me if there's :P), only
  // "pointer to array", ie in C-terms, there's no "int *ptrs[...]", only
  // "int (*ptr)[...]".
  if (auto ptrType = llvm::dyn_cast<PointerType>(type))
    type = ptrType->getElementType();

  assert(type && (type->isArrayTy() || type->isUndeclaredTy()) &&
         "Not a array type");
  return new (Context, alignof(ArraySection))
      ArraySection(sym, baseExpr, list, loc);
}

ArraySection *ParseTreeBuilder::buildArraySection(Symbol *sym, Expr *baseExpr,
                                                  SourceLoc loc) {
  assert(sym && sym->getType() &&
         (sym->getType()->isArrayTy() || sym->getType()->isUndeclaredTy()) &&
         "Not a array type");
  return new (Context, alignof(ArraySection)) ArraySection(sym, baseExpr, loc);
}

RangeExpr *ParseTreeBuilder::buildRangeExpr(SourceLoc loc, Expr *lb, Expr *ub,
                                            Expr *index) {
  return new (Context, alignof(RangeExpr))
      RangeExpr(lb, ub, index, Type::getUndeclaredTy(Context), loc);
}

StopStmt *ParseTreeBuilder::buildStopStmt(Expr *stopCode,
                                          SourceLoc _sourceLoc) {
  // assert(stopCode->getType()->isIntegralTy() && "Stop code should be int");
  return new (Context, alignof(StopStmt)) StopStmt(stopCode, _sourceLoc);
}

RelationalExpr *ParseTreeBuilder::buildRelationalExpr(Expr *op1, Expr *op2,
                                                      RelationalOpKind opKind,
                                                      SourceLoc loc) {
  return new (Context, alignof(RelationalExpr))
      RelationalExpr(op1, op2, opKind, Type::getLogicalTy(Context), loc);
}

IOImpliedDo *ParseTreeBuilder::buildIOImpliedDo(ExprList exprs, ExprList ioDOs,
                                                Type *type, SourceLoc loc) {
  return new (Context, alignof(IOImpliedDo))
      IOImpliedDo(exprs, ioDOs, type, loc);
}

LogicalExpr *ParseTreeBuilder::buildLogicalExpr(Expr *op1, Expr *op2,
                                                LogicalOpKind kind,
                                                SourceLoc loc) {
  return new (Context, alignof(LogicalExpr))
      LogicalExpr(op1, op2, kind, Type::getLogicalTy(Context), loc);
}

PrintStmt *ParseTreeBuilder::buildPrintStmt(ExprList &exprList,
                                            SourceLoc _sourceLoc) {
  return new (Context, alignof(PrintStmt)) PrintStmt(exprList, _sourceLoc);
}

ReadStmt *ParseTreeBuilder::buildReadStmt(ExprList &exprList,
                                          SourceLoc sourceLoc, Expr *constant,
                                          Format *format, Expr *iostat) {
  return new (Context, alignof(ReadStmt))
      ReadStmt(exprList, sourceLoc, constant, format, iostat);
}

Format *ParseTreeBuilder::buildFormat(ExprList &exprList, Type *type,
                                      SourceLoc loc) {
  return new (Context, alignof(Format)) Format(exprList, type, loc);
}

WriteStmt *ParseTreeBuilder::buildWriteStmt(ExprList &exprList,
                                            SourceLoc sourceLoc, Expr *unit,
                                            Format *format,
                                            ConstantVal *advance,
                                            Expr *iostat) {
  return new (Context, alignof(WriteStmt))
      WriteStmt(exprList, sourceLoc, unit, format, advance, iostat);
}

OpenStmt *ParseTreeBuilder::buildOpenStmt(SourceLoc sourceLoc, Expr *unit,
                                          Expr *file, StatusKind status,
                                          Expr *iostat, ExprList &specList) {

  assert(unit->getType()->isIntegralTy() && "Unit should be integer");

  assert((file->getType()->isStringArrTy() ||
          file->getType()->isUndeclaredFnTy() ||
          file->getType()->isUndeclaredTy()) &&
         "File should be string type");
  return new (Context, alignof(OpenStmt))
      OpenStmt(sourceLoc, unit, file, status, iostat, specList);
}

CloseStmt *ParseTreeBuilder::buildCloseStmt(SourceLoc sourceLoc, Expr *unit,
                                            Expr *iostat) {
  return new (Context, alignof(CloseStmt)) CloseStmt(sourceLoc, unit, iostat);
}

CallStmt *ParseTreeBuilder::buildCallStmt(Symbol *sym, ExprList &exprList,
                                          SourceLoc _sourceLoc) {
  return new (Context, alignof(CallStmt)) CallStmt(sym, exprList, _sourceLoc);
}

FunctionReference *ParseTreeBuilder::buildFunctionReference(Symbol *sym,
                                                            ExprList &list,
                                                            Type *type,
                                                            SourceLoc loc) {
  return new (Context, alignof(FunctionReference))
      FunctionReference(sym, list, type, loc);
}

UseStmt *ParseTreeBuilder::buildUseStmt(SymbolTable *modSymbolTable,
                                        SourceLoc _sourceLoc,
                                        bool isIntrinsic) {
  return new (Context, alignof(UseStmt))
      UseStmt(modSymbolTable, _sourceLoc, isIntrinsic);
}

IfElseStmt *ParseTreeBuilder::buildIfElseStmt(IfStmtList &list,
                                              IfConstructKindList &kindList,
                                              SourceLoc _sourceLoc) {
  return new (Context, alignof(IfElseStmt))
      IfElseStmt(list, kindList, _sourceLoc);
}

IfElseStmt *ParseTreeBuilder::buildIfElseStmt(Expr *logicalExpr,
                                              Block *actionStmt,
                                              SourceLoc _sourceLoc) {
  auto ifStmt = buildIfStmt(logicalExpr, actionStmt, _sourceLoc);

  IfStmtList ifList{ifStmt};
  IfConstructKindList kindList{IfKind};
  return buildIfElseStmt(ifList, kindList, _sourceLoc);
}

IfStmt *ParseTreeBuilder::buildIfStmt(Expr *logicalExpr, Block *actionStmt,
                                      SourceLoc _sourceLoc) {
  return new (Context, alignof(IfStmt))
      IfStmt(logicalExpr, actionStmt, _sourceLoc);
}

WhereStmt *ParseTreeBuilder::buildWhereStmt(Expr *maskExpr, Block *block,
                                            SourceLoc loc) {
  return new (Context, alignof(WhereStmt)) WhereStmt(maskExpr, block, loc);
}

WhereConstruct *ParseTreeBuilder::buildWhereConstruct(
    WhereStmtList &list, WhereStmtKindList &kindList, SourceLoc loc) {
  return new (Context, alignof(WhereConstruct))
      WhereConstruct(list, kindList, loc);
}

DoWhileStmt *ParseTreeBuilder::buildDoWhileStmt(Expr *logicalExpr,
                                                Block *actionStmt,
                                                SourceLoc _sourceLoc) {
  return new (Context, alignof(DoWhileStmt))
      DoWhileStmt(logicalExpr, actionStmt, _sourceLoc);
}

DoStmt *ParseTreeBuilder::buildDoStmt(QuadExpr *expr, Block *block,
                                      llvm::StringRef constructName,
                                      SourceLoc _sourceLoc) {
  return new (Context, alignof(DoStmt))
      DoStmt(expr, block, constructName, _sourceLoc);
}

ForAllStmt *ParseTreeBuilder::buildForAllStmt(ExprList &exprList, Block *block,
                                              llvm::StringRef constructName,
                                              SourceLoc _sourceLoc) {
  return new (Context, alignof(ForAllStmt))
      ForAllStmt(exprList, block, constructName, _sourceLoc);
}

ReturnStmt *ParseTreeBuilder::buildReturnStmt(Expr *expr,
                                              SourceLoc _sourceLoc) {
  return new (Context, alignof(ReturnStmt)) ReturnStmt(expr, _sourceLoc);
}

ACSpec *ParseTreeBuilder::buildACSpec(Type *type, ACValueList &ACVals,
                                      SourceLoc loc) {
  return new (Context, alignof(ACSpec)) ACSpec(type, ACVals, loc);
}

ArrayConstructor *ParseTreeBuilder::buildArrayConstructor(ACSpec *Spec,
                                                          SourceLoc loc) {
  return new (Context, alignof(ArrayConstructor)) ArrayConstructor(Spec, loc);
}

ArraySpec *ParseTreeBuilder::buildArraySpec(DynArrBoundsList &boundsList,
                                            unsigned int numDims,
                                            SourceLoc loc) {
  return new (Context, alignof(ArraySpec))
      ArraySpec(boundsList, numDims, Type::getUndeclaredTy(Context), loc);
}

ArraySpec *ParseTreeBuilder::buildArraySpec(SourceLoc loc) {
  return new (Context, alignof(ArraySpec))
      ArraySpec(Type::getUndeclaredTy(Context), loc);
}

NullifyStmt *ParseTreeBuilder::buildNullifyStmt(ExprList &ptrObjList,
                                                SourceLoc loc) {
  return new (Context, alignof(NullifyStmt)) NullifyStmt(ptrObjList, loc);
}

AllocateStmt *ParseTreeBuilder::buildAllocateStmt(SymbolList &symbolList,
                                                  ArraySpecList &arraySpecList,
                                                  SourceLoc sourceLoc,
                                                  Expr *stat) {
  return new (Context, alignof(AllocateStmt))
      AllocateStmt(symbolList, arraySpecList, sourceLoc, stat);
}

DeAllocateStmt *ParseTreeBuilder::buildDeAllocateStmt(SymbolList &symbolList,
                                                      SourceLoc sourceLoc,
                                                      Expr *stat) {
  return new (Context, alignof(DeAllocateStmt))
      DeAllocateStmt(symbolList, sourceLoc, stat);
}

CaseStmt *ParseTreeBuilder::buildCaseStmt(ExprList &list, Block *b,
                                          SourceLoc loc) {
  return new (Context, alignof(CaseStmt)) CaseStmt(list, b, loc);
}

SelectCaseStmt *ParseTreeBuilder::buildSelectCaseStmt(Expr *caseExpr,
                                                      CaseStmtList &list,
                                                      SourceLoc loc) {
  return new (Context, alignof(SelectCaseStmt))
      SelectCaseStmt(caseExpr, list, loc);
}

CycleStmt *ParseTreeBuilder::buildCycleStmt(llvm::StringRef name,
                                            SourceLoc _sourceLoc) {
  return new (Context, alignof(CycleStmt)) CycleStmt(_sourceLoc, name);
}

AssignmentExpr *ParseTreeBuilder::buildAssignmentExpr(llvm::StringRef name,
                                                      Expr *expr,
                                                      SourceLoc loc) {
  return new (Context, alignof(AssignmentExpr)) AssignmentExpr(name, expr, loc);
}

ExitStmt *ParseTreeBuilder::buildExitStmt(llvm::StringRef name,
                                          SourceLoc _sourceLoc) {
  return new (Context, alignof(ExitStmt)) ExitStmt(_sourceLoc, name);
}

// Returns the loop nest with top-level in the front.
llvm::SmallVector<DoStmt *, 2> ParseTreeBuilder::buildLoopNestFor(
    SymbolList &indvars, ArrBoundsList &boundsList,
    llvm::SmallVector<Expr *, 2> &strideList, SourceLoc loc) {
  DoStmt *doStmt = nullptr;
  llvm::SmallVector<DoStmt *, 2> doList;
  auto numDims = indvars.size();

  for (unsigned I = 0; I < numDims; ++I) {
    auto var = buildObjectName(indvars[I], loc);
    auto lb = buildConstantVal(std::to_string(boundsList[I].first),
                               Type::getInt32Ty(Context), loc);
    auto ub = buildConstantVal(std::to_string(boundsList[I].second),
                               Type::getInt32Ty(Context), loc);
    auto stride = strideList[I];

    Expr *ops[4] = {var, lb, ub, stride};
    auto quadExpr = buildQuadExpr(ops, var->getType(), loc);
    StmtList list;
    auto block = buildBlock(list, loc);
    auto currDo = buildDoStmt(quadExpr, block, "", loc);

    if (I == 0) {
      doStmt = currDo;
    } else {
      doStmt->getBlock()->addStmt(currDo);
      doStmt = currDo;
    }
    doList.push_back(currDo);
  }
  return doList;
}

FunctionReference *
ParseTreeBuilder::buildLBoundIntrin(Symbol *arr, unsigned dim, SourceLoc loc) {

  auto arrSec = buildArraySection(arr, buildObjectName(arr, loc), loc);
  auto dimExpr =
      buildConstantVal(std::to_string(dim), Type::getInt32Ty(Context), loc);

  auto symTable = arr->getSymTable();

  auto Int32Ty = Type::getInt32Ty(Context);
  TypeList list;
  // list.push_back(PointerType::get(Context, Int32Ty));
  // list.push_back(Int32Ty);
  list.push_back(Type::getVarArgTy(Context));
  auto FuncTy = FunctionType::get(Context, Int32Ty, list);
  auto lbound = symTable->getOrInsertSymbol("lbound", FuncTy, loc);
  ExprList argList;
  argList.push_back(arrSec);
  argList.push_back(dimExpr);
  auto funcReference =
      buildFunctionReference(lbound, argList, Type::getInt32Ty(Context), loc);
  return funcReference;
}

FunctionReference *
ParseTreeBuilder::buildUBoundIntrin(Symbol *arr, unsigned dim, SourceLoc loc) {
  auto arrSec = buildArraySection(arr, buildObjectName(arr, loc), loc);
  auto dimExpr =
      buildConstantVal(std::to_string(dim), Type::getInt32Ty(Context), loc);

  auto Int32Ty = Type::getInt32Ty(Context);
  TypeList list;
  // list.push_back(PointerType::get(Context, Int32Ty));
  // list.push_back(Int32Ty);
  list.push_back(Type::getVarArgTy(Context));
  auto FuncTy = FunctionType::get(Context, Int32Ty, list);
  auto symTable = arr->getSymTable();
  auto lbound = symTable->getOrInsertSymbol("ubound", FuncTy, loc);
  ExprList argList;
  argList.push_back(arrSec);
  argList.push_back(dimExpr);
  auto funcReference =
      buildFunctionReference(lbound, argList, Type::getInt32Ty(Context), loc);
  return funcReference;
}

FunctionReference *ParseTreeBuilder::buildSizeIntrin(Symbol *arr, unsigned dim,
                                                     SourceLoc loc) {
  auto arrSec = buildArraySection(arr, buildObjectName(arr, loc), loc);
  auto dimExpr =
      buildConstantVal(std::to_string(dim), Type::getInt32Ty(Context), loc);

  auto Int32Ty = Type::getInt32Ty(Context);
  TypeList list;
  list.push_back(Type::getVarArgTy(Context));
  auto FuncTy = FunctionType::get(Context, Int32Ty, list);
  auto symTable = arr->getSymTable();
  auto lbound = symTable->getOrInsertSymbol("size", FuncTy, loc);
  ExprList argList;
  argList.push_back(arrSec);
  argList.push_back(dimExpr);
  auto funcReference =
      buildFunctionReference(lbound, argList, Type::getInt32Ty(Context), loc);
  return funcReference;
}

FunctionReference *ParseTreeBuilder::buildModIntrin(Symbol *arr, Expr *lhs,
                                                    Expr *rhs, SourceLoc loc) {
  auto Int32Ty = Type::getInt32Ty(Context);
  TypeList list;
  list.push_back(Type::getVarArgTy(Context));
  auto FuncTy = FunctionType::get(Context, Int32Ty, list);
  auto symTable = arr->getSymTable();
  auto lbound = symTable->getOrInsertSymbol("mod", FuncTy, loc);
  ExprList argList;
  argList.push_back(lhs);
  argList.push_back(rhs);
  auto funcReference =
      buildFunctionReference(lbound, argList, Type::getInt32Ty(Context), loc);
  return funcReference;
}

ConstantVal *ParseTreeBuilder::getConstantOne(SourceLoc loc, Type *ty) {
  return buildConstantVal("1", ty, loc);
}

// Same as above, but loop bounds are dynamic
llvm::SmallVector<DoStmt *, 2> ParseTreeBuilder::buildLoopNestFor(
    SymbolList &indvars, DynArrBoundsList &boundsList,
    llvm::SmallVector<Expr *, 2> &strideList, SourceLoc loc) {
  DoStmt *doStmt = nullptr;
  llvm::SmallVector<DoStmt *, 2> doList;
  auto numDims = indvars.size();

  for (unsigned I = 0; I < numDims; ++I) {
    auto var = buildObjectName(indvars[I], loc);
    auto lb = boundsList[I].first;
    auto ub = boundsList[I].second;
    auto stride = strideList[I];

    Expr *ops[4] = {var, lb, ub, stride};
    auto quadExpr = buildQuadExpr(ops, var->getType(), loc);
    StmtList list;
    auto block = buildBlock(list, loc);
    auto currDo = buildDoStmt(quadExpr, block, "", loc);

    if (I == 0) {
      doStmt = currDo;
    } else {
      doStmt->getBlock()->addStmt(currDo);
      doStmt = currDo;
    }
    doList.push_back(currDo);
  }
  return doList;
}

BinaryExpr *ParseTreeBuilder::buildAddExpr(Expr *op1, Expr *op2) {
  return buildExpr(op1, op2, BinaryOpKind::Addition, op2->getType(),
                   op2->getSourceLoc());
}
