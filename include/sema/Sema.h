#ifndef FC_SEMA_H
#define FC_SEMA_H

#include "AST/ParserTreeCommon.h"
#include "common/Diagnostics.h"

namespace fc {
class ASTContext;
class TypeUpdaterPass;

namespace ast {
class ASTPass;
}

using namespace ast;

class Sema {
  std::unique_ptr<ParseTree> &parseTree;

public:
  explicit Sema(std::unique_ptr<ParseTree> &parseTree) : parseTree(parseTree) {}
  bool run();
};

// Pass to update the various types in symbol table
// like array types, intent arguments, etc.
ASTPass *createTypeUpdaterPass(ASTContext &C, bool onlyUpdateSpec = false);

// Pass to adjust the Expression types. Implicit type conversions, etc.
ASTPass *createExprTypeUpdaterPass(ASTContext &C);

// Pass to resolve variables, function calls, module accesses, etc.
ASTPass *createSymbolResolverPass(ASTContext &C,
                                  bool createUndeclaredFunction = true);

// Pass to evaluate and propagate all the constants expressions.
ASTPass *createConstPropPass(ASTContext &C);

// Pass to print the error for unresolved symbols.
ASTPass *createUndeclaredErrorPrinter(ASTContext &C);

// Pass to emit the module files.
ASTPass *createUseStmtHandlerPass(ASTContext &C);

// Update all the types after the symbol resolver pass.
ASTPass *createStmtTypeUpdaterPass(ASTContext &C);

// Pass to expand the Array Section Statements.
ASTPass *createArraySecExpanderPass(ASTContext &C);

// Pass to expand the Where Statement
ASTPass *createConstructExpanderPass(ASTContext &C);

// Pass to expand intrinics.
ASTPass *createIntrinsicExpanderPass(ASTContext &C);

// Pass to expand lbound/ubound intrinsics for static size arrays.
ASTPass *createArrBoundsIntrinExpanderPass(ASTContext &C);

ASTPass *createFunctionTypeUpdaterPass(ASTContext &C);

ASTPass *createParamConstPropPass(ASTContext &C);

// Pass to convert entity-decl to assignment statements
// in non-module program-units.
ASTPass *createDeclEliminatorPass(ASTContext &C);

ASTPass *createFormatHandlerPass(ASTContext &C);

ASTPass *createBinaryOpHandlerPass(ASTContext &C);

ASTPass *createModFileDumperPass(ASTContext &C);

// Pass to replace non full range arraysections in function call
ASTPass *createArraySectionReplacerPass(ASTContext &C);

// Pass to bind symbols in part-refs to correct symtab (after use-stmt handler)
ASTPass *createPartRefSymResolverPass(ASTContext &C);
} // namespace fc

#endif
