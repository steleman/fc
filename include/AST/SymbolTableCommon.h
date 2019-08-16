#ifndef FC_AST_COMMON_H
#define FC_AST_COMMON_H

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace fc {
class Symbol;
class SymbolTable;
struct SymbolAttributes;

enum IntentKind {
  Intent_None = 0,
  InOut, // Default if intent is specified
  In,
  Out,
};

enum AllocationKind {
  Alloc_None = 0,
  StaticLocal,  // in stack
  StaticGlobal, // in global space
  Argument,     // mostly, no allocation just getting the right argument.
};

enum LinkageKind {
  Link_None = 0,
  Link_Extern = 1,
  Link_Internal = 2,
};

enum ScopeKind {
  GlobalScope = 0,
  MainProgramScope,
  SubroutineScope,
  ModuleScope,
  FunctionScope,
};

typedef llvm::SmallVector<Symbol *, 2> SymbolList;
typedef llvm::SmallVector<SymbolTable *, 2> SymbolTableList;
typedef llvm::SmallPtrSet<SymbolTable *, 2> SymbolTableSet;
} // namespace fc
#endif