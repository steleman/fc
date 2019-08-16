Program: decl_allocatable.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_alloca
}

// MainProgram
int32 decl_alloca() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_alloca {
    // Symbol List: 

    // (2, 51)
    // ID: 2, NonConstant, Allocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_alloca
    int64[1:10, 1:20] i
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

