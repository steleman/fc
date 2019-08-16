Program: decl_arr_upper.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_arr
}

// MainProgram
int32 decl_arr() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_arr {
    // Symbol List: 

    // (2, 12)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_arr
    int32[1:10] g
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

