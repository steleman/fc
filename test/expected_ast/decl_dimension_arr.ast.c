Program: decl_dimension_arr.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_dim_arr
}

// MainProgram
int32 decl_dim_arr() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_dim_arr {
    // Symbol List: 

    // (2, 42)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_dim_arr
    int32[1:10, -1:100, 3:10] g
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

