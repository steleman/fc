Program: decl_arr_multi.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_arr_multi
}

// MainProgram
int32 decl_arr_multi() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_arr_multi {
    // Symbol List: 

    // (2, 7)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_arr_multi
    real[3:10, 4:100, 1:10] val1
    // (3, 9)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_arr_multi
    int32[3:10, 1:1003, 10:100] val2
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

