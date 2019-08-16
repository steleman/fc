Program: decl_arr_2d.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_arr_2d
}

// MainProgram
int32 decl_arr_2d() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_arr_2d {
    // Symbol List: 

    // (2, 7)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_arr_2d
    real[3:10, -4:100] val
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

