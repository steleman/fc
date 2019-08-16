Program: decl_arr_1d.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_arr_1d
}

// MainProgram
int32 decl_arr_1d() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_arr_1d {
    // Symbol List: 

    // (2, 12)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_arr_1d
    int32[3:10] g
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

