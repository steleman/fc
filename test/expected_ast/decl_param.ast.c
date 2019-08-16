Program: decl_param.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_param
}

// MainProgram
int32 decl_param() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_param {
    // Symbol List: 

    // (2, 32)
    // ID: 2, Constant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_param
    double i
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

