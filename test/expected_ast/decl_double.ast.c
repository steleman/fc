Program: decl_double.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_double
}

// MainProgram
int32 decl_double() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_double {
    // Symbol List: 

    // (2, 21)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_double
    double i
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

