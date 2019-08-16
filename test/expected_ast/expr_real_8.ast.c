Program: expr_real_8.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() foo
}

// MainProgram
int32 foo() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable foo {
    // Symbol List: 

    // (2, 24)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, foo
    double x
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

