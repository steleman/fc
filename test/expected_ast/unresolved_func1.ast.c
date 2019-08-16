Program: unresolved_func1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (2, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() readtest
}

// MainProgram
int32 readtest() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable readtest {
    // Symbol List: 

    // (3, 12)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, readtest
    int32[1:20] a
    // (4, 10)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, readtest
    real b
    // (5, 5)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, readtest
    (real)(...) count1
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (5, 1)
  b = count1(a())
  // (7, 1)
  printf(a(), b)
}

