Program: constprop2.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() test
}

// MainProgram
int32 test() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable test {
    // Symbol List: 

    // (2, 25)
    // ID: 2, Constant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 k
    // (3, 33)
    // ID: 3, Constant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    double pi
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 25)
  k = 8
  // (3, 33)
  pi = 3.142
  // (4, 3)
  printf(3.142)
}

