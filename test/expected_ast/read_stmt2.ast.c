Program: read_stmt2.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() readtest
}

// MainProgram
int32 readtest() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable readtest {
    // Symbol List: 

    // (2, 28)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, readtest
    int32[1:4] a
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 3)
  read a()
  // (4, 3)
  printf(a())
}

