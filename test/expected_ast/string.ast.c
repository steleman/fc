Program: string.f90
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
    character[0:34] msg
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 7)
  read msg()
  // (4, 7)
  printf(msg())
}

