Program: fileio3.f90
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

    // (2, 23)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, readtest
    character[0:80] msg
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (4, 3)
  1 = open({./fileio/input/3.dat}, OLD)
  // (5, 3)
  read(unit = 1) msg()
  // (6, 3)
  printf(msg())
  // (7, 3)
  1 = close(/*unit=*/1)
}

