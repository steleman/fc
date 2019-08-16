Program: fileio4.f90
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

    // (3, 11)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, readtest
    int32 i
    // (2, 15)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, readtest
    character[1:7] msg
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (5, 3)
  1 = open({./fileio/input/4.dat}, OLD)
  // (6, 3)
  read(unit = 1) msg()
  // (7, 3)
  printf(msg())
  // (8, 3)
  1 = close(/*unit=*/1)
}

