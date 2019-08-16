Program: select1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() selectcaseprog
}

// MainProgram
int32 selectcaseprog() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable selectCaseProg {
    // Symbol List: 

    // (5, 26)
    // ID: 2, Constant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, selectCaseProg
    int32 a
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (5, 26)
  a = 1008
  // (8, 4)
  if (.false.) {
    // (9, 6)
    printf({Number is 1})
  }
  // (10, 4)
  else if (.false.) {
    // (11, 6)
    printf({Number is 2})
  }
  // (12, 4)
  else if (.false.) {
    // (13, 6)
    printf({Number is 3})
  }
  // (14, 4)
  else if (.false.) {
    // (15, 6)
    printf({Number is 4})
  }
  // (16, 4)
  else if (.false.) {
    // (17, 6)
    printf({Number is 5})
  }
  // (18, 4)
  else {
    // (19, 6)
    printf({Some other number}, 1008)
  }
}

