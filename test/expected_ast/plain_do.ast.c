Program: plain_do.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() vin
}

// MainProgram
int32 vin() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable vin {
    // Symbol List: 

    // (2, 13)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, vin
    int32 i
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 13)
  i = 0
  // (3, 3)
  do {
    // (5, 3)
    t.1 = i + 1
    i = t.1
    // (6, 3)
    t.2 = i == 5
    if (t.2) {
      // (6, 15)
      cycle 
    }
    // (7, 3)
    t.3 = i == 9
    if (t.3) {
      // (7, 15)
      exit 
    }
    // (8, 3)
    printf(i)
  }
}

