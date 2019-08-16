Program: cycle_and_exit.f90
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

    // (2, 12)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, vin
    int32 i
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 3)
  t.1 = (/*IndVar=*/i, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.1) {
    // (5, 3)
    t.2 = i == 5
    if (t.2) {
      // (5, 15)
      cycle 
    }
    // (6, 3)
    t.3 = i == 9
    if (t.3) {
      // (6, 15)
      exit 
    }
    // (7, 3)
    printf(i)
  }
}

