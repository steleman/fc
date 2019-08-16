Program: do_label_and_return.f90
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

  // (3, 7)
  t.1 = (/*IndVar=*/i, /*Init=*/1, /*End=*/4, /*Incr=*/1)
  v1: do (t.1) {
    // (4, 5)
    printf(i)
    // (5, 5)
    return 
  }
}

