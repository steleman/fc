Program: array_implied_do1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() arr_construct
}

// MainProgram
int32 arr_construct() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable arr_construct {
    // Symbol List: 

    // (3, 10)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr_construct
    real[1:4] a
    // (2, 13)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr_construct
    int32 k
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 10)
  t.2 = (/*IndVar=*/k, /*Init=*/1, /*End=*/3, /*Incr=*/1)
  t.1 = cast (/ 2, t.2 /) to   real
  a = t.1
  // (4, 3)
  printf(a())
}

