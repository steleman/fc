Program: array_const_expr2.f90
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

    // (3, 10)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, vin
    real[1:4] a
    // (2, 13)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, vin
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
  t.4 = (/*IndVar=*/k, /*Init=*/1, /*End=*/3, /*Incr=*/2)
  t.5 = (/*IndVar=*/k, /*Init=*/3, /*End=*/5, /*Incr=*/2)
  t.3 = (/ 2, t.4 /) + (/ t.5, 6 /)
  printf(t.3)
  // (5, 3)
  t.6 = (/*IndVar=*/k, /*Init=*/1, /*End=*/4, /*Incr=*/1)
  do (t.6) {
    // (6, 5)
    printf(a(k))
  }
}

