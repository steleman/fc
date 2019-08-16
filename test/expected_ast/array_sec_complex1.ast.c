Program: array_sec_complex1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() test
}

// MainProgram
int32 test() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable test {
    // Symbol List: 

    // (2, 16)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32[1:10, 1:20] a
    // (3, 16)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32[1:10, 1:20] b
    // (2, 25)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 c
    // (4, 7)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, test
    (int32)(...) lbound
    // (4, 7)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.0
    // (4, 7)
    // ID: 8, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.1
    // (5, 7)
    // ID: 9, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.2
    // (5, 7)
    // ID: 10, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.3
    // (8, 7)
    // ID: 11, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.4
    // (4, 7)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, test
    (int32)(...) ubound
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (4, 7)
  t.1 = (/*IndVar=*/test.tmp.1, /*Init=*/1, /*End=*/20, /*Incr=*/1)
  do (t.1) {
    // (4, 7)
    t.2 = (/*IndVar=*/test.tmp.0, /*Init=*/1, /*End=*/10, /*Incr=*/1)
    do (t.2) {
      // (4, 7)
      a(test.tmp.0, test.tmp.1) = 1
    }
  }
  // (5, 7)
  t.3 = (/*IndVar=*/test.tmp.3, /*Init=*/1, /*End=*/20, /*Incr=*/1)
  do (t.3) {
    // (5, 7)
    t.4 = (/*IndVar=*/test.tmp.2, /*Init=*/1, /*End=*/10, /*Incr=*/1)
    do (t.4) {
      // (5, 7)
      b(test.tmp.2, test.tmp.3) = 2
    }
  }
  // (6, 7)
  c = 2
  // (8, 7)
  t.5 = b(2, 1) + 1
  t.6 = (/*IndVar=*/test.tmp.4, /*Init=*/b(1, 1), /*End=*/t.5, /*Incr=*/c)
  do (t.6) {
    // (8, 7)
    t.9 = c + 2
    t.8 = t.9 - 32
    t.7 = t.8 + 32
    a(test.tmp.4, t.7) = 10
  }
  // (9, 7)
  printf(a())
}

