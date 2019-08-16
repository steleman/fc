Program: array_sec_operations1.f90
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
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32[1:10, 1:20] b
    // (4, 7)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, test
    (int32)(...) lbound
    // (4, 7)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.0
    // (4, 7)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.1
    // (5, 7)
    // ID: 8, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.2
    // (5, 7)
    // ID: 9, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.3
    // (6, 7)
    // ID: 10, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.4
    // (6, 20)
    // ID: 11, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.5
    // (6, 33)
    // ID: 12, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.6
    // (4, 7)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, test
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
  // (6, 33)
  test.tmp.6 = 3
  // (6, 20)
  test.tmp.5 = 1
  // (6, 7)
  t.5 = (/*IndVar=*/test.tmp.4, /*Init=*/1, /*End=*/5, /*Incr=*/2)
  do (t.5) {
    // (6, 7)
    t.6 = b(test.tmp.5, 2) + b(test.tmp.6, 11)
    a(test.tmp.4, 1) = t.6
    // (6, 33)
    t.7 = test.tmp.6 + 2
    test.tmp.6 = t.7
    // (6, 20)
    t.8 = test.tmp.5 + 2
    test.tmp.5 = t.8
  }
  // (7, 7)
  printf(a(:, 1))
}

