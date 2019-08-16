Program: allocate2.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() alloctest
}

// MainProgram
int32 alloctest() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable alloctest {
    // Symbol List: 

    // (2, 27)
    // ID: 2, NonConstant, Allocatable, NonTarget, NonPointer, StaticLocal, Intent_None, alloctest
    int32[U, U] a
    // (3, 14)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, alloctest
    int32 i
    // (3, 17)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, alloctest
    int32 j
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (5, 3)
  allocate a[1:2, 1:2]
  // (7, 3)
  t.1 = (/*IndVar=*/i, /*Init=*/1, /*End=*/2, /*Incr=*/1)
  do (t.1) {
    // (8, 5)
    t.2 = (/*IndVar=*/j, /*Init=*/1, /*End=*/2, /*Incr=*/1)
    do (t.2) {
      // (9, 7)
      t.3 = i * j
      a(i, j) = t.3
    }
  }
  // (12, 3)
  printf(a())
}

