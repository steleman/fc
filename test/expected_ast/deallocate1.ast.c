Program: deallocate1.f90
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
    int32[U] a
    // (3, 27)
    // ID: 3, NonConstant, Allocatable, NonTarget, NonPointer, StaticLocal, Intent_None, alloctest
    int32[U] b
    // (4, 14)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, alloctest
    int32 i
    // (5, 14)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, alloctest
    int32 st
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (7, 3)
  allocate a[1:6], b[1:8]
  // (8, 3)
  printf({Allocation status }, st)
  // (10, 3)
  t.1 = (/*IndVar=*/i, /*Init=*/1, /*End=*/6, /*Incr=*/1)
  do (t.1) {
    // (11, 5)
    a(i) = i
  }
  // (14, 3)
  t.2 = (/*IndVar=*/i, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.2) {
    // (15, 5)
    b(i) = i
  }
  // (18, 3)
  printf(a())
  // (19, 3)
  printf(b())
   // (20, 3)
  deallocate a, b, statst
  // (21, 3)
  printf({De Allocation status }, st)
}

