Program: where_stmt2.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() t
}

// MainProgram
int32 t() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable t {
    // Symbol List: 

    // (3, 29)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, t
    int32[1:10] a
    // (2, 14)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, t
    int32 i
    // (8, 11)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, t
    (int32)(...) lbound
    // (8, 11)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, t
    int32 t.tmp.0
    // (8, 11)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, t
    (int32)(...) ubound
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 14)
  i = 10
  // (5, 3)
  t.1 = (/*IndVar=*/i, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.1) {
    // (6, 5)
    a(i) = i
  }
  // (8, 3)
  t.2 = (/*IndVar=*/t.tmp.0, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.2) {
    // (8, 3)
    t.3 = a(t.tmp.0) > 3
    if (t.3) {
      // (9, 7)
      a(t.tmp.0) = 3
    }
  }
}

