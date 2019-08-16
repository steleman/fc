Program: for_all1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_alloca
}

// MainProgram
int32 decl_alloca() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_alloca {
    // Symbol List: 

    // (2, 15)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_alloca
    int32[1:10, 1:10] a
    // (3, 3)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_alloca
    int32 decl_alloca.tmp.0
    // (3, 3)
    // ID: 8, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_alloca
    int32 decl_alloca.tmp.1
    // (2, 13)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_alloca
    int32 i
    // (2, 26)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_alloca
    int32 j
    // (3, 3)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, decl_alloca
    (int32)(...) lbound
    // (3, 3)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, decl_alloca
    (int32)(...) ubound
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 3)
  t.1 = (/*IndVar=*/decl_alloca.tmp.1, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.1) {
    // (3, 3)
    t.2 = (/*IndVar=*/decl_alloca.tmp.0, /*Init=*/1, /*End=*/10, /*Incr=*/1)
    do (t.2) {
      // (3, 3)
      a(decl_alloca.tmp.0, decl_alloca.tmp.1) = 0
    }
  }
  // (4, 3)
  t.3 = (/*IndVar=*/i, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.3) {
    // (4, 3)
    t.4 = (/*IndVar=*/j, /*Init=*/1, /*End=*/10, /*Incr=*/1)
    do (t.4) {
      // (4, 3)
      t.5 = a(i, j) == 0
      if (t.5) {
        // (4, 40)
        a(i, j) = 1
      }
    }
  }
  // (5, 3)
  printf(a())
}

