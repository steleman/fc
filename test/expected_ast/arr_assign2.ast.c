Program: arr_assign2.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() arr2
}

// MainProgram
int32 arr2() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable arr2 {
    // Symbol List: 

    // (2, 13)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr2
    int32[1:10, 1:10] a
    // (2, 23)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr2
    int32[-10:10] b
    // (3, 13)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr2
    int32 c
    // (4, 13)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr2
    int32 d
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 13)
  c = -3
  // (4, 13)
  d = 5
  // (5, 3)
  a(d, d) = 10
  // (6, 3)
  b(c) = 10
  // (7, 3)
  t.1 = a(d, d) + b(-3)
  printf(t.1)
}

