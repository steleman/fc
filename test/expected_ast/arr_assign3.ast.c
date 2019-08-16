Program: arr_assign3.f90
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
    int32[1:10, 1:10, 1:40] a
    // (2, 27)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr2
    int32[1:25, 1:23, 1:21, -4:23] b
    // (3, 13)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr2
    int32 i
    // (3, 15)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr2
    int32 j
    // (3, 17)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr2
    int32 k
    // (3, 19)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr2
    int32 l
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (4, 3)
  i = 1
  // (5, 3)
  j = 2
  // (6, 3)
  k = 3
  // (7, 3)
  l = -2
  // (8, 3)
  b(i, j, k, l) = 30
  // (9, 3)
  b(5, j, k, l) = 10
  // (10, 3)
  t.1 = b(5, j, k, l) * j
  t.2 = i + j
  t.3 = k + l
  t.4 = b(i, j, k, l) + i
  a(t.2, t.3, t.4) = t.1
  // (11, 3)
  printf(a(3, 1, 31))
}

