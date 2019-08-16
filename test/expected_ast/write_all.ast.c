Program: write_all.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() v
}

// MainProgram
int32 v() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable v {
    // Symbol List: 

    // (6, 11)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    int32 a
    // (9, 9)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    real b
    // (4, 10)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    logical boolf
    // (3, 10)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    logical boolt
    // (10, 20)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    double c
    // (7, 19)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    int64 d
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 10)
  boolt = .TRUE.
  // (4, 10)
  boolf = .false.
  // (6, 11)
  a = 10
  // (7, 19)
  d = 9999999999
  // (9, 9)
  b = 3.45600000
  // (10, 20)
  c = 4.556
   // (12, 1)
  write   boolt
   // (13, 1)
  write   boolf
   // (15, 1)
  write   a
   // (16, 1)
  write   d
   // (18, 1)
  write   b
   // (19, 1)
  write   c
}

