Program: expr_paren5.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() i
}

// MainProgram
int32 i() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable i {
    // Symbol List: 

    // (4, 11)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    real a
    // (2, 10)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    real b
    // (3, 11)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    real c
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 10)
  b = 40.000000
  // (3, 11)
  c = 16.000000
  // (5, 3)
  t.1 = b * c
  a = t.1
}

