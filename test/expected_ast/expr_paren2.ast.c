Program: expr_paren2.f90
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

    // (4, 10)
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
  b = 10.0
  // (3, 11)
  c = 4.0
  // (5, 3)
  t.3 = b * c
  t.4 = cast 5 to   real
  t.2 = t.3 + t.4
  t.1 = 3.0 * t.2
  a = t.1
  // (6, 3)
  b = 20.0
}

