Program: expr_paren7.f90
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
  t.3 = cast 8 to   real
  t.2 = 10.0 * t.3
  t.4 = cast 16 to   real
  t.1 = t.2 * t.4
  b = t.1
  // (3, 11)
  c = 16.000000
  // (5, 3)
  t.6 = b * c
  t.7 = b * c
  t.5 = t.6 / t.7
  a = t.5
}

