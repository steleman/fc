Program: unary_minus2.f90
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

    // (2, 13)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, t
    int32 a
    // (2, 21)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, t
    int32 b
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 13)
  a = 10
  // (2, 21)
  b = 20
  // (3, 3)
  t.2 = 0 - a
  t.4 = 0 - b
  t.3 = t.4 * a
  t.1 = t.2 + t.3
  printf(t.1)
}

