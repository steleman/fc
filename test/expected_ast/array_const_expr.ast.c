Program: array_const_expr.f90
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

    // (2, 12)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, t
    int32[1:2] t.tmp.0
    // (2, 25)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, t
    int32[1:2] t.tmp.1
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 12)
  t.tmp.0(1) = 1
  // (2, 12)
  t.tmp.0(2) = 2
  // (2, 25)
  t.tmp.1(1) = 2
  // (2, 25)
  t.tmp.1(2) = 3
  // (2, 3)
  t.1 = t.tmp.0() <= t.tmp.1()
  printf(t.1)
}

