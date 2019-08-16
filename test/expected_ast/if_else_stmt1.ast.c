Program: if_else_stmt1.f90
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
    int32 a
    // (2, 20)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, t
    int32 b
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 12)
  a = 10
  // (2, 20)
  b = 20
  // (4, 3)
  t.1 = a == 10
  if (t.1) {
    // (5, 4)
    a = 30
    // (6, 4)
    b = 20
  }
  // (8, 4)
  else {
    // (8, 4)
    t.2 = b + 20
    a = t.2
    // (9, 4)
    b = 40
  }
  // (12, 3)
  printf(a, b)
}

