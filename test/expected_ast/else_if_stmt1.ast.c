Program: else_if_stmt1.f90
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
  t.1 = a != 10
  if (t.1) {
    // (5, 4)
    a = 30
  }
  // (6, 8)
  t.2 = a == 10
  else if (t.2) {
    // (7, 4)
    a = 20
  }
  // (9, 5)
  else {
    // (9, 5)
    a = 40
  }
  // (11, 3)
  printf(a)
}

