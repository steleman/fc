Program: logical_expr1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() test
}

// MainProgram
int32 test() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable test {
    // Symbol List: 

    // (3, 18)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 a
    // (4, 18)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 b
    // (2, 18)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    logical flag1
    // (2, 25)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    logical flag2
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 18)
  a = 5
  // (4, 18)
  b = 5
  // (5, 7)
  t.2 = a == b
  t.3 = b == a
  t.1 = t.2 .AND. t.3
  flag1 = t.1
  // (6, 7)
  t.5 = a == b
  t.6 = b == a
  t.4 = t.5 .OR. t.6
  flag2 = t.4
}

