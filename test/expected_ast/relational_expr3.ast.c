Program: relational_expr3.f90
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
    // ID: 8, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 a
    // (4, 18)
    // ID: 9, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 b
    // (2, 18)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    logical flag1
    // (2, 25)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    logical flag2
    // (2, 32)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    logical flag3
    // (2, 39)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    logical flag4
    // (2, 46)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    logical flag5
    // (2, 53)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    logical flag6
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
  t.1 = a == b
  flag1 = t.1
  // (6, 7)
  t.2 = a != b
  flag2 = t.2
  // (7, 7)
  t.3 = a < b
  flag3 = t.3
  // (8, 7)
  t.4 = a <= b
  flag4 = t.4
  // (9, 7)
  t.5 = a > b
  flag5 = t.5
  // (10, 7)
  t.6 = a >= b
  flag6 = t.6
}

