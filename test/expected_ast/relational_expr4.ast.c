Program: relational_expr4.f90
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
    // (5, 18)
    // ID: 10, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32[1:4, 1:3] array
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
  // (6, 7)
  array(3, 3) = a
  // (7, 7)
  t.2 = array(3, 3) + a
  t.3 = a + b
  t.1 = t.2 == t.3
  flag2 = t.1
}

