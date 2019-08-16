Program: expr_array1d.f90
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

    // (3, 13)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    int32 a
    // (5, 28)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    int32[1:5] array
    // (2, 13)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    int32 b
    // (4, 13)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    int32 c
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (6, 3)
  a = 3
  // (7, 3)
  b = 20
  // (8, 3)
  array(1) = 1
  // (9, 3)
  array(2) = 2
  // (10, 3)
  array(3) = 3
  // (11, 3)
  t.2 = array(2) * array(3)
  t.1 = array(1) + t.2
  array(4) = t.1
  // (12, 3)
  t.3 = a + b
  array(5) = t.3
  // (13, 3)
  t.6 = array(1) + array(2)
  t.5 = t.6 + array(3)
  t.4 = t.5 + array(4)
  c = t.4
}

