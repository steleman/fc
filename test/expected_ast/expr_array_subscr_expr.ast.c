Program: expr_array_subscr_expr.f90
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

    // (2, 31)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    int32[1:2, 1:2] array
    // (3, 12)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    int32 b
    // (4, 12)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    int32 c
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 12)
  b = 1
  // (4, 12)
  c = 2
  // (5, 3)
  t.1 = b * b
  array(b, b) = t.1
  // (6, 3)
  t.2 = b * c
  array(b, c) = t.2
  // (7, 3)
  t.3 = b * c
  t.4 = c * 1
  t.5 = b ** 1
  array(t.4, t.5) = t.3
  // (8, 3)
  t.6 = b * array(b, c)
  t.7 = c ** 1
  array(t.7, c) = t.6
}

