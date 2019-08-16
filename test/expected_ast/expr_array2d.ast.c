Program: expr_array2d.f90
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
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 3)
  array(1, 1) = 2
  // (4, 3)
  array(1, 2) = 2
  // (5, 3)
  array(2, 1) = 1
  // (6, 3)
  t.2 = array(1, 2) * array(2, 1)
  t.1 = array(1, 1) + t.2
  array(2, 2) = t.1
}

