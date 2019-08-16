Program: sub_arg1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (2, 12)
  // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (void)(int32[1:10]) pgm
}

// Subroutine
void pgm(int32[1:10] a) {
  // SubroutineScope, Parent: GlobalScope
  SymbolTable pgm {
    // Symbol List: 

    // (2, 12)
    // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Argument, In, pgm
    int32[1:10] a
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (4, 3)
  a(1) = 10
}

