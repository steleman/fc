Program: print_assign.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() test_print
}

// MainProgram
int32 test_print() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable test_print {
    // Symbol List: 

    // (2, 21)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test_print
    int32 a
    // (2, 35)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test_print
    int32[1:3, 1:2] arr
    // (2, 28)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test_print
    int32 b
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 21)
  a = 1
  // (2, 28)
  b = 2
  // (3, 3)
  arr(1, 1) = 1
  // (4, 3)
  arr(2, 1) = 1
  // (5, 3)
  arr(3, 1) = 1
  // (6, 3)
  arr(1, 2) = 2
  // (7, 3)
  arr(2, 2) = 2
  // (8, 3)
  arr(3, 2) = 2
  // (9, 3)
  printf(arr())
}

