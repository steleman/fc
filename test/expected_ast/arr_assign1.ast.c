Program: arr_assign1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() arr1
}

// MainProgram
int32 arr1() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable arr1 {
    // Symbol List: 

    // (2, 13)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, arr1
    int32[1:2, 1:2] a
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 3)
  a(1, 1) = 10
  // (4, 3)
  printf(a(1, 1))
}

