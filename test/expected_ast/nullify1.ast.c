Program: nullify1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() foo
}

// MainProgram
int32 foo() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable foo {
    // Symbol List: 

    // (3, 22)
    // ID: 4, NonConstant, NonAllocatable, Target, NonPointer, StaticLocal, Intent_None, foo
    int32 i1
    // (3, 26)
    // ID: 5, NonConstant, NonAllocatable, Target, NonPointer, StaticLocal, Intent_None, foo
    int32 i2
    // (2, 23)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, Pointer, StaticLocal, Intent_None, foo
    int32* iptr1
    // (2, 30)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, Pointer, StaticLocal, Intent_None, foo
    int32* iptr2
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (5, 3)
  iptr1 => i1
  // (6, 3)
  iptr2 => i2
  // (8, 3)
  nullify(iptr1, iptr2)
}

