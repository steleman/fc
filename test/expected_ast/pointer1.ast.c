Program: pointer1.f90
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
    // ID: 3, NonConstant, NonAllocatable, Target, NonPointer, StaticLocal, Intent_None, foo
    int32[1:10] arr
    // (5, 23)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, Pointer, StaticLocal, Intent_None, foo
    int32[U]* arrptr
    // (2, 22)
    // ID: 2, NonConstant, NonAllocatable, Target, NonPointer, StaticLocal, Intent_None, foo
    int32 i
    // (4, 23)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, Pointer, StaticLocal, Intent_None, foo
    int32* iptr
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (7, 3)
  iptr => i
  // (8, 3)
  arrptr() => arr()
}

