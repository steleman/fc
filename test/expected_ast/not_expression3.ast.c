Program: not_expression3.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() t
}

// MainProgram
int32 t() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable t {
    // Symbol List: 

    // (2, 16)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, t
    logical a
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 16)
  a = .false.
  // (3, 5)
  printf(a)
}

