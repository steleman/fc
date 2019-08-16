Program: stop_stmt1.f90
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

    // (2, 23)
    // ID: 2, Constant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    int32 a
    // (3, 23)
    // ID: 3, Constant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    int32 b
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 23)
  a = 4
  // (3, 23)
  b = 10
  // (5, 3)
  stop 14
}

