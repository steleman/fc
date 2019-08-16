Program: semicolon1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_alloca
}

// MainProgram
int32 decl_alloca() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_alloca {
    // Symbol List: 

    // (2, 18)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, decl_alloca
    int32 i
  }

  // Specification Constructs: 

  EntityDeclList {
    {
      // (2, 18)
      NAME: i
      SYMBOL ID: 2
      INIT: 10
    }
  }

  // Execution Constructs: 

  // (3, 3)
  i = 1
  // (3, 10)
  i = 2
  // (4, 3)
  i = 3
  // (5, 3)
  i = 4
}

