Program: module2.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (6, 8)
  // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  Module mod1
  // (2, 8)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  Module mod2
  // (11, 9)
  // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() pg
}

Module mod2 { 

  // ModuleScope, Parent: GlobalScope
  SymbolTable mod2 {
    // Symbol List: 

    // (3, 13)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod2
    int32[1:10] c
  }

  // Specification Constructs: 

  EntityDeclList {
    {
      // (3, 13)
      DIMS: [1:10]
      NAME: c
      SYMBOL ID: 2
    }
  }
}

Module mod1 { 

  // ModuleScope, Parent: GlobalScope
  SymbolTable mod1 {
    // Symbol List: 

    // (8, 14)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod1
    int32 a
    // (8, 22)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod1
    int32[1:20] b
    // (1, 1)
    // ID: 11, ParentSymbol: 10, ParentSymbolTable: mod2
    int32[1:10] c
  }
  UsedSymbolTables {
    // ModuleScope, Parent: None
    SymbolTable mod2 {
      // Symbol List: 

      // (1, 1)
      // ID: 10, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod2
      int32[1:10] c
    }
  }

  // Specification Constructs: 

  UseStmtList {
    // (7, 7)
    Module mod2
  }
  EntityDeclList {
    {
      // (8, 14)
      NAME: a
      SYMBOL ID: 4
      INIT: 10
    }
    {
      // (8, 22)
      DIMS: [1:20]
      NAME: b
      SYMBOL ID: 5
    }
  }
}

// MainProgram
int32 pg() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable pg {
    // Symbol List: 

    // (20, 3)
    // ID: 9, ParentSymbol: 12, ParentSymbolTable: mod1
    int32 a
    // (18, 3)
    // ID: 7, ParentSymbol: 13, ParentSymbolTable: mod1
    int32[1:20] b
    // (19, 3)
    // ID: 8, ParentSymbol: 14, ParentSymbolTable: mod1
    int32[1:10] c
  }
  UsedSymbolTables {
    // ModuleScope, Parent: None
    SymbolTable mod1 {
      // Symbol List: 

      // (1, 1)
      // ID: 12, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod1
      int32 a
      // (1, 1)
      // ID: 13, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod1
      int32[1:20] b
      // (1, 1)
      // ID: 14, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod2
      int32[1:10] c
    }
  }

  // Specification Constructs: 

  UseStmtList {
    // (14, 7)
    Module mod1
  }
  EntityDeclList {
  }

  // Execution Constructs: 

  // (18, 3)
  b(1) = 13
  // (19, 3)
  c(9) = 3
  // (20, 3)
  t.2 = a + 10
  t.1 = t.2 + b(1)
  a = t.1
  // (22, 3)
  printf(a, c(9))
}

