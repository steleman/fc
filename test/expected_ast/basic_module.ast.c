Program: basic_module.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (3, 8)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  Module mod1
  // (7, 9)
  // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() pg
}

Module mod1 { 

  // ModuleScope, Parent: GlobalScope
  SymbolTable mod1 {
    // Symbol List: 

    // (4, 14)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod1
    int32 a
  }

  // Specification Constructs: 

  EntityDeclList {
    {
      // (4, 14)
      NAME: a
      SYMBOL ID: 2
      INIT: 10
    }
  }
}

// MainProgram
int32 pg() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable pg {
    // Symbol List: 

    // (13, 12)
    // ID: 4, ParentSymbol: 5, ParentSymbolTable: mod1
    int32 a
  }
  UsedSymbolTables {
    // ModuleScope, Parent: None
    SymbolTable mod1 {
      // Symbol List: 

      // (1, 1)
      // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod1
      int32 a
    }
  }

  // Specification Constructs: 

  UseStmtList {
    // (10, 7)
    Module mod1
  }
  EntityDeclList {
  }

  // Execution Constructs: 

  // (13, 3)
  printf(a)
}

