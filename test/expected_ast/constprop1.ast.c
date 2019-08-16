Program: constprop1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (6, 9)
  // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() foo
  // (1, 8)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  Module mod
}

Module mod { 

  // ModuleScope, Parent: GlobalScope
  SymbolTable mod {
    // Symbol List: 

    // (3, 25)
    // ID: 5, Constant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod
    logical log
    // (2, 25)
    // ID: 2, Constant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod
    int32 rank
    // (2, 33)
    // ID: 3, Constant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod
    int32 rank2
    // (2, 50)
    // ID: 4, Constant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod
    int32 total
  }

  // Specification Constructs: 

  EntityDeclList {
    {
      // (2, 25)
      NAME: rank
      SYMBOL ID: 2
      INIT: 3
    }
    {
      // (2, 33)
      NAME: rank2
      SYMBOL ID: 3
      INIT: 9
    }
    {
      // (2, 50)
      NAME: total
      SYMBOL ID: 4
      INIT: 45
    }
    {
      // (3, 25)
      NAME: log
      SYMBOL ID: 5
      INIT: .false.
    }
  }
}

// MainProgram
int32 foo() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable foo {
    // Symbol List: 

    // (9, 14)
    // ID: 9, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, foo
    int32[1:9] arr1
    // (8, 14)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, foo
    int32 k
    // (20, 23)
    // ID: 12, ParentSymbol: 16, ParentSymbolTable: mod
    logical log
    // (11, 22)
    // ID: 11, ParentSymbol: 13, ParentSymbolTable: mod
    int32 rank
    // (9, 19)
    // ID: 8, ParentSymbol: 14, ParentSymbolTable: mod
    int32 rank2
    // (11, 14)
    // ID: 10, ParentSymbol: 15, ParentSymbolTable: mod
    int32 total
  }
  UsedSymbolTables {
    // ModuleScope, Parent: None
    SymbolTable mod {
      // Symbol List: 

      // (1, 1)
      // ID: 16, Constant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod
      logical log
      // (1, 1)
      // ID: 13, Constant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod
      int32 rank
      // (1, 1)
      // ID: 14, Constant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod
      int32 rank2
      // (1, 1)
      // ID: 15, Constant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, mod
      int32 total
    }
  }

  // Specification Constructs: 

  UseStmtList {
    // (7, 7)
    Module mod
  }
  EntityDeclList {
  }

  // Execution Constructs: 

  // (9, 14)
  t.1 = (/*IndVar=*/k, /*Init=*/1, /*End=*/9, /*Incr=*/1)
  arr1 = (/ t.1 /)
  // (11, 3)
  t.2 = (/*IndVar=*/k, /*Init=*/1, /*End=*/7, /*Incr=*/1)
  do (t.2) {
    // (12, 5)
    arr1(k) = 12
  }
  // (15, 3)
  if (.false.) {
    // (16, 5)
    arr1(3) = 2
  }
  // (19, 3)
  if (.false.) {
    // (20, 5)
    if (.true.) {
      // (21, 7)
      t.3 = 3 + k
      arr1(t.3) = 2
    }
  }
}

