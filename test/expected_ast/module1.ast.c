Program: module1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (5, 12)
  // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (void)() pgm
  // (1, 8)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  Module vin
}

Module vin { 

  // ModuleScope, Parent: GlobalScope
  SymbolTable vin {
    // Symbol List: 

    // (2, 12)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, vin
    int32 a
  }

  // Specification Constructs: 

  EntityDeclList {
    {
      // (2, 12)
      NAME: a
      SYMBOL ID: 2
    }
  }
}

// Subroutine
void pgm() {
  // SubroutineScope, Parent: GlobalScope
  SymbolTable pgm {
    // Symbol List: 

    // (6, 14)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, pgm
    int32[1:10] a
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (7, 3)
  a(1) = 10
}

