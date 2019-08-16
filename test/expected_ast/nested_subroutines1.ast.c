Program: nested_subroutines1.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() p
}

// MainProgram
int32 p() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable p {
    // Symbol List: 

    // (3, 10)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, p
    int32 a
    // (3, 15)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, p
    int32 d
    // (5, 6)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, p
    (void)() sub1
    // (6, 6)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, p
    (void)() sub2
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 10)
  a = 10
  // (3, 15)
  d = 1
  // (5, 6)
  call sub1()
  // (6, 6)
  call sub2()

  // Internal SubProgram Lists: 

  // Subroutine
  void sub1() {
    // SubroutineScope, Parent: MainProgramScope
    SymbolTable sub1 {
      // Symbol List: 

      // (14, 3)
      // ID: 8, ParentSymbol: 2, ParentSymbolTable: p
      int32 a
      // (11, 14)
      // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, sub1
      int32 b
      // (13, 11)
      // ID: 7, ParentSymbol: 3, ParentSymbolTable: p
      int32 d
    }

    // Specification Constructs: 

    EntityDeclList {
      {
        // (11, 14)
        NAME: b
        SYMBOL ID: 6
        INIT: 20
      }
    }

    // Execution Constructs: 

    // (12, 3)
    printf(b)
    // (13, 3)
    printf(d)
    // (14, 3)
    a = 20
  }

  // Subroutine
  void sub2() {
    // SubroutineScope, Parent: MainProgramScope
    SymbolTable sub2 {
      // Symbol List: 

      // (20, 11)
      // ID: 10, ParentSymbol: 2, ParentSymbolTable: p
      int32 a
      // (18, 14)
      // ID: 9, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, sub2
      int32 b
    }

    // Specification Constructs: 

    EntityDeclList {
      {
        // (18, 14)
        NAME: b
        SYMBOL ID: 9
        INIT: 30
      }
    }

    // Execution Constructs: 

    // (19, 3)
    printf(b)
    // (20, 3)
    printf(a)
  }

}

