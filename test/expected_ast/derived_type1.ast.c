Program: derived_type1.f90
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

    // (21, 24)
    // ID: 15, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, foo
    dt.foo.PowerCoords pc
    // (20, 21)
    // ID: 14, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, foo
    dt.foo.PowerTyp pt
  }

  // Specification Constructs: 

  DerivedTypeDef powertyp {
    // MainProgramScope, Parent: MainProgramScope
    SymbolTable PowerTyp {
      // Symbol List: 

      // (9, 35)
      // ID: 8, NonConstant, NonAllocatable, NonTarget, Pointer, StaticGlobal, Intent_None, PowerTyp
      dt.foo.PowerCoords* coords
      // (5, 26)
      // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, PowerTyp
      character[0:80] filenamebase
      // (12, 23)
      // ID: 11, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, PowerTyp
      logical mine
      // (11, 35)
      // ID: 10, NonConstant, NonAllocatable, NonTarget, Pointer, StaticGlobal, Intent_None, PowerTyp
      dt.foo.PowerTyp* next
      // (10, 23)
      // ID: 9, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, PowerTyp
      int32 nocoords
      // (6, 23)
      // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, PowerTyp
      int32 nofreq
      // (4, 26)
      // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, PowerTyp
      int32 skip
      // (3, 26)
      // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, PowerTyp
      int32 stride
      // (7, 23)
      // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, PowerTyp
      int32 window
      // (8, 23)
      // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, PowerTyp
      int32 y_index
    }
  }
  DerivedTypeDef powercoords {
    // MainProgramScope, Parent: MainProgramScope
    SymbolTable PowerCoords {
      // Symbol List: 

      // (16, 35)
      // ID: 12, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticGlobal, Intent_None, PowerCoords
      int32[1:7] coords
      // (17, 35)
      // ID: 13, NonConstant, NonAllocatable, NonTarget, Pointer, StaticGlobal, Intent_None, PowerCoords
      dt.foo.PowerCoords* next
    }
  }
  EntityDeclList {
  }
}

