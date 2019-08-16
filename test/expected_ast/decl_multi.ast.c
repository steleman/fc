Program: decl_multi.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_all
}

// MainProgram
int32 decl_all() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_all {
    // Symbol List: 

    // (2, 9)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_all
    int32 a
    // (6, 9)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_all
    real[1:10, -1:100] arr
    // (3, 12)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_all
    int32 b
    // (3, 15)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_all
    int32 c
    // (4, 9)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_all
    real d
    // (5, 12)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_all
    logical e
    // (6, 25)
    // ID: 8, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_all
    real f
    // (7, 43)
    // ID: 9, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_all
    int32[1:10, -1:100, -1:10] g
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

