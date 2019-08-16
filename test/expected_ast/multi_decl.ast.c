Program: multi_decl.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() multi_decl
}

// MainProgram
int32 multi_decl() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable multi_decl {
    // Symbol List: 

    // (2, 12)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, multi_decl
    logical a
    // (2, 15)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, multi_decl
    logical b
    // (2, 18)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, multi_decl
    logical c
    // (2, 21)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, multi_decl
    logical d
    // (2, 38)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, multi_decl
    logical e
    // (2, 34)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, multi_decl
    logical f
    // (3, 12)
    // ID: 8, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, multi_decl
    int32 g
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

