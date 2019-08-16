Program: decl2.f90
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

    // (2, 14)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, i
    logical a
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

