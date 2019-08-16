Program: decl_logical_init.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() decl_log_init
}

// MainProgram
int32 decl_log_init() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable decl_log_init {
    // Symbol List: 

    // (2, 11)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_log_init
    logical a
    // (2, 14)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_log_init
    logical b
    // (2, 17)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_log_init
    logical c
    // (2, 20)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_log_init
    logical d
    // (2, 37)
    // ID: 7, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_log_init
    logical e
    // (2, 33)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, decl_log_init
    logical f
  }

  // Specification Constructs: 

  EntityDeclList {
  }
}

