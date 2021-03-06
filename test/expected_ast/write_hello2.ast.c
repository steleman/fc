Program: write_hello2.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() test
}

// MainProgram
int32 test() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable test {
    // Symbol List: 

    // (2, 28)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    character[0:80] string
    // (3, 28)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    character[0:20] string2
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (4, 7)
  string() = {hello world}
  // (5, 7)
  string2() = string()
   // (6, 7)
  write   string2(), string(), string2()
}

