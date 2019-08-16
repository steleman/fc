Program: array_section3.f90
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

    // (2, 33)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32[1:10, 1:10] array
    // (3, 25)
    // ID: 3, Constant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 l
    // (5, 3)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 test.tmp.0
    // (4, 25)
    // ID: 4, Constant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 u
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (3, 25)
  l = 1
  // (4, 25)
  u = 10
  // (5, 3)
  t.1 = (/*IndVar=*/test.tmp.0, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.1) {
    // (5, 3)
    array(5, test.tmp.0) = 10
  }
}

