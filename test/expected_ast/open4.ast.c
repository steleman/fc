Program: open4.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() outputdata
}

// MainProgram
int32 outputdata() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable outputdata {
    // Symbol List: 

    // (5, 15)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, outputdata
    int32 i
    // (3, 27)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, outputdata
    real[1:10] x
    // (4, 27)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, outputdata
    real[1:10] y
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (7, 4)
  1 = open({data1.dat}, OLD)
  // (8, 4)
  t.1 = (/*IndVar=*/i, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.1) {
    // (9, 7)
    read(unit = 1) x(i), y(i)
    // (10, 7)
    printf(x(i), y(i))
  }
  // (12, 4)
  1 = close(/*unit=*/1)
}

