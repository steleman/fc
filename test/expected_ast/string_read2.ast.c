Program: string_read2.f90
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

    // (3, 14)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32[1:3, 1:3] a
    // (2, 39)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    character[1:10, 0:10] array
    // (4, 14)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 i
    // (4, 17)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 j
    // (4, 20)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, test
    int32 k
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (6, 3)
  7 = open({./fileio/input/string_read2.dat}, undefined)
  // (7, 3)
  t.1 = (/*IndVar=*/i, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.1) {
    // (8, 5)
    read(unit = 7)(format = {(a9)}) array(i)
  }
  // (11, 3)
  t.2 = (/*IndVar=*/i, /*Init=*/1, /*End=*/10, /*Incr=*/1)
  do (t.2) {
    // (12, 5)
    read(unit = array(i))(format = {(9i1)}) a()
    // (13, 5)
    printf(a())
  }
  // (15, 3)
  7 = close(/*unit=*/7)
}

