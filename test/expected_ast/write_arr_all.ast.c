Program: write_arr_all.f90
// GlobalScope, Parent: None
SymbolTable Global {
  // Symbol List: 

  // (1, 9)
  // ID: 1, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, Global
  (int32)() v
}

// MainProgram
int32 v() {
  // MainProgramScope, Parent: GlobalScope
  SymbolTable v {
    // Symbol List: 

    // (4, 11)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    int32[1:2, 1:2, 1:2] a
    // (7, 9)
    // ID: 5, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    real[1:2] b
    // (3, 10)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    logical[1:2] bool
    // (8, 20)
    // ID: 6, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    double[1:2] c
    // (5, 19)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, v
    int64[1:2] d
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (10, 1)
  bool(1) = .true.
  // (11, 1)
  bool(2) = .false.
  // (12, 1)
  a(1, 1, 1) = 10
  // (13, 1)
  a(2, 1, 1) = 20
  // (14, 1)
  a(2, 2, 1) = 30
  // (15, 1)
  a(1, 2, 1) = 40
  // (16, 1)
  a(1, 1, 2) = 50
  // (17, 1)
  a(2, 1, 2) = 60
  // (18, 1)
  a(1, 2, 2) = 70
  // (19, 1)
  a(2, 2, 2) = 80
  // (20, 1)
  d(1) = 1000000
  // (21, 1)
  d(2) = 100
  // (22, 1)
  b(1) = 3.468947
  // (23, 1)
  b(2) = 3000.46
  // (24, 1)
  c(1) = 9743589.40
  // (25, 1)
  c(2) = 0.40
   // (27, 1)
  write   bool()
   // (29, 1)
  write   a()
   // (30, 1)
  write   d()
   // (32, 1)
  write   b()
   // (33, 1)
  write   c()
}

