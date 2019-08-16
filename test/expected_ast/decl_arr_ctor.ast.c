Program: decl_arr_ctor.f90
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

    // (2, 14)
    // ID: 2, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, foo
    int32[1:3] arr
    // (2, 14)
    // ID: 3, NonConstant, NonAllocatable, NonTarget, NonPointer, StaticLocal, Intent_None, foo
    int32 foo.tmp.0
    // (2, 14)
    // ID: 4, NonConstant, NonAllocatable, NonTarget, NonPointer, Alloc_None, Intent_None, foo
    (int32)(...) lbound
  }

  // Specification Constructs: 

  EntityDeclList {
  }

  // Execution Constructs: 

  // (2, 14)
  foo.tmp.0 = 1
  // (2, 14)
  arr(foo.tmp.0) = 1
  // (2, 14)
  t.1 = foo.tmp.0 + 1
  foo.tmp.0 = t.1
  // (2, 14)
  arr(foo.tmp.0) = 2
  // (2, 14)
  t.2 = foo.tmp.0 + 1
  foo.tmp.0 = t.2
  // (2, 14)
  arr(foo.tmp.0) = 3
  // (3, 3)
  printf(arr())
}

