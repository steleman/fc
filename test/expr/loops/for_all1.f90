program decl_alloca
  integer ::i,a(10, 10), j
  a = 0
  forall(i=1:10, j=1:10, a(i, j) == 0) a(i, j) = 1
  print *, a
end program decl_alloca
