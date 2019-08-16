program test
  integer ::  a(10,20)
  integer ::  b(10,20)
  a(1:10,1:20) = 3
  b(1:10,1:20) = 6
  a(1:10,1) = b(1,1:10)
  print *,a
end program test
