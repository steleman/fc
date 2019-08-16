program test

  real :: a(2)
  real :: b(2, 3)
  real :: c(3, 2)

  a(1) = 1.0
  a(2) = 2.0

  c = spread(a(1:2), 1, 3)
  print *, c

  b = spread(a(1:2), 2, 3)
  print *, b
end program
