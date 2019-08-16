program test

  real :: a(3)
  real :: sq = 9

  a(1) = 1.0
  a(2) = 2.0
  a(3) = 4.0

  a = sqrt(sq) * a
  print *, a

end program
