
module mod2
  integer:: c(10)
end module mod2

module mod1
  use mod2
  integer :: a = 10 ,b(20)
end module mod1

program pg

  ! Module used
  use mod1

  ! module variables 

  b(1) = 13
  c(9) = 3
  a = a + 10 + b(1)
  
  print *, a, c(9)
end program pg


