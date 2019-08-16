

module mod1
  integer :: a = 10
end module mod1

program pg

  ! Module used
  use mod1

  ! module variables 
  print *, a
end program pg


