module mod1
  contains
subroutine foo(a, b, c)
  implicit none
  integer :: a, b
  integer, optional :: c

  if (present(c)) then
    print *, "C is present"
  else
    print *, "C is not present"
  endif
end subroutine foo
end module mod1

program test
  use mod1
  call foo(1, 2, 0)
  call foo(1, 2, 3)
  call foo(1, 2)

end program
