module mod1
  contains
subroutine foo(a, b, c, d, e)
  implicit none
  integer                :: a, b
  integer, optional      :: c
  real, optional         :: d
  real(kind=8), optional :: e

  if (present(c)) then
    print *, "C is present"
  else
    print *, "C is not present"
  endif
  if (present(d)) then
    print *, "d is present"
  else
    print *, "d is not present"
  endif
  if (present(e)) then
    print *, "e is present"
  else
    print *, "e is not present"
  endif
end subroutine foo
end module mod1

program test
  use mod1
  real           :: f1 = 3.124
  real(kind = 8) :: f2 = 3.142
  call foo(1, 2)
  call foo(1, 2, 3)
  call foo(1, 2, 3, f1)
  call foo(1, 2, 3, f1, f2)

end program
