module mod1
  contains
  function foo(x)
      integer, dimension(:) :: x
      integer :: foo(size(x))

      foo = 10

  end function foo
end module mod1


program test
  use mod1
  integer :: lb, ub

  integer :: array(10)
  integer :: res(6)

  array = 5

  lb = 2
  ub = 7

  res = foo(array(lb:ub) * 4) * foo(array(lb:ub)) * 4
  print *, res

end program test
