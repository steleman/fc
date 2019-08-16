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

  lb = 3
  ub = 8
  print *, foo(array(lb:ub))

end program test
