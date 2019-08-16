module mod
  contains
  function add(x, y)
    real :: x
    real :: y
    real  :: add(10)
    integer :: i
    do i = 1, 10
      add(i) = x + y
    end do
  end function add
end module mod

program t
  use mod
  real :: r(10)
  r = add(10.55, 5.1010)
  print *, r

end program t
