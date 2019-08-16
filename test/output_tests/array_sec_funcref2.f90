module test_mod
  contains
function sum_function(array)
  real, dimension(:, :) :: array
  real :: sum_function
  integer :: i, j
  sum_function = 0.0

  do i = 1, 5
    do j = 1, 5
      sum_function = sum_function + array(i, j)

    end do
  end do
end function sum_function
end module test_mod

program test
  use test_mod
  real, dimension(10, 10) :: array

  array = 99

  print *, sum_function(array(1:5, 1:5))
end program
