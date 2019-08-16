program test
  integer, dimension(10, 10) :: array
  integer, parameter :: l = 1
  integer, parameter :: u = 10
  array(5, l:u) = 10
end program test
