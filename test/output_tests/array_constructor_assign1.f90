program test
  integer, dimension(10) :: array
  integer :: k
  array(1:10) = (/ (k,k=1,10) /)
  print *,array
end program test
