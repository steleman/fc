program test
  real, dimension(:), allocatable :: array

  allocate(array(10))

  array = 10
  print *, array

  deallocate(array)


end program test
