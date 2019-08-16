program test
  integer, dimension(:, :, :), allocatable :: array

  allocate(array(2, 2, 2))

  array = 5
  print *, array

  deallocate(array)
  print *, array


end program test
