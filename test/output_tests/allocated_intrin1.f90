program test 
  integer, dimension(:, :), allocatable :: array

  print *, "Allocated ?", allocated(array)

  allocate(array(10, 10))

  print *, "Allocated ?", allocated(array)

  deallocate(array)

  print *, "Allocated ?", allocated(array)

end program test
