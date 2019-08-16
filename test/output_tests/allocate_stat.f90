program test
  integer                               :: allocstat = -1
  integer, dimension(:, :), allocatable :: array1
  integer, dimension(:, :), allocatable :: array2

  print *, "Before ", allocstat

  allocate(array1(5, 5), array2(10, 10), STAT=allocstat)
  deallocate(array1, array2)

  print *, "After ", allocstat

end program test
