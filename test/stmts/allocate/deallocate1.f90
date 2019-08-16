program alloctest
  integer, allocatable :: a(:)
  integer, allocatable :: b(:)
  integer :: i
  integer :: st

  allocate(a(6), b(8), stat = st)
  print *, "Allocation status ", st

  do i = 1, 6
    a(i) = i
  end do

  do i = 1, 10
    b(i) = i
  end do

  print *, a
  print *, b
  deallocate(a, b, stat = st)
  print *, "De Allocation status ", st
end program alloctest
