program alloctest
  integer, allocatable :: a(:)
  integer, allocatable :: b(:)
  integer :: i

  allocate(a(6), b(8))

  do i = 1, 6
    a(i) = i
  end do

  do i = 1, 10
    b(i) = i
  end do

  print *, a
  print *, b
end program alloctest
