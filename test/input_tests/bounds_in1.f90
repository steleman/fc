program alloctest
  integer, allocatable :: a(:, :, :)
  integer :: i, j, k
  integer :: m, n, o

  read *, m, n, o

  allocate(a(m, n, o))

  do i = 1, m
    do j = 1, n
      do k = 1, o

        a(i, j, k) = i * j * k
      end do
    end do
  end do
  print *, ubound(a, 1)
  print *, ubound(a, 2)
  print *, ubound(a, 3)
  print *, lbound(a, 1)
  print *, lbound(a, 2)
  print *, lbound(a, 3)
end program alloctest
