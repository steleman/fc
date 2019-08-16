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
  print *, a
end program alloctest
