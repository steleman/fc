program alloctest
  integer, allocatable :: a(:, :, :)
  integer :: i, j, k

  allocate(a(2, 3, 4))

  do i = 1, 2
    do j = 1, 3
      do k = 1, 4

        a(i, j, k) = i * j * k
      end do
    end do
  end do
  print *, a
end program alloctest
