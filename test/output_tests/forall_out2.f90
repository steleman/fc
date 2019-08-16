program test
  integer :: a(4, 3), b(4, 3, 5)
  integer :: i, j

  b = 0
  do i = 1, 4
  do j = 1, 3
      a(i, j) = 3
  end do
  end do 

  forall(i=1:4, j=1:3, a(i, j) < 6) b(i, j, a(i, j)) = -1

  print *, b

end program test
