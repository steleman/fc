program test
  integer :: a(4, 3)
  integer :: i, j

  do i = 1, 4
  do j = 1, 3
      a(i, j) = i + j
  end do
  end do 

  forall(i=1:4, j=1:3, a(i, j) /= 0) a(i, j) = -1

  print *, a

end program test
