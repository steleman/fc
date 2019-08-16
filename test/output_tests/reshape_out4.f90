program test
  integer :: a(10), r(5, 2)
  integer :: i, j

  do j = 1, 10
      a(j) = j 
  end do

  r  = reshape(a, (/5, 2/))
  do i = 1, 5
    do j = 1, 2
      print *, r(i, j)
    end do
  end do
end program test
