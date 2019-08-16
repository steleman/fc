program test
  integer :: a(2, 5), r(10)
  integer :: i, j

  do j = 1, 5
    do i = 1, 2
      a(i, j) = j + i
    end do
  end do

  r  = reshape(a, (/10/))
  do i = 1, 10 
    print *, r(i)
  end do
end program test
