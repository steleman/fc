program test
  integer :: a(10, 10), b(10,10), c(10, 10)
  integer:: i = 1,j =1
  
  do i = 1,10
    do j = 1,10
      a(j,i) = i
      b(j,i) = j
    end do
  end do
 
  do i = 1,10
    do j = 1,10,1
      c(j,i) = a(j,i) + b(j,i)
    end do
  end do
  
  print *, c
end program test
