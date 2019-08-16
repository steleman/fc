program test
  integer :: a(10, 10), b(10,10), c(10, 10)
  integer:: i = 1,j =1,d=10,one=1
  
  do i = 1,10
    do j = 1,10
      a(j,i) = i
      b(j,i) = j

      IF (i >= 5 .and. j >= 5) THEN
        b(j,i) = b(1,1) + j * j
      END IF

    end do
  end do
 
  IF (b(1,1) >= 0) THEN
    do i = one,d,one
      j = 1
      do while (j <= d)
        c(j,i) = a(j,i) + b(j,i)
        j = j + 1
      end do
    end do
  END IF
  
  print *, c
end program test
