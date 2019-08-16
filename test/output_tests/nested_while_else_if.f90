program test
  integer :: a(10, 10), b(10,10), c(10, 10)
  integer:: i = 1,j =1
  
  do while (i <= 10)
    j = 1
    do while (j <= 10)
      a(j,i) = i
      b(j,i) = j

      IF (i >= 5 .and. j >= 5) THEN
        b(j,i) = b(1,1) + j * j
      ELSE IF (i > 8 ) THEN
        a(i,j) = 3
      END IF

      j = j + 1
    end do
    i = i + 1
  end do
 
  i = 1
  IF (b(1,1) >= 0) THEN
    do while (i <= 10)
      j = 1
      do while (j <= 10)
        c(j,i) = a(j,i) + b(j,i)
        j = j + 1
      end do
      i = i + 1
    end do
  END IF
  
  print *, c
end program test
