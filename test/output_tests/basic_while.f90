program test
  integer :: arr(10)
  integer:: i = 1
  integer:: sumVal = 0
  
  do while (i <= 10)
    arr(i) = i
    i = i + 1
  end do
 
  i = 1
  do while (i <= 10)
    sumVal = sumVal +  arr(i)
    i = i + 1
  end do

  print *,sumVal
end program test
