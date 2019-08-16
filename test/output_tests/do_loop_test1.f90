program arr1
  
  integer:: i=1, j = 3
  do i =1,j,1
    print *,"i=",i
    print *,"j=",j
    j = 4
  end do 
  print *,i
  print *,j
  
  j = 3
  do i =1,j,i
    print *,"i=",i
    print *,"j=",j
    j = 4
  end do 
  print *,i
  print *,j
end program arr1
