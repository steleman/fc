program vin
  integer :: val2(2,5), i, j, val3(2,5,10)
  
  do i = 1,5
    do j = 1,2
      val2(j,i) = mod(i * j, i + j)
    end do
  end do
    
  print *, minloc(val2,mask=val2>2)
  print *, minloc(val2,mask=val2<3)
  print *, minloc(val2(:,1) + val2(:,2) ,mask=val2(:,1)<3)
  
  print *, maxloc(val2 + val2 ,mask=val2>2)
  print *, maxloc(val2,mask=val2<3)
  print *, maxloc(val2(:,1),mask=val2(:,1)<3)

end program vin
