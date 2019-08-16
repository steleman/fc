program vin
  integer :: val2(2,5), i, j, val3(2,5,10)
  
  do i = 1,5
    do j = 1,2
      val2(j,i) = mod(i * j, i + j)
    end do
  end do
    

!  print *, minloc(val2,dim=1)
  print *, minloc(val2,dim=1)
  print *, minloc(val2,dim=2)
  print *, minloc(val2(1,:),dim=1)
  print *, minloc(val2(:,1),dim=1)


  val3 = 30
  val3(2,1,1) = 50
  
  print *,minloc(val3,dim=3)
  print *,minloc(val3,dim=2)
  print *,minloc(val3,dim=1)
  print *,minloc(val3(:,1,1),dim=1)
  print *,minloc(val3(1,:,1),dim=1)
  print *,minloc(val3(1,1,:),dim=1)

  ! TODO: Yet to support
  !print *,minloc(val3(:,1,:),dim=2)

end program vin
