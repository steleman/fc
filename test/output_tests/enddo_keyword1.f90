program vin
  
  integer :: a(10,20+3:25)
  integer i
  a= 10
  do i = 1,10
    a(i,24) = 5
  enddo
  print *,a
end program vin
