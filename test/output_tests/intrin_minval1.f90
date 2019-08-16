
program vin
  integer::arr(1,2,3), temp(2,3)
  arr = 3
  arr(1,2,3) = 1
  temp = minval(arr,dim=1)
  print *,temp
end program vin
