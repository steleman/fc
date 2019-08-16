
program vin
  integer::arr(10,20,30)
  arr = 3
  arr(10,20,30) = 30
  print *, maxval(arr(10,20,:))
end program vin
