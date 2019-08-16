program vin
  integer :: val(5:9) = (/ 1, 2, 3, 4 , 5/)
  integer :: val2(1:5)
  print *, cshift(val,1)
  print *, cshift(val,2)
  print *, cshift(val,3)
  print *, cshift(val,4)
  val2 = cshift(val,5)
  print *, val2
end program vin
