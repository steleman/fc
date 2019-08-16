program vin
  integer:: val(10,5)
  val = 5
  val(1:5,1:2) = 3

  print *, any(val == 5)
  print *, any(val /= 5)
  print *,any(val == 5, dim=2)
  print *,any(val == 5, dim=1)
  print *,any(val /= 5, dim=2)
  print *,any(val /= 5, dim=1)
end program vin
