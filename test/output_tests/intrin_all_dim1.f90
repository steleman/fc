program vin
  integer:: val(10,5)
  val = 5

  print *, all(val == 5)
  print *, all(val /= 5)
  print *,all(val == 5, dim=2)
  print *,all(val == 5, dim=1)
  print *,all(val /= 5, dim=2)
  print *,all(val /= 5, dim=1)
end program vin
