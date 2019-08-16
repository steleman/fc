program vin
  integer:: val(10,5),val2(5)
  val = 5

  val2 = sum(val, dim=1)
  print *,sum(val, dim=2)
  print *,sum(val, dim=2,mask=val /=5)
  print *,sum(val, dim=1,mask=val ==5)
  print *,val2
end program vin
