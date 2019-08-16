program vin
  integer:: a=1,b=10,c=-3,d=4
  print *, min(a,b,d)
  print *, min(a,b,c)
  print *, min(b,c,d)
  print *, min(b,c)
  print *, max(a,b,d)
  print *, max(a,d,c)
  print *, max(b,c,d)
  print *, max(b,c,d,a)
  print *, max(a,d)
end program vin
