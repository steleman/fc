program t
  integer::a = 10, b = 20
 
  if (a /= 10) then
   a = 30
  else if (a == 10) then
   a = 20
  else
    a = 40
  end if
  print *,a
end program t
