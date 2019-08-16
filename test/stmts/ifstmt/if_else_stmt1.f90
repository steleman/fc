program t
  integer::a = 10, b = 20
 
  if (a == 10) then
   a = 30
   b = 20
  else
   a =  b + 20
   b = 40
  end if

  print *,a, b
end program t
