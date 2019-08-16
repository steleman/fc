program decl_alloca
  integer,save ::i = 10
  if (i == 9) then
   i = 1; i = 2 
  elseif (i == 10) then
  i= 3;
  i = 4
  end if

  print *,i

end program decl_alloca
