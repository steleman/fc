program vin
  integer:: i = 0
  do 

  i = i + 1
  if (i == 5) cycle
  if (i == 9) exit
  print *,i
  end do
end program vin
