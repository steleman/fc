program vin
  integer::i
  do i =1,10

  if (i == 5) cycle
  if (i == 9) exit
  print *,i
  end do
end program vin
