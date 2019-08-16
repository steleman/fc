program vin
  integer :: numbers(9,9)
  numbers = 3
  numbers(1,1:3) = numbers(2:4,3) + numbers(6,3:5) + 2 - numbers(8,2)
  print *,numbers
end program vin
