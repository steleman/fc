program vin
  integer :: numbers(9),k1,k2
  numbers = 3
  numbers(1:5) = numbers(4:8) + (/ (k1,k1=1,5) /)  + (/ (k2,k2=5,9) /)
  print *,numbers
end program vin
