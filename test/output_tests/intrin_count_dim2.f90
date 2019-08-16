program vin
 integer :: numbers(5,10,2), res(10,2)
 
  numbers = 5
  res = count(numbers /= 0,dim=1)
  print *,count(numbers == 0,dim=2)
  print *,count(numbers == 0,dim=3)
  print *,count(numbers == 0)
  print *,res
end program vin
