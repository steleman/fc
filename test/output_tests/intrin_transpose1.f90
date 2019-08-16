program vin
  integer :: val2(2,5), val3(5,5),  i, j
  
  do i = 1,5
    do j = 1,2
      val2(j,i) = mod(i * j, i + j)
    end do
  end do
    
  val3 = 10

  print *, (val2)
  print *, transpose(val2)
  
  val3 = transpose(val3)
  print *, val3

end program vin
