program i
  integer ::b
  integer ::a
  integer ::c
  integer, dimension(5) :: array
  a = 3 
  b = 20
  array(1) = 1
  array(2) = 2
  array(3) = 3
  array(4) = array(1) + array(2) * array(3)
  array(5) = a + b
  c = array(1) + array(2) + array(3) + array(4)

  
end program i
