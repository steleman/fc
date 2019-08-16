program i
  integer, dimension(2, 2) :: array
  integer::b = 1 ** 2 / 1 
  integer::c = 2 ** 1
  array(b, b) = b * b
  array(b, c) = b * c
  array(c*1, b**1) = b * c
  array(c**1, c) = b * array(b, c)
  
end program i
