program i
  integer, dimension(2, 2) :: array
  array(1, 1) = 1 + 1
  array(1, 2) = 1 * 2
  array(2, 1) = 1 ** 2
  array(2, 2) = array(1, 1) + array(1, 2) * array(2, 1)
  
end program i
