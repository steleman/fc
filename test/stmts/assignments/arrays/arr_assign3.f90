program arr2
  integer:: a(10,10, 40), b(25, 23, 21, -4:23)
  integer:: i,j,k,l
  i = 1
  j = 2
  k = 3
  l = -2
  b(i,j,k,l) = 30
  b(5,j,k,l) = 10
  a(i+j, k + l, b(i,j,k,l) + i) = b(5,j,k,l) * j
  print *,a(3, 1, 31)
end program arr2
