program arr2
  integer:: a(10,10), b(-10:10)
  integer:: c = -3
  integer:: d = 5
  a(d,d) = 10
  b(c) = 10
  print *,(a(d,d) + b(-3))
end program arr2
