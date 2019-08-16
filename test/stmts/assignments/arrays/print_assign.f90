program test_print
  integer(kind=4):: a =1 , b =2 , arr(3, 2)
  arr(1,1) = 1
  arr(2, 1) = 1
  arr(3, 1) = 1
  arr(1,2) = 2
  arr(2, 2) = 2
  arr(3, 2) = 2
  print *,arr
end program test_print
