program test
  integer :: a = (2*3) * 2
  integer :: b = 2 * 3 * 2
  integer :: arr(-1:2) 
  arr(-1) = 11

  if (a == b .and. b == 12) THEN
    a = 10
    b = 20

    if (arr(-1) == 11) THEN
      arr(-1) = 40
    END IF
    
    if (arr(-1) /= 11) THEN
      arr(-1) = 30 + arr(-1)
    END IF
    
    if (arr(-1) /= 30) arr(0) = arr(-1) + a*2
  
  END IF 
 
  print *,a,b,arr(-1),arr(0)
end program test
