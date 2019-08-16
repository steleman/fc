program t
  integer :: i = 10
  integer, dimension(10) :: a, b

  do i = 1,10,1
    a(i) = i
    b(i) = -i
  end do
  where ( a > 3 ) 
    a = 3
    b = 4
  elsewhere (a < 2) 
    a = 5
    b = 6
  end where

  print *, a

end program t
