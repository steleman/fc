program t
  integer :: i = 10
  integer, dimension(10) :: a, b

  do i = 1,10,1
    a(i) = 5
    b(i) = -i
  end do
  where ( a > 3 ) 
    a = 3
  end where

  print *, a

end program t
