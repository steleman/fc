program t
  integer :: i = 10
  integer, dimension(10) :: a, b

  do i = 1,10,1
    a(i) = i
    b(i) = -i
  end do
  where ( a > 5 )
    a = 10 
    b = 5
  end where

  print *, a
  print *, b

end program t
