program t
  integer :: i = 10
  integer, dimension(10) :: a

  do i = 1,10,1
    a(i) = i
  end do
  where ( a > 3 ) a = 3

  print *, a

end program t
