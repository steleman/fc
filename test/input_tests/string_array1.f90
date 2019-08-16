program test
  character(len=10), dimension(5) :: array
  character(len=10) :: message
  character, dimension(10) :: charArray
  integer :: a(5)
  integer :: i, j, k

  do i = 1, 5
    read *, array(i)
  end do 

  do i = 1, 5
    print *, array(i)
  end do 

  do i = 1, 5
    read(array(i), *) a(i)
  end do
  
  print *, a

end program test 
