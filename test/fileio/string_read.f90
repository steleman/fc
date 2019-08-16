program test
  character(len=10), dimension(10) :: array
  integer :: a(3, 3)
  integer :: i, j, k

  open(7, file="./fileio/input/string_read.dat")
  do i = 1, 10
    read(7, '(a9)') array(i)
  end do 

  do i = 1, 10
    read(array(i), '(9i1)') a
    print *, a
  end do
  close(7)
  
end program test 
