program test
  character(len=10), dimension(5) :: array
  character(len=10) :: message
  character, dimension(10) :: charArray
  integer :: a(3, 3)
  integer :: i, j, k=0

  do i = 1, 3
    do j = 1, 3
      a(i, j) = k
      k = k + 1
    end do 
  end do
  write(*, '(3i3)') ((a(i, j), j = 1, 3), i = 1, 3)

end program test 
