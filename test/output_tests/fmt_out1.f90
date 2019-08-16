program test
  character(len=10), dimension(10) :: array
  integer :: a(3, 3)
  integer :: i=1, j=2, k=3, val = 4, i2 = 5, j2 = 6, val2 = 7

  !write(*,'(/," At", 2(2i2," change to", i2,:,", "))') i, j, val, i2, j2, val2
  do i = 1, 10
    write(*, '(/"Puzzle ", i0)') i 
  end do

end program test 
