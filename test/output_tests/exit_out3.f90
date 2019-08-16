program t
  integer :: i, j

  do i = 1, 10
    do j = 1, 10 
      if (i + j < 10 ) exit
      print *, i, j
    end do 
  end do

end program t
