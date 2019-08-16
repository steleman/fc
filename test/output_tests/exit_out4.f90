program t
  integer :: i, j

  L1: do i = 1, 10
    L2: do j = 1, 10 
      if (i + j < 10 ) exit L1
      print *, i, j
    end do L2 
  end do L1
  print *, "Only this will be printed"

end program t
