program t
  integer :: i, j

  L1: do i = 1, 10
    L2: do j = 1, 10 
      if (i + j < 10 ) cycle L1
      print *, i, j
    end do L2 
  end do L1

end program t
