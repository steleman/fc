program test
      logical :: flag1, flag2, flag3, flag4, flag5, flag6
      integer :: a = 5
      integer :: b = 5
      flag1 = (a .eq. b)
      flag2 = (a .ne. b)
      flag3 = a .lt. b
      flag4 = (a .le. b)
      flag5 = a .gt. b
      flag6 = a .ge. b

      end program test
