program test
      logical :: flag1, flag2
      integer :: a = 5
      integer :: b = 5
      flag1 = (a .eq. b) .and. (b .eq. a)
      flag2 = a == b .or. b == a
      end program test
