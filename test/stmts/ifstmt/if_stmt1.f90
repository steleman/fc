program test
      logical :: flag1, flag2
      integer :: a = (2*3) * 2
      integer :: b = 2 * 3 * 2
      flag1 = a == b .and. b == 12
      if (flag1) a = 10
      PRINT *,a
end program test
