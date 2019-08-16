program test
      logical :: flag1, flag2
      integer :: a = (2*3) * 2
      integer :: b = 2 * 3 * 2
      if ( a / b == 2)  a = 11
      PRINT *,a
end program test
