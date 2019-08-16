program test
      integer :: a = (2*3) * 2 * 2
      integer :: b = 2 * 3 * 2
      
      if ( a / b == 2) THEN
        a = 11
      END IF
      
      PRINT *,a
end program test
