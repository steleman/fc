program test
      logical :: flag1, flag2, flag3, flag4, flag5, flag6
      integer :: a = 5
      integer :: b = 5
      integer :: array(4, 3)
      array(3, 3) = a
      flag2 = array(3, 3) + a .eq.  a + b

      end program test
