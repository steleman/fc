module m1

contains
subroutine sub1(val)
  integer,intent(inout) :: val(:)
  integer :: i
  do i = 1,10
    print *,val(i)
  end do

end subroutine sub1

end module m1

module m2
use m1

contains

  subroutine pgm

    integer :: val1(10),i
    do i = 1,10
      val1(i) = i
    end do
  call sub1(val1)
  end subroutine pgm

end module m2

program test
use m2
  call pgm()
end program test
