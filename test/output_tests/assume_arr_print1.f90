module m1

contains
subroutine sub1(val)
  integer,intent(inout) :: val(:, :)
  print *,val
end subroutine sub1

end module m1

module m2
use m1

contains

  subroutine pgm

    integer :: val1(10,20),i,j
    do i = 1,10
      do j = 1,20
        val1(i,j) = (i * 10) + j
      end do
    end do
  call sub1(val1)
  end subroutine pgm

end module m2

program test
use m2
  call pgm()
end program test
