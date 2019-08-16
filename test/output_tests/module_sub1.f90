
module a

integer::some = 1

contains
  subroutine b0(val1)
    integer,intent(in)::val1
    print *,some
    print *, val1
  end subroutine b0
  
  subroutine b1(val)
    print *, val
    print *,some
  end subroutine b1

end module a

program vin
use a

call b0(10)
call b1(11.344)

end program vin
