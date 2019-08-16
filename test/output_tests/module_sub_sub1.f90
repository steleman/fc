
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
    call b2(val)

    contains
    
      subroutine b2(val2)
        print *,val2
      end subroutine b2

  end subroutine b1

end module a

program vin

use a

call internal(13.413)
 
contains

 subroutine internal(val)
  call b0(10)
  call b1(val)
 end subroutine internal

end program vin
