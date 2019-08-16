subroutine a(val1, val2)

integer,intent(in) :: val1
real, intent(in) :: val2

call b0(val1)
call b1(val2)

contains
  subroutine b0(val1)
    integer,intent(in)::val1
    print *, val1
  end subroutine b0
  
  subroutine b1(val)
    print *, val
  end subroutine b1

end subroutine a


program vin
  call a(10, 20.000)
end program vin
