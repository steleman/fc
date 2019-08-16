subroutine a(val)

integer,intent(in) :: val

call b0(val)

contains
  subroutine b0(val1)
    integer,intent(in)::val1
    print *, val1
  end subroutine b0
end subroutine a


program vin
  call a(10)
end program vin
