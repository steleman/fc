

  

program vin

integer ::a2 = 2, a(20)

a(1) = 21

call b0()

print *,a(1)

contains
  subroutine b0
    a(1) = 33
    print *, a2
  end subroutine b0
end program vin
