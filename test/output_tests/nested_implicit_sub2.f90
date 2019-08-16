

  

program vin

integer ::a2 = 2, a(20)

a(1) = 21

call b1()
call b0()

print *,a(1)

contains
  subroutine b0
    a(1) = 33
    print *, a2
    print *, a(2)
  end subroutine b0
  
  subroutine b1
    a(2) = 44
    print *, a(1)
  end subroutine b1
end program vin
