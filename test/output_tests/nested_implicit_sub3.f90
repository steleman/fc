

  

program vin

integer ::a2 = 2, a(20)

a(1) = 21

call b0()
call b1()

print *,a(1)
print *,a(2)
print *,a2

contains
  subroutine b0
    a(1) = 33
    a2 = 34
    call b1()
  end subroutine b0
  
  subroutine b1
    a(2) = 44
    a(1) = a(1) + 2
    print *,a(1)
  end subroutine b1
end program vin
