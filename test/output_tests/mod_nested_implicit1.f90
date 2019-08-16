module mod1

integer:: glob = 10

contains

  subroutine mod1_sub
  integer ::intern = 1
  print *,glob
  glob = glob + 1

  contains
    subroutine internal
      print *,intern
    end subroutine internal
  end subroutine mod1_sub

end module mod1

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
    use mod1
    
    call mod1_sub()
    a(2) = 44
    a(1) = a(1) + 2
    print *,a(1), glob
  end subroutine b1
end program vin
