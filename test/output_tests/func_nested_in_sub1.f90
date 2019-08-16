module mod1

contains
  subroutine add(x, y)
    real :: x
    real :: y
    real :: z
    z = 0.1
    x = func1()
    contains
      real function func1()
        func1 = x + y + z
      end function func1
  end subroutine add
end module mod1


program t

    use mod1
    real :: r
    real :: x = 10.2
    call add(x, 5.1010)
    print *,  x

end program t
