module mod

contains
  real function add(x, y)
    real :: x
    real :: y
    real :: z
    z = 0.1
    add = x + y + z
  end function add
end module mod


program t

    use mod
    real :: r
    r = add(10.55, 5.1010)
    print *,  r

end program t
