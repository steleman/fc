module mod1
contains
  function array_ret()
       integer :: array_ret(9)
       real :: numbers(9)=(/  5,  6,  3,  8,  9,  1,  7,  4,  2 /)
       array_ret = INT(numbers)
    end function array_ret
end module mod1

    program prog
    use mod1
    integer ::val(9)
    val = array_ret()
    print *,val
    end program prog
