module module1
contains

subroutine mod1()

integer:: val2 = 1
call array_ret(1)
call sub2()
call array_ret(2)
print *, val2
contains
  subroutine array_ret(val)

      integer, intent(in) :: val
      integer :: val2 = 5
      logical :: openfile = .false.
      if (val == 1) then
      openfile = .true.
      end if
      val2 = val2 + 3
      print *, openfile
      print *, val2
    end subroutine  array_ret

    subroutine sub2
      val2  = 5
    end subroutine sub2
end subroutine  mod1
end module module1

    program prog
    use module1
    call mod1()
    end program prog
