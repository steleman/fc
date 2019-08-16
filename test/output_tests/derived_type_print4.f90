module mod1
  type :: t1
    integer :: i1
    integer :: i2
  end type t1

  type(t1) :: t1_obj
  integer :: i1 = 123

  contains
    subroutine subr1
      type(t1) :: t2_obj

      t1_obj%i1 = 11
      t1_obj%i2 = 22

      t2_obj%i1 = 111
      t2_obj%i2 = 222

      print *, t1_obj%i1
      print *, t1_obj%i2

      print *, t2_obj%i1, t2_obj%i2

      print *, i1
    end subroutine
end module mod1

program foo
  use mod1
  call subr1
end program foo
