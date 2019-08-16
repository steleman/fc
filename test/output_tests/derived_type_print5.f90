module communicate_mod
  type :: info_type
    sequence
    real(kind=4), dimension(12) :: r
    integer, dimension(13) :: i
  end type info_type

contains
subroutine subr1(info)
  type(info_type), intent(in) :: info
  print *, info%r(1)
  print *, info%r(2)
  print *, info%r(3)
end subroutine subr1

subroutine subr2(i1)
  integer :: i1(:)
end subroutine subr2

end module communicate_mod

subroutine init
  use communicate_mod
  type(info_type) :: info
  info%r(1) = 12.0
  info%r(2:3) = (/23.0, 34.0/)
  call subr1(info)
end subroutine

program foo
  call init
end program foo
