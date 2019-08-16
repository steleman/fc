module mod1
  integer, pointer :: i_ptr
  integer, pointer :: iarr_ptr(:)
  integer, target :: i
  integer, target :: iarr(5)
end module mod1

program foo
  use mod1

  i = 12
  iarr = (/1, 2, 3, 4, 5/)
  i_ptr => i
  iarr_ptr => iarr

  iarr(1) = 13
  iarr_ptr(2) = 14
  print *, i_ptr
  print *, iarr_ptr
end program foo
