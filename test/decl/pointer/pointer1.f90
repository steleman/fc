program foo
  integer, target :: i
  integer, target :: arr(10)
  integer, pointer :: iptr
  integer, pointer :: arrptr(:)

  iptr => i
  arrptr => arr
end program foo
