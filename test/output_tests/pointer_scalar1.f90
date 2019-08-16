program foo
  integer, target :: i = 1
  integer, pointer :: iptr1, iptr2

  iptr1 => i
  iptr1 = 9
  iptr2 => iptr1
  iptr2 = 2

  print *, i, iptr1, iptr2

  i = 3
  print *, i, iptr1, iptr2
end program foo
