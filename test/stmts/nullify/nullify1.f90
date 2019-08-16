program foo
  integer, pointer :: iptr1, iptr2
  integer, target :: i1, i2

  iptr1 => i1
  iptr2 => i2

  nullify(iptr1, iptr2)
end program foo
