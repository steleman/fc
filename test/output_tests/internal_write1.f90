program test
  character(4)             :: char_length
  integer                  :: line_length

  line_length = 100

  write(char_length, '(I4)') line_length

  print *, char_length

end program test
