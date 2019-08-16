program alloctest
  integer :: a, &
    b

  a = & 
    ! multi line expr
    5
    !this is a comment
  b = &
    10 ! this is a comment
  print *, & ! multi line print
    !multiline print 
    a * b
    !this is a comment

end program alloctest
