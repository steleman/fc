program test
  integer :: a = 32564
  integer :: err
  character(len=90) :: string

  write(string, '(i5)', iostat=err) a

  print *, err
  print *, string

end program
