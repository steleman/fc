program test
  character (len = 80) :: msg
  character :: ch
  integer :: i
  msg = "hello world"
  read *, i
  ch = msg(i:i)
  print *, ch
end program test
