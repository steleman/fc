program test
  character (10) :: msg
  msg = "    hello"
  print *, msg
  msg = adjustl(msg);
  print *, msg
end program test
