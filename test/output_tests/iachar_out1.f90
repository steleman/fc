program test
  character(len=80) :: msg1 = "hello world"
  print *, msg1(2:2)
  print *, IACHAR(msg1(2:2))
end program test
