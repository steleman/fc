program readtest
  character ::msg(7)
  integer i
  
  open  (1, file = './fileio/input/4.dat', status = 'old')
  read (1, *) msg
  print *, msg
  close(1)

end program readtest
