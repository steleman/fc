program readtest
  character(len=80) ::msg
  
  open  (1, file = './fileio/input/3.dat', status = 'old')
  read (1, *) msg
  print *, msg
  close(1)

end program readtest
