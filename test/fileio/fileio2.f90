program readtest
  integer, dimension(10) :: x
  
  open  (1, file = './fileio/input/2.dat', status = 'old')
  read (1, *) x
  print *, x
  close(1)

end program readtest
