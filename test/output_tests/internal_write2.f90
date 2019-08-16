program test
  character(len=80)     :: filename
  integer               :: my_id

  my_id  = 0
  write(filename,'(A,I1,A)') 'OBJ_', my_id, '.dat'

  print *, filename

end program test
