program test
      character, dimension(10) :: arr
      character(len=100) :: msg
      msg = "this is a comment !!!!!!!!"
      print *, index(msg, 't')
      print *, index(msg, '!')
      print *, index(msg, 'n')
end program test
