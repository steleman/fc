program test
      character(len=80) :: string
      character(len=80) :: string2
      string = "in this test case we are trying to print really long message!!!!"
      string2 = string
      print *, string2
      print *, string
end program test
