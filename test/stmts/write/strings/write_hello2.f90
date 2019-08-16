program test
      character(len=80) :: string
      character(len=20) :: string2
      string = "hello world"
      string2 = string
      write(*, *) string2, string, string2
end program test
