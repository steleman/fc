program test
      character, dimension(10) :: arr
      character(len=100) :: msg
      character(len=100) :: final_msg
      msg = "compiler " // " tree " //  "technologies'" // " compiler   "
      final_msg = '( A ' // trim(msg) // ' )'
      print *, final_msg
end program test
