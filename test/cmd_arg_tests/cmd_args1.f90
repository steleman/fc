subroutine foo() 
  integer :: nargs, num1, num2
  character(len=5) :: carg1, carg2 
  nargs = command_argument_count()
  call get_command_argument(1, carg1)
  call get_command_argument(2, carg2)
  print *, nargs
  !print *, carg
  read(carg1, *) num1
  read(carg2, *) num2
  print *, num1
  print *, num2
end subroutine foo

program test
  call foo()
end program test 
