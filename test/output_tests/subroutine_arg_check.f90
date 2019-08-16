subroutine sub1(s)
  print *,"Hello, world"
end subroutine sub1

subroutine sub2(a,b)
  integer,intent(in) :: a
  print *,"Hello, world", a,b
end subroutine sub2

program pgm

call sub1(3.00)
call sub2(5,6.00)
end program pgm

