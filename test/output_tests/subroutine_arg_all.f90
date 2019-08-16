subroutine sub2(a,b,c,d,e)
  integer,intent(in) :: a
  real,intent(in) :: b
  integer(kind=8),intent(in) :: c
  real(kind=8),intent(in) :: d
  character(len=6),intent(in) :: e
  
  print *,"Hello, world", a,b,c,d
end subroutine sub2

program pgm

call sub2(5,6.00, 56_8, 6.01_8, "string")
end program pgm

