program p

integer::a=10,d=1

call sub1
call sub2()

contains

subroutine sub1
  integer :: b = 20
  print *,b
  print *,d
  a = 20
end subroutine sub1

subroutine sub2
  integer :: b = 30
  print *,b
  print *,a
end subroutine sub2
end program  p
