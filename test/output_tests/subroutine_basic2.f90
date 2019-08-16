program t
  integer :: i = 11
  call printOthers(i,10)
end program t


subroutine printOthers(n,i)
  integer ,intent(in):: n,i
  PRINT *, n,i
end subroutine printOthers
