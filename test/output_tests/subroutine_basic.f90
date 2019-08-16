program t
  integer :: n =10,i =11,j=12
  call printOthers(n,i,j,10)
end program t


subroutine printOthers(n,i,j,const)
  integer,intent(in) :: n,i,j,const
 
  PRINT *, n,i,j,const
end subroutine printOthers
