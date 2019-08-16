program t
  integer :: a(10,10)
  integer :: n =10,i,j

  do i = 1,n,1
    do j = 1,n,1
      a(i,j) =  i * j + 3
  end do
    end do
    i = 10
    j = 10
  call dummy(a,n,i,j)
end program t

subroutine dummy(vin, n,i,j)
  integer,intent(inout) :: vin(10,10),n,i,j
 
  IF (n == 10) THEN
    do i = 1,n,1
      do j = 1,n,1
        print *,vin(j,i)
      end do
    end do
  END IF
  call printOthers(n,i,j)
end subroutine dummy

subroutine printOthers(n,i,j)
  integer,intent(in) :: n,i,j
 
  PRINT *, n,i,j
end subroutine printOthers
