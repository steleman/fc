program outputdata   

   real, dimension(5) :: x, y(10)  
   integer :: i

   do i = 1, 5
    x(i) = 1.0
   end do

   do i = 1, 10
    y(i) = 10.0
   end do


   print *, x
   print *, y
   
   
end program outputdata
