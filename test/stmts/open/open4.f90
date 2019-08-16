program outputdata   

   real, dimension(10) :: x
   real, dimension(10) :: y
   integer :: i

   open(1, file = 'data1.dat', status='old')  
   do i = 1,10  
      read(1,*) x(i), y(i)
      print *, x(i), y(i)
   end do  
   close(1)

   
end program outputdata
