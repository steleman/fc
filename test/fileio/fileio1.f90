program readtest
  integer, dimension(10) :: x
  integer, dimension(10) :: y
  integer :: i
  

   open  (1, file = './fileio/input/1.dat', status = 'old')
   do i = 1,10 
      read(1,*) x(i), y(i)
   end do 

   do i = 1,10  
      print *, x(i), y(i)
   end do 

end program readtest
