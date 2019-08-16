program selectCaseProg
implicit none

   !local variable declaration
   integer, parameter :: a = 1008

   select case (a)
   case (1)
     print *, "Number is 1"
   case (2)
     print *, "Number is 2"
   case (3)
     print *, "Number is 3"
   case (4)
     print *, "Number is 4"
   case (5)
     print *, "Number is 5"
   case default
     print *, "Some other number", a
   end select


end program selectCaseProg

