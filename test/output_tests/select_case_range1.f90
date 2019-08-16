program t
  integer :: a = 1

  select case (a)
   case (1:10)
    print *, "Yes"
   end select
end program t
