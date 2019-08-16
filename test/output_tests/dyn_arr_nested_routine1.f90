module b
contains
subroutine a(arr)
  integer:: arr(:)
  call sub()
contains 
  subroutine sub
    arr(1) = 10  
  end subroutine sub
end subroutine a
end module b

program vin
  use b
  integer::arr(10)
  call a(arr)
  print *, arr(1)
end program vin
