subroutine a(arr)
  integer:: arr(10)
    integer::i
    arr(1) = 10  

    do i = 1, 10
     arr(i) = 1
     return
    end do
    arr(2) = 20
end subroutine a

program vin
  integer::arr(10)
  call a(arr)
  print *, arr(1)
end program vin
