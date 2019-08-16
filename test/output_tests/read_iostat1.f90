program test
  integer :: num
  integer :: ios

  open(unit=10, file="./input/read_iostat1.in")

  do
    read(10, *, iostat=ios) num

    if (ios<0) then
      print *, "EOF"
      exit
    
    elseif ( ios > 0) then
      write(*, *) "Some thing went wrong"
    endif

    print *, "NUmber is ", num
  enddo
  close(10)

end program test
