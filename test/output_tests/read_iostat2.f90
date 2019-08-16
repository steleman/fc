program test
  character(80) :: line
  integer :: ios

  open(unit=10, file="./input/read_iostat2.in")

  do
    read(10, *, iostat=ios) line

    if (ios<0) then
      print *, "EOF"
      exit
    
    elseif ( ios > 0) then
      write(*, *) "Some thing went wrong"
    endif

    print *, line
  enddo
  close(10)

end program test
