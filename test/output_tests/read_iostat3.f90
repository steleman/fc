program test
  integer :: ios
  integer :: array(10)

  open(unit=10, file="./input/read_iostat3.in")

  read(10, *, iostat=ios) array

  if (ios < 0) then
    print *, "Can not read all the elements"
  else
    print *, array
  endif


  close(10)

end program test
