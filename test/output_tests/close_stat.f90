program test
  integer                               :: closestat = -1

  print *, "Before ", closestat
  open(unit=10, file = "1.in")
  close(10, iostat=closestat)
  print *, "After ", closestat

end program test
