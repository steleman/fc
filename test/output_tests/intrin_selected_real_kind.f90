program foo
  integer, parameter :: sfp1 = selected_real_kInd(6, 37)
  integer, parameter :: sfp2 = selected_real_kInd(13, 99)

  print *, sfp1, sfp2
end program foo

