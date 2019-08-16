module parameter_mod
  integer, parameter ::   one_byte = selected_int_kind(2)   !   One byte integer
  integer, parameter ::   two_byte = selected_int_kind(3)   !   Two byte integer
  integer, parameter ::  four_byte = selected_int_kind(5)   !  Four byte integer
  integer, parameter :: eight_byte = selected_int_kind(15)  !  Eight byte integer
end module parameter_mod

program foo
  use parameter_mod
  print *, one_byte, two_byte, four_byte, eight_byte
end program foo
