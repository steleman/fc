program t
  logical ::a = .TRUE.
  logical :: b = .FALSE.
  if (.not. (a .and. b)) then
    b = .TRUE.
  end if

  print *, b
end program t
