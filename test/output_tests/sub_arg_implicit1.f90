subroutine sub1(val)
  integer, intent(in) ::val

  call in1()
contains
  subroutine in1
  print *, val
  end subroutine in1
end subroutine sub1


program pgm

call sub1(10)
end program pgm
