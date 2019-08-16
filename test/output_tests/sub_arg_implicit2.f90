subroutine sub1(val, val2)
  integer, intent(in) ::val, val2

  call in1()
  print *,val2
  print *,val
contains
  subroutine in1
  print *, val
  end subroutine in1
end subroutine sub1


program pgm

call sub1(10, 20)
end program pgm
