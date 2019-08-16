program test
  integer :: n= 10
  real(kind=8) :: cfl,cfll
  cfl = 2.0

  cfll=0.1+(n-1.)*cfl/20.0
  print *, cfll
end program test
