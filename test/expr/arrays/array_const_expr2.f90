program vin
  integer:: K
  real ::a(4) = (/ 2, (K,K=1,3,1) /)
  print *, (/ 2, (K,K=1,3,2) /)  + (/ (K,K=3,5,2), 6 /)
  do K=1,4
    print *,a(K)
  end do

end program vin
