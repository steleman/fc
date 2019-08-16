module mod 
  integer, parameter :: rank=3, rank2=rank*rank, total=rank2*(rank2+1)/2
  logical, parameter :: log = rank > total
end module mod

program foo
  use mod
  integer :: k
  integer :: arr1(rank2) = (/ (k, k = 1, rank2) /)

  do k = 1, (total - rank - 35)
    arr1(k) = 12
  end do

  if ((rank + total) < (rank2)) then
    arr1(rank) = 2
  end if

  if (.true. .and. (1 > rank)) then
    if (.false. .eqv. log) then
      arr1(rank + k) = 2
    end if
  end if
end program foo
