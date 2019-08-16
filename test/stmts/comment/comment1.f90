! this is a comment
program alloctest
  ! this is a comment
  ! this is a comment

  integer, allocatable :: a(:, :) 
  integer :: i, j !this is a comment
  allocate(a(2, 2))

! this is a comment


! this is a comment


  !this is a comment
  do i = 1, 2
    do j = 1, 2
      a(i, j) = i * j
    end do
    !this is a comment
  end do
  print *, a
  !this is a comment
end program alloctest
! this is a comment
