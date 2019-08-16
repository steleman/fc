subroutine subr1(arr1, arr2)
  integer :: arr1(*)
  integer, dimension(*) :: arr2
  integer :: i

  do i = 1, 5
    print *, arr1(i), arr2(i)
  end do
end subroutine subr1

program foo
  integer :: arr1(5) = (/1, 2, 3, 4, 5/)
  call subr1(arr1, arr1)
end program foo
