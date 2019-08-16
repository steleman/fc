module modtest
  integer, dimension(:, :), allocatable :: array

  contains

  subroutine check()
    print *, "Allocated ?", allocated(array)

    allocate(array(10, 10))

    print *, "Allocated ?", allocated(array)

    deallocate(array)

    print *, "Allocated ?", allocated(array)
  end subroutine check

end module modtest


program test 
  use modtest

  call check()
end program test
