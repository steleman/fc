program test
      character, dimension(10) :: arr
      integer, dimension(10, 9) :: arr2
      integer, dimension(:, :), allocatable :: arr3

      print *, size(arr)
      print *, size(arr2)
      allocate(arr3(5, 6))
      print *, size(arr3)

end program test
