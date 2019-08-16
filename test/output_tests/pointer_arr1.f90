program foo
  integer, dimension(:, :), pointer :: Ex
  integer, dimension(2, 2), target :: arr1

  arr1 = reshape((/1, 2, 3, 4/), (/2, 2/))
  Ex => arr1
  Ex(1, 1) = 123

  print *, Ex
  print *, arr1

end program foo
