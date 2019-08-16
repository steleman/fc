program foo
  type :: linked_list
    integer :: val ! Thank you for keeping "data" as a keyword Fortran
    integer :: id
    type(linked_list), pointer :: next
  end type linked_list

  integer, target :: i1
  integer, pointer :: iptr
  logical :: l1

  type(linked_list), target :: head, node0, node1, node2
  type(linked_list), pointer :: ll_walk

  head%id = 0
  node0%id = 1
  node1%id = 2
  node2%id = 3

  ! the commented code is producing non-deterministic result in some versions
  ! of gfortran.

  l1 = associated(iptr)
  ! print *, l1
  iptr => i1
  print *, associated(iptr)

  ! print *, associated(head%next)
  ! print *, associated(node0%next)
  ! print *, associated(node1%next)
  ! print *, associated(node2%next)

  head%next => node0;
  node0%next => node1;
  node1%next => node2;

  print *, associated(head%next)
  print *, associated(node0%next)
  print *, associated(node1%next)
  ! print *, associated(node2%next)

  ! The following snippet segfaults in older gfortran (eg. 5.5),
  ! temporarily commenting
  ! ll_walk => head
  ! do while (associated(ll_walk))
  !   print *, ll_walk%id
  !   ll_walk => ll_walk%next
  ! end do
end program foo
