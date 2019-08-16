module mod1
  type :: proc
    integer :: pid
    real :: prio
    integer :: nice
    integer, pointer :: address
    integer :: fd(3)
    type(proc), pointer :: next
  end type proc

  integer :: pid = 123
  integer :: nice = 7
end module mod1

subroutine print_pid(process)
  use mod1
  type(proc) :: process
  print *, process%pid
end subroutine print_pid

subroutine print_prio(process)
  use mod1
  type(proc) :: process
  print *, process%prio
end subroutine print_prio

program foo
  use mod1
  integer :: fd(3)
  type(proc), target :: vim, gdb
  type(proc), pointer :: proc_ptr

  vim%pid = 12
  vim%prio = 1.0
  vim%fd = (/0, 1, 2/)

  gdb%pid = 13
  gdb%prio = 2.0

  call print_pid(vim)
  call print_prio(gdb)

  proc_ptr => vim
  proc_ptr%next => gdb

  ! must be able to differentiate pid here.
  print *, pid, vim%pid, vim%prio

  fd = vim%fd
  print *, fd, vim%fd(1)

  print *, proc_ptr%pid, proc_ptr%prio
  print *, proc_ptr%next%pid, proc_ptr%next%prio
end program foo
