program foo
  type :: mem_address
    integer :: loc
  end type mem_address

  type :: mem_chunk
    type(mem_address) :: start_address
    type(mem_address) :: end_address
    type(mem_address) :: next_free_chunks(3)
    integer :: data_section(3)
  end type mem_chunk

  type :: thread
    integer :: id
    type(mem_chunk) :: local_mem
    type(mem_chunk) :: shared_mem(3)
    integer :: arr(10)
  end type thread

  type :: mem_idx
    integer :: shared_mem(3)
    integer :: local_mem(7)
  end type mem_idx

  integer :: data_section(3)
  integer :: shared_mem(3)
  integer :: local_mem(7)
  type(thread) :: t0
  type(mem_idx) :: t0_mem_idx

  t0%id = 0
  t0%local_mem%start_address%loc = 10
  t0%local_mem%end_address%loc = 100

  t0%shared_mem(1)%start_address%loc = 110
  t0%shared_mem(2)%start_address%loc = 220
  t0%shared_mem(3)%start_address%loc = 330

  t0_mem_idx%shared_mem = (/1, 2, 3/)
  t0_mem_idx%local_mem(1:3) = (/11, 22, 33/)
  t0_mem_idx%local_mem(4:7) = (/44, 55, 66, 77/)

  t0%shared_mem(2)%next_free_chunks(1)%loc = 123
  t0%shared_mem(2)%next_free_chunks(2)%loc = 234
  t0%shared_mem(3)%next_free_chunks(1:3)%loc = (/321, 432, 543/)

  t0%shared_mem(2)%data_section = (/345, 456, 567/)
  data_section = t0%shared_mem(2)%data_section
  shared_mem = t0_mem_idx%shared_mem
  local_mem = t0_mem_idx%local_mem

  print *, t0%id, t0%local_mem%start_address%loc, &
    t0%local_mem%end_address%loc

  print *, t0%shared_mem(2)%next_free_chunks(1)%loc
  print *, t0%shared_mem(2)%next_free_chunks(2)%loc
  print *, t0%shared_mem(1)%start_address%loc
  print *, t0%shared_mem(2)%start_address%loc
  print *, t0%shared_mem(t0_mem_idx%shared_mem(3))%start_address%loc
  print *, data_section
  print *, shared_mem
  print *, local_mem
end program foo
