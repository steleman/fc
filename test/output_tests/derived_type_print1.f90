program foo
  type :: PowerTyp                   ! Data type for POWER
    integer           :: stride              ! Output stride
    integer           :: skip                ! Output skip
    character(len=80) :: filenamebase        ! Base name for output file
    integer        :: nofreq                 ! Number of frequencies to compute
    integer        :: window                 ! Start timestep for window function
    integer        :: y_index                ! Y-index
    type(PowerCoords), pointer :: coords     ! Pointer to the coordinates
    integer        :: nocoords               ! Number of coords
    type(PowerTyp), pointer    :: next       ! Pointer to the next entry
    logical        :: mine                   !Do I own any twinkles in this plane
  end type PowerTyp

  type :: PowerCoords                ! The coordinates for POWER
    integer, dimension(7)      :: coords     ! [normal i_min i_max j_min ...]
    type(PowerCoords), pointer :: next       ! Pointer to the next coordinates
  end type PowerCoords

  type(PowerTyp), target :: pt, pt2, pt3
  type(PowerCoords) :: pc
  type(PowerTyp), pointer :: pt_ptr

  pt%stride = 1
  pt%skip = 10
  pt%nofreq = 100
  pt%mine = .true.

  print *, pt%stride, pt%skip, pt%nofreq, pt%mine

  pt_ptr => pt
  pt_ptr%stride = 2
  pt_ptr%skip = 20
  pt_ptr%nofreq = 200
  pt_ptr%mine = .false.

  print *, pt_ptr%stride, pt_ptr%skip, pt_ptr%nofreq, pt_ptr%mine

  pt2%stride = 2
  pt3%stride = 3

  pt%next => pt2
  pt%next%next => pt3
  print *, pt%stride, pt%next%stride, pt%next%next%stride
  print *, pt%stride, pt2%stride, pt3%stride

end program foo
