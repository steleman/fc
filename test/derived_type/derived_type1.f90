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

  type(PowerTyp) :: pt
  type(PowerCoords) :: pc
end program foo
