program foo
    logical :: l = .false.
    integer :: a = -5 + (+1)

    if ((.not. l) .and. .true.) then
        print *, .not. l
    end if

    print *, -(a + (-3))
end program foo
