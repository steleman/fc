program v

logical::boolT = .TRUE.
logical::boolF = .false.

integer:: a= 10
integer(kind=8) ::d = 9999999999_8

real :: b = 3.45600000
double precision ::c=4.556_8

write (*, *) boolT
write (*, *) boolF

write (*, *) a
write (*, *) d

write (*, *) b
write (*, *) c

end program v
