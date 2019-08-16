program v

logical::bool(2)
integer:: a(2,2,2)
integer(kind=8) ::d(2) 

real :: b(2)
double precision ::c(2)

bool(1) = .true.
bool(2) = .false.
a(1, 1,1) = 10
a(2, 1,1) = 20
a(2, 2,1) = 30
a(1, 2,1) = 40
a(1, 1,2) = 50
a(2, 1,2) = 60
a(1, 2,2) = 70
a(2, 2,2) = 80
d(1) = 1000000_8
d(2) = 100_8
b(1) = 3.468947
b(2) = 3000.46
c(1) = 9743589.40_8
c(2) = 0.40_8

write (*, *)bool

write (*, *)a
write (*, *)d

write (*, *)b
write (*, *)c

end program v
