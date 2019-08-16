program test
      integer::a(10,20),c
      integer::b(10,20)
      a = 1
      b = 2
      c = 2
      
      a(b(1,1):b(2,1) + 1:c,c+2 -4*8 + 16*2) = 10
      print *,a
end program test
