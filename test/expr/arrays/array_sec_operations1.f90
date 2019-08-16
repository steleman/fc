program test
      integer::a(10,20)
      integer::b(10,20)
      a = 1
      b = 2
      a(1:5:2,1) = b(1:5:2,2) + b(3:7:2,11)
      PRINT *,a (:,1)
end program test
