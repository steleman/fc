      subroutine func(a,x,y,z)

        integer x,y,z
        integer :: a(x,y,z), b(x,y)
        integer ::i,j=1,k=2

            do k = 1, z
              do j = 1, y
                do i = 1, x
                  a(i, j, k) = 20
                end do
              end do
            end do
            
            j = 1 
            k = 2
            do i = 1, x
              a(i, j, k) = 20
              b (i,j) = 10
              b (i,k) = 13
            end do

            do i = 1,x-1
             a(i,j,k) = b (i,j) + b (i+1,k)
            end do
      end

      program prog
        integer :: a(10,2,3)

        a(1,1,1) = 3

        call func(a,10,2,3)
        print *,a
      end
