      subroutine func(a,x,y,z)

        integer x,y,z
        integer :: a(x,y,z)
        integer ::i,j,k

        do k = 1, z
          do j = 1, y
            do i = 1, x
              a(i, j, k) = 20
            end do
          end do
        end do
      end

      program prog
        integer :: a(1,2,3)

        a(1,1,1) = 3

        call func(a,1,2,3)
        print *,a
      end
