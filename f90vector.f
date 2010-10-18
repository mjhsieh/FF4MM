! Author: Mengjuei Hsieh, University of California Irvine.
#include "definitions.fpp"
module tienchun
   implicit none

contains

function vec3d_a2b(pos1,pos2)
   implicit none
   _REAL_             :: vec3d_a2b(3)
   _REAL_, intent(in) :: pos1(3), pos2(3)
   ! Periodic Boundary processing goes here.
   ! Arithmatics
   vec3d_a2b=pos2-pos1
end function vec3d_a2b

function vec3dlength(vecab)
   implicit none
   _REAL_             :: vec3dlength
   _REAL_, intent(in) :: vecab(3)
   vec3dlength=sqrt(dot_product(vecab,vecab))
end function vec3dlength

! just for 3d
function cross_product(r,s)
   implicit none
   _REAL_, dimension (3), intent(in) :: r,s
   _REAL_, dimension (3) :: cross_product
   ! local
   integer :: n,i,j

   do n = 1,3
      i = modulo(n,3) + 1
      j = modulo(i,3) + 1
      cross_product(n) = r(i)*s(j) - s(i)*r(j)
   end do
end function cross_product

end module tienchun
