#include "definitions.fpp"
module tienchun
implicit none

contains

function vec3d_a2b(pos1,pos2)
implicit none
_REAL_             :: vec3d_a2b(3)
_REAL_, intent(in) :: pos1(3), pos2(3)
vec3d_a2b=pos2-pos1
end function vec3d_a2b

function vec3dlength(vecab)
implicit none
_REAL_             :: vec3dlength
_REAL_, intent(in) :: vecab(3)
vec3dlength=sqrt(dot_product(vecab,vecab))
end function vec3dlength

end module tienchun
