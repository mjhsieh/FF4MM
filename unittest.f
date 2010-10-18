#include "definitions.fpp"
program unittest
use tienchun
implicit none

_REAL_ mypos1(3),mypos2(3)
_REAL_ myvec1(3),myvec2(3)
integer iter,i
_REAL_ mytim1,mytim2,mytim3,mytim4,mylength
logical l_pass

call test1

contains

subroutine test1
implicit none
call RANDOM_NUMBER(mypos1);call RANDOM_NUMBER(mypos2)
myvec1=vec3d_a2b(mypos1,mypos2)
myvec2(1)=mypos2(1)-mypos1(1)
myvec2(2)=mypos2(2)-mypos1(2)
myvec2(3)=mypos2(3)-mypos1(3)
l_pass = .true.
do i = 1, 3
   if ( .not. l_pass ) exit
   if ( myvec1(i)/=myvec2(i) ) then
      l_pass=.false.
   endif
enddo
if (l_pass) then
   write(6,*)"Test on vector ab PASSED"
else
   write(6,*)"Test on vector ab FAILED"
endif
end subroutine test1
end program unittest

subroutine wallclock( wallc )
   implicit none
   _REAL_ wallc
   integer mycount, myrate

   call system_clock( COUNT=mycount, COUNT_RATE=myrate)
   wallc = dble(mycount)/dble(myrate)
   return
end subroutine wallclock
