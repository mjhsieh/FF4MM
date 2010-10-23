! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
program unittest
   use tienchun
   use vec3d
   implicit none

   logical l_pass
   integer i
   _REAL_ mypos1(3), mypos2(3)
   _REAL_ myvec(3)
   _REAL_ myveclen, myveclen2
   !_REAL_ mytim1, mytim2, mytim3, mytim4

   natom = 2
   call TCStart
   !call RANDOM_NUMBER(x); x=x*12 ! for random numbers between 0 and 12
      x = (/1d0, 1d0, 2d0, 3d0, 5d0, 8d0/)
      f = 0d0
   mass = (/4.0026d0, 4.0026d0/)

   mypos1=x(1:3); mypos2=x(4:6)
   
   myvec(1)  = mypos2(1)-mypos1(1)
   myvec(2)  = mypos2(2)-mypos1(2)
   myvec(3)  = mypos2(3)-mypos1(3)
   myveclen2 = myvec(1)**2+myvec(2)**2+myvec(3)**2
   myveclen  = sqrt(myveclen2)

   call test1! Vec(a to b)
   call test2! Len(a to b)

contains

subroutine test1
   implicit none
   _REAL_ myvectest(3)

   myvectest = vec3d_a2b(mypos1,mypos2)

   l_pass = .true.
   do i = 1, 3
      if ( .not. l_pass ) exit
      if ( myvectest(i)-myvec(i) > 1d-16 ) l_pass = .false.
   enddo
   if (l_pass) then
      write(6,*)"Test on vector ab PASSED"
   else
      write(6,*)"Test on vector ab FAILED", myvectest-myvec
   endif
end subroutine test1

subroutine test2
   implicit none
   _REAL_ mylen2, mylen
   mylen2 = 0d0; mylen = 0d0
   l_pass = .true.

   mylen=vec3d_l(myvec)
   if ( myveclen - mylen > 1d-16 ) l_pass = .false.
   if (l_pass) then
      write(6,*)"Test on length ab PASSED"
   else
      write(6,*)"Test on length ab FAILED", myveclen-mylen
   endif
end subroutine test2
end program unittest
