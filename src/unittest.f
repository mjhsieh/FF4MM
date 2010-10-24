! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
subroutine unittest
   use tienchun
   implicit none
   logical l_pass

   natom = 2
   call TCStart
       x = (/1d0, 1d0, 2d0, 3d0, 5d0, 8d0/)
       v = (/-0.0254312,-0.1482216,0.2271172,-0.4594442,1.0257714,0.2857496/)
    mass = (/4.0026d0, 4.0026d0/)
   l_pass = .true.; call test1! Vec(a to b)
   l_pass = .true.; call test2! Len(a to b)
   l_pass = .true.; call test3! EKtot
!      x = (/1d0, 0d0, 0d0, 5d0, 0d0, 0d0/)
!  r_vdw = (/1.4d0, 1.4d0/)

contains

subroutine test3
   implicit none
   _REAL_ KE
   call E_kinetic(KE,natom,mass,v)
   write(6,"(x,a,f10.4)")"Test on EKtot:",KE!need a standard
end subroutine test3

subroutine test2
   use vec3d
   implicit none
   _REAL_ mylen2, mylen
   _REAL_ mypos1(3), mypos2(3)
   _REAL_ myvec(3)
   _REAL_ myvectest(3)
   _REAL_ myveclen, myveclen2

   mylen2 = 0d0; mylen = 0d0
   mypos1=x(1:3); mypos2=x(4:6)
   myvec(1)  = mypos2(1)-mypos1(1)
   myvec(2)  = mypos2(2)-mypos1(2)
   myvec(3)  = mypos2(3)-mypos1(3)
   myveclen2 = myvec(1)**2+myvec(2)**2+myvec(3)**2
   myveclen  = sqrt(myveclen2)
   mylen=vec3d_l(myvec)

   if ( myveclen - mylen > 1d-16 ) l_pass = .false.
   if (l_pass) then
      write(6,*)"Test on length ab PASSED"
   else
      write(6,*)"Test on length ab FAILED", myveclen-mylen
   endif
end subroutine test2

subroutine test1
   use vec3d
   implicit none
   integer i
   _REAL_ mypos1(3), mypos2(3)
   _REAL_ myvec(3)
   _REAL_ myvectest(3)

   mypos1=x(1:3); mypos2=x(4:6)
   myvec(1)  = mypos2(1)-mypos1(1)
   myvec(2)  = mypos2(2)-mypos1(2)
   myvec(3)  = mypos2(3)-mypos1(3)

   myvectest = vec3d_a2b(mypos1,mypos2)

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
end subroutine unittest
