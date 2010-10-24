! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
subroutine TCStart
   use tienchun
   implicit none

   integer ierr(64), i
   ierr=0; i=1

   allocate( mass(natom  ),stat=ierr(i)); i=i+1
   allocate(r_vdw(natom  ),stat=ierr(i)); i=i+1

   allocate(    x(natom*3),stat=ierr(i)); i=i+1
   allocate(    f(natom*3),stat=ierr(i)); i=i+1
   allocate(    v(natom*3),stat=ierr(i)); i=i+1

   if ( sum(ierr) > 0 ) then
      print *, "allocation error"; stop
   end if
    mass = 0d0
       x = 0d0
       f = 0d0
       v = 0d0
   r_vdw = 0d0
end subroutine TCStart
