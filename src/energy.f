! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
subroutine E_kinetic(KE,natom,mass,v,oldv)
   implicit none
   integer, intent(in) :: natom 
   _REAL_, intent(out) :: KE
   _REAL_, dimension(natom), intent(in) :: mass
   _REAL_, dimension(natom*3), intent(in) :: v
   _REAL_, dimension(natom*3), intent(in) :: oldv

   ! Local
   _REAL_ tmpvec(3)
   integer i, i1, i2, i3 

   KE = 0d0; i1 = 1; i3 = 3
   do i=1, natom
      ! averaging the v from t0-dt/2 and t0+dt/2 (by dividing it latter)
      tmpvec = v(i1:i3) + oldv(i1:i3)
      KE = KE + mass(i) * dot_product(tmpvec,tmpvec)
      i1 = i1 + 3; i3 = i3 + 3
   enddo
   ! KE = 1/2 Σ mv**2
   KE = KE * 0.5d0 * 0.25d0
end subroutine E_kinetic
