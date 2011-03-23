! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
! force a given gravity field
subroutine force_GF(atom, GF)
   implicit none
   _REAL_, dimension(3), intent(in) :: atom
   _REAL_, dimension(3), intent(in) :: GF
end subroutine force_GF

! force of a given magnetic field
subroutine force_BF(atom, BF)
   implicit none
   _REAL_, dimension(3), intent(in) :: atom
   _REAL_, dimension(3), intent(in) :: BF
end subroutine force_BF

! Pair-wise force

! Gravitation between two atoms
subroutine force_gravity(atom1,atom2)
   implicit none
   _REAL_, dimension(3), intent(in) :: atom1
   _REAL_, dimension(3), intent(in) :: atom2
end subroutine force_gravity

subroutine force_lj(atom1, atom2)
   implicit none
   _REAL_, dimension(3), intent(in) :: atom1
   _REAL_, dimension(3), intent(in) :: atom2
end subroutine force_lj

#if 0
!vectorized Lennard-Jones
subroutine vdfrclj
end subroutine vdfrclj
#endif
