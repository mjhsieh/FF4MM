! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
module tienchun
   integer natom
   _REAL_, allocatable ::     x(:)
   _REAL_, allocatable ::     f(:)
   _REAL_, allocatable ::     v(:)
   _REAL_, allocatable ::  oldv(:)
   _REAL_, allocatable ::  mass(:) 
   _REAL_, allocatable :: r_vdw(:)

   ! Program Control
   _REAL_              :: vdwcutoff
   _REAL_              :: eelcutoff
end module tienchun

subroutine force_lj(A,B)
   implicit none
   _REAL_, dimension(3), intent(in) :: A
   _REAL_, dimension(3), intent(in) :: B
end subroutine force_lj

#if 0
!vectorized Lennard-Jones
subroutine vdfrclj
end subroutine vdfrclj
#endif
