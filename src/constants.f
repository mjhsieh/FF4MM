! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
module physical_constants
   _REAL_, parameter ::        PI = 3.1415926535897932384626433832795d0
   _REAL_, parameter :: AVOGADRO  = 6.0221367d+23
   _REAL_, parameter :: J_PER_CAL = 4.184d0
  !_REAL_, parameter :: DEGperRAD = PI / 180.d0
  !_REAL_, parameter :: RADperDEG = 180.d0 / PI

   _REAL_, parameter :: BOLTZMANN = 1.380658d-23 ! Joule/K
end module physical_constants

! from http://ambermd.org/Questions/units.html
! AMBER uses lengths  in angstrom
!            masses   in atomic mass units
!            energies in kcal/mol
!            time     in 20.455 pico second
module amber_constants
   _REAL_, parameter :: amber_qscale = 18.2223d0 ! used to convert amber charge
   _REAL_, parameter :: amber_tscale = 20.455d0  ! 1ps == 20.455d0 * amber ps
end module amber_constants
