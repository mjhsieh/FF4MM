! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
module physical_constants
   _REAL_, parameter ::        PI = 3.1415926535897932384626433832795d0
   _REAL_, parameter :: AVOGADRO  = 6.0221367d+23
   _REAL_, parameter :: J_PER_CAL = 4.184d0
   ! By the way, a Joule is kg(m**2)(s-2)
!  _REAL_, parameter :: DEGperRAD = PI / 180.d0
!  _REAL_, parameter :: RADperDEG = 180.d0 / PI

   _REAL_, parameter :: BOLTZMANN = 1.380658d-23 ! Joule/K
   ! Gravitation constant: 6.6742867d-11 ----> Joule/meter
   ! 6.6742867d-11 Joule/meter   --> 9.6064691d+9 Kcal/mol/meter
   ! 9.6064691d+9  Kcal/mol/meter =  9.6064691d-1 Kcal/mol/Å
   ! (amu^2)/(Å^2) = (2.75738904928253790280d-44kg^2)/m^2
   _REAL_, parameter ::     BIG_G = 2.6488772d-46 !Kcal/mol/Å
   ! Gravity: 9.81 m/s^2 = 9.81d10Å / s^2 = 9.81d-10Å / ps^2
   !        = 2.344607d-12Å
   _REAL_, parameter ::     LIT_G = 2.3446069d-12 !
  !_REAL_,parameter :: pressure_constant = 6.85695d+4
end module physical_constants

! from http://ambermd.org/Questions/units.html
! AMBER uses lengths  in angstrom
!            masses   in atomic mass units
!            energies in kcal/mol
!            time     in 20.455 pico second
module amber_constants
   _REAL_, parameter :: amber_qscale = 18.2223d0 ! used to convert amber charge
   _REAL_, parameter :: amber_tscale = 20.455d0  ! 1ps == 20.455d0 * amber ps
!  boltz2 = 8.31441d-3 * 0.5d0
!  pconv = 1.6604345d+04  ! factor to convert the pressure kcal/mole to bar
end module amber_constants
