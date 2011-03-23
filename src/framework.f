! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"

! This will be the main module that should be providing
! 1. public storage of variables
! 2. interface for callers
!
! Rule 1: Use one dimension array so that it would be easier to
!         do parallelisation.
module tienchun

   ! Acquired data
   integer             :: natom
   _REAL_, allocatable ::     x(:)
   _REAL_, allocatable ::  mass(:) 
   _REAL_, allocatable :: r_vdw(:)

   ! Acquired or Computed data
   _REAL_, allocatable ::     f(:)
   _REAL_, allocatable ::     v(:)
   _REAL_, allocatable ::  oldv(:)

   ! Computed system states
   ! RNDF_SOLUTE:   net (real) number of degrees of freedom for solute
   ! RNDF_SOLVENT:  net (real) number of degrees of freedom for solvent
   ! RNDF:        total (real) number of degrees of freedom

   ! User Control: Geometry
   logical             :: usePBC !(periodic boundary condition)
   logical             :: usePIB !(particle in a box)
   _REAL_              :: xmin, ymin, zmin
   _REAL_              :: xmax, ymax, zmax

   ! User Control: Force
   _REAL_              :: vdwcutoff
   _REAL_              :: eelcutoff

   ! User Control: Types of computation
   logical             :: doMD   !Molecular Dynamics
!  logical             :: doMIN  !Minimization
!  logical             :: doMC   !Monte Carlo

   ! User Control: MD and MC
   _REAL_              :: dt     !timestep, δt
   _REAL_              :: dt2    !2*δt
   _REAL_              :: dtsq   !δt**2
end module tienchun
! The origin of module name, TienChun, is a town in central Taiwan.
