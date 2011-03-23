! Copyright (c) 1998, 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, National Tsing Hua University, HsinChu, Taiwan
!         by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"

! Finite Difference Methods
!     .... are used to generate molecular dynamics trajectories with
!     continuous potential models, which we will assume to be pairwise
!     additive. The essential idea is that the integration is broken
!     down into many small stages, each separated in time by a fixed
!     time δt. The accelerations of the particles, are combined with
!     the posipositions and velocities at a time t to calculate the
!     positions and velocities at a time t + δt....
!     .... All algorithm assumethat the positions and dynamic
!     properties (velocities, accelerations, etc.) can be approximated
!     as Taylor series expansions.
!     r(t+δt) = r(t) + δt*v(t) + δt*δt*a(t)/2 + δt*δt*δt*b(t)/6 + ....
!     v(t+δt) = v(t) + δt*a(t) + δt*δt*b(t)/2 + δt*δt*δt*c(t)/6 + ....
!     a(t+δt) = a(t) + δt*b(t) + δt*δt*c(t)/2 + ....
!
! Verlet Algorithm [Verlet 1967]
!     r(t+δt) = r(t) + δt*v(t) + δt*δt*a(t)/2 + ....
!     r(t-δt) = r(t) - δt*v(t) + δt*δt*a(t)/2 + ....
!     ==> r(t+δt) = 2*r(t) - r(t-δt) + δt*δt*a(t)
!         v(t) = [r(t+δt) - r(t-δt)]/2/δt
!      or v(t+½δt) = [r(t+δt)-r(t)]/δt
subroutine verlet
! 1. Specify positions r(0,i) and r(1,i)
! 2. Compute the foreces at time step n: 	f(n,i)
! 3. Compute the positions at time step n+1:	r(n+1,i)
!    as	r(n+1,i) = 2*r(n,i)-r(n-1,i)+f(n,i)*h^2/m
! 4. Comput the velocities at time step n,	v(n,i)
!    as	v(n  ,i) = (r(n+1,i)-r(n-1,i))/(2*h)
! n -> nstep
! h -> dt
! dtsq is δt^2; dt2 = 2δt
!  implicit none
!  integer i, i3, j
!  _REAL_ sumvsq, sumv(3)
!  sumvsq = 0d0
!  sumv(1:3) = 0d0
!  do i = 1, natom
!     i3 = 3*(i-1)
!     do j = 1, 3
!        rnew(j) = 2*rx(i3+j)-rold(i3+j)+dtsq*a(i3+j)
!        v(i3+j) = (rnew(j)-rold(i3+j))/dt2
!        sumvsq  = sumvsq + v(i3+j)*v(i3+j)
!        sumv(j) = sumv(j) + v(i3+j)
!        rold(i3+j) = r(i3+j)
!        r(i3+j)    = rnew(j)
!     enddo
!  enddo
!--- From AMBER
!   ---Step 2:  Do the velocity update:
   !--- Newtonian dynamics:
   call veloupd_newton(v(istart),invmass(istart),f(istart), &
                       iend - istart + 1, amberdt )
!      --- consider vlimit
!   ---Step 3:  update the positions, putting the "old" positions into F:
end subroutine verlet

subroutine veloupd_newton(myv,myinvmass,myf,mynatom,mydt)
   implicit none
   integer mynatom
   _REAL_, dimension(mynatom), intent(inout) :: myv
   _REAL_, dimension(mynatom), intent(in)    :: myinvmass
   _REAL_, dimension(mynatom), intent(in)    :: myf
   _REAL_,                     intent(in)    :: mydt
   myv = myv + myf*myinvmass*mydt
end subroutine veloupd_newton
!
! Leap-frog Algorithm [Hockney 1970]:
!     It explicitly includes the velocity and also does not require 
!     the calculation of the differences of large numbers. However,
!     it has the obvious disadvantage that the positions and 
!     velocities are not synchronised:
!     r(t+δt)  = r(t) + δt*v(t+½δt)
!     v(t+½δt) = v(t-½δt) + δt*a(t)
!
! Velocity Verlet Method [Swope et al. 1982]
!     Gives positions, velocities and accelerations at the same
!     time and does not compromise precision:
!     r(t+δt) = r(t) + δt*v(t) + ½δt*δt*a(t)
!     v(t+δt) = v(t) + ½δt*[a(t) + a(t+δt)]
!     ->  v(t+½δt) = v(t)     + ½δt*a(t)
!         v(t+δt)  = v(t+½δt) + ½δt*a(t+δt)
!
! Beeman's Algorithm [Beeman 1976]
!     Often gives better energy conservation, because the kinetic
!     energy is calculated directed from the velocities. However the
!     expressions used are more complicated than those of the Verlet
!     algorithm and so it is computationally more expensive.
!     r(t+δt) = r(t) + δt*v(t) + (2/3)*δt*δt*a(t) - (1/6)*δt*δt*a(t-δt)
!     v(t+δt) = r(t) + (1/3)*δt*a(t) + (5/6)*δt*a(t) - (1/6)*δt*a(t-δt)
!    



subroutine leapfrog
end subroutine leapfrog
subroutine veloverlet
! 1. Specify the initial position r(1,i)
! 2. Specify the initial velocity v(1,i)
! 3. Compute the positions at time step n+1:	r(n+1,i)
!    as	r(n+1,i) = r(n,i) + h*v(n,i) + h^2*f(n,i)/(2*m)
! 4. Compute the velocities at time step n:	v(n,i)
!    as	v(n+1,i) = v(n,i) + h*(f(n+1,i)+f(n,i))/(2*m)
end subroutine veloverlet
subroutine beeman
end subroutine beeman
!What is NEB anyway?
subroutine atomdynamics
! Energy unit:	 kcal/mol
! Mass unit:	 amu
! Distance unit: angstrom
! ambertime = 20.455 ps

! ----- INITIALIZE SOME VARIABLES -----
!   Determine system degrees of freedom (for T scaling, reporting)
!   modify RNDFP to reflect NDFMIN (set in mdread)
! ----- MAKE A FIRST DYNAMICS STEP -----
!   case 1: starting without prior information
!     ----- CALCULATE THE FORCE -----
!   case 2: restarting from a restart file
!     Note for AMBER restart file
!       if the last printed energy from the previous trajectory was
!       at time "t", then the restrt file has velocities at time
!       t + 0.5dt, and coordinates at time t + dt
! ----- MAIN LOOP FOR PERFORMING THE DYNAMICS STEP -----
!   ---Step 1a: do some setup for pressure calculations:
!   ---Step 1b: Get the forces for the current coordinates:
!   ---Step 1c: do randomization of velocities, if needed:
    call verlet
!   ---Step 4a: if shake is being used, update the new positions to fix
!               the bond lengths.
!   ---Step 4b: Now fix the velocities and calculate KE
!   ---Step 4c: get the KE, either for averaging or for Berendsen:
!   ---Step 5:  several tasks related to dumping of trajectory information
!   ---Step 6:  zero COM velocity if requested; used for preventing
!               ewald "block of ice flying thru space" phenomenon, or
!               accumulation of rotational momentum in vacuum simulations
!   put current velocities into VOLD
!   ---Step 7:  scale coordinates if constant pressure run:
!   ---Step 8:  update the step counter and the integration time:
!   ---Step 9:  output from this step if required:
! major cycle back to new step unless we have reached our limit:
end subroutine atomdynamics
