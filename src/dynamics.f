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
!      or v(t+½δt) = [r(t+δt)-t(t)]/δt
! 1. Specify positions r(0,i) and r(1,i)
! 2. Compute the foreces at time step n: 	f(n,i)
! 3. Compute the positions at time step n+1:	r(n+1,i)
!    as	r(n+1,i) = 2*r(n,i)-r(n-1,i)+f(n,i)*h^2/m
! 4. Comput the velocities at time step n,	v(n,i)
!    as	v(n  ,i) = (r(n+1,i)-r(n-1,i))/(2*h)
! n -> nstep
! h -> dt
subroutine verlet
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
end subroutine verlet
! The leap-frog algorithm [Hockney 1970]:
! r(t+δt)  = r(t) + δt*v(t+½δt)
! v(t+½δt) = v(t-½δt) + δt*a(t)
! it explicitly includes the velocity and also does not require the
! calculation of the differences of large numbers. However, it has
! the obvious disadvantage that the positions and velocities synchronised.
subroutine leapfrog
end subroutine leapfrog
! the velocity Verlet method [Swope et al. 1982]
! r(t+δt) = r(t) + δt*v(t) + ½δt*δt*a(t)
! v(t+δt) = v(t) + ½δt*[a(t) + a(t+δt)]
! Velocity form
! 1. Specify the initial position r(1,i)
! 2. Specify the initial velocity v(1,i)
! 3. Compute the positions at time step n+1:	r(n+1,i)
!    as	r(n+1,i) = r(n,i) + h*v(n,i) + h^2*f(n,i)/(2*m)
! 4. Compute the velocities at time step n:	v(n,i)
!    as	v(n+1,i) = v(n,i) + h*(f(n+1,i)+f(n,i))/(2*m)
subroutine veloverlet
end subroutine veloverlet
subroutine beeman
end subroutine beeman
