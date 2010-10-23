! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
! just for 3d
module vec3d
   implicit none

contains

function vec3d_a2b(A,B)
   implicit none
   _REAL_, dimension(3)             :: vec3d_a2b
   _REAL_, dimension(3), intent(in) :: A
   _REAL_, dimension(3), intent(in) :: B
   ! Periodic Boundary processing goes here.
   ! Arithmatics
   vec3d_a2b=B-A
end function vec3d_a2b

function vec3d_lsq(vecAB)
   implicit none
   _REAL_                           :: vec3d_lsq
   _REAL_, dimension(3), intent(in) :: vecAB
   vec3d_lsq = dot_product(vecAB,vecAB)
end function vec3d_lsq

function vec3d_l(vecAB)
   implicit none
   _REAL_                           :: vec3d_l
   _REAL_, dimension(3), intent(in) :: vecAB
   vec3d_l = sqrt(vec3d_lsq(vecAB))
end function vec3d_l

! ======================================================================
!     MULTIPLY VECTORS A AND B AND RETURN _REAL_ RESULT
! ======================================================================
function vec3d_vxv(A,B)
   implicit none
   _REAL_, dimension (3)             :: vec3d_vxv
   _REAL_, dimension (3), intent(in) :: A
   _REAL_, dimension (3), intent(in) :: B
   vec3d_vxv(1)=A(2)*B(3)-A(3)*B(2)
   vec3d_vxv(2)=A(3)*B(1)-A(1)*B(3)
   vec3d_vxv(3)=A(1)*B(2)-A(2)*B(1)
end function vec3d_vxv

end module vec3d

#if 0
c ======================================================================
c     MULTIPLY TWO 3 X 3 MATRICES (OR 3 X 3 LEVELS OF HIGHER ORDER
c     MATRICES) AND PUT THE real*8 RESULT IN MATRIX C.
c     A * B = C
c ======================================================================
      subroutine mmul(A,MXA,LA,B,MXB,LB,C)
      integer I, J
      real*8 A(3,3,MXA), B(3,3,MXB), C(3,3), D(3,3)

      DO I=1,3
         DO J=1,3
            D(I,J)=A(I,1,LA)*B(1,J,LB) + A(I,2,LA)*B(2,J,LB)
     $           +A(I,3,LA)*B(3,J,LB)
         enddo
      enddo

      DO I=1,3
         DO J=1,3
            C(I,J)=D(I,J)
         enddo
      enddo

      RETURN
      END

c ======================================================================
c     MULTIPLY 3 X 3 MATRIX A BY 3 ELEMENT VECTOR V AND PUT THE REAL*8
c     RESULT IN VECTOR R.
c     A * V = R
c ======================================================================
      subroutine mvmul(A,V,R,N)
      integer I, N
c     N = FLAG DETERMINING ORDER OF MULTIPLICATION.  IF < 0, R EQUALS
c     VECTOR TIMES MATRIX; IF => 0, R EQUALS MATRIX X VECTOR.
      real*8 A(3, 3), V(3), R(3), T(3)

      IF (N .LT. 0) GO TO 3020

      DO I=1,3
         T(I)=A(I,1)*V(1)+A(I,2)*V(2)+A(I,3)*V(3)
      enddo
      GO TO 3030

 3020 DO I=1,3
         T(I)=V(1)*A(1,I)+V(2)*A(2,I)+V(3)*A(3,I)
      enddo

 3030 DO I=1,3
         R(I)=T(I)
      enddo

      RETURN
      END

#endif
