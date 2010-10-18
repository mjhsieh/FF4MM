! Author: Mengjuei Hsieh, University of California Irvine.
#include "definitions.fpp"
module tienchun
   implicit none

contains

function vec3d_a2b(pos1,pos2)
   implicit none
   _REAL_             :: vec3d_a2b(3)
   _REAL_, intent(in) :: pos1(3), pos2(3)
   ! Periodic Boundary processing goes here.
   ! Arithmatics
   vec3d_a2b=pos2-pos1
end function vec3d_a2b

function vec3dlength(vecab)
   implicit none
   _REAL_             :: vec3dlength
   _REAL_, intent(in) :: vecab(3)
   vec3dlength=sqrt(dot_product(vecab,vecab))
end function vec3dlength

! just for 3d
function cross_product(r,s)
   implicit none
   _REAL_, dimension (3), intent(in) :: r,s
   _REAL_, dimension (3) :: cross_product
   ! local
   integer :: n,i,j

   do n = 1,3
      i = modulo(n,3) + 1
      j = modulo(i,3) + 1
      cross_product(n) = r(i)*s(j) - s(i)*r(j)
   end do
end function cross_product

end module tienchun

#if 0
c ======================================================================
c     MULTIPLY VECTORS A AND B AND PUT THE real*8 RESULT IN C
c ======================================================================
      subroutine vxv(A,B,C)
      real*8 A(3), B(3), C(3)
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      END

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

c ======================================================================
c     ADD OR SUBTRACT VECTORS A AND B AND PUT THE RESULT IN VECTOR C.
c     MP=1 FOR ADD; MP=-1 FOR SUBTRACT. N=THE NUMBER OF ELEMENTS IN
c     VECTOR A & B.
c ======================================================================
      subroutine vadd(A,B,C,MP,N)
      integer I, JOUT
      real*8 A(N),B(N),C(N)
      DATA JOUT/6/

      IF (MP.EQ.1 .OR. MP.EQ.-1) GO TO 2010
      WRITE(JOUT,2001)
 2001 FORMAT(' ERROR. OPERATOR NOT SPECIFIED.  RESULT EQUALS VECTOR A.')
      MP=0

 2010 DO I=1,N
         C(I)=A(I)+MP*B(I)
      enddo

      RETURN
      END
#endif
