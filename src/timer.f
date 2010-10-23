! Copyright (c) 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, University of California Irvine
#include "definitions.fpp"
subroutine wallclock( wallc )
   implicit none
   _REAL_ wallc
   integer mycount, myrate

   call system_clock( COUNT=mycount, COUNT_RATE=myrate)
   wallc = dble(mycount)/dble(myrate)
   return
end subroutine wallclock
