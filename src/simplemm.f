! Copyright (c) 1998, 2010, Mengjuei Hsieh,
! All rights reserved. - please read information in "LICENCSE.txt"
! Written by Mengjuei Hsieh, National Tsing Hua University, HsinChu, Taiwan
!         by Mengjuei Hsieh, University of California Irvine
program main
   implicit none
   integer      :: IArgC,ArgC!,i
   character*20 :: string1

   ! If ifc (v7) does not recognize GetArg, link it with -lPEPCF90
   ArgC = IArgC()            ! number of arguments
   call GetArg(0,string1)    ! the program name itself

   if (index(string1,'unittest') > 0) then
      call unittest
   else
!     do i=1,ArgC
!        call GetArg(i,string1)
!        write(6,*) string1
!     enddo
      write(6,*) "Usage: simplemm arg1 arg2 arg3 arg4"
   endif
end program
