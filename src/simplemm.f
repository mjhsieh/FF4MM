! If ifc (ver 7) does not compile, use -lPEPCF90 in the compiler command.
       program main
       implicit none
       integer      :: IArgC,ArgC!,i
       character*20 :: string1

       ArgC = IArgC()            ! number of arguments
       call GetArg(0,string1)    ! the program name itself

       if (index(string1,'unittest') > 0) then
          call unittest
       else
!         do i=1,ArgC
!            call GetArg(i,string1)
!            write(6,*) string1
!         enddo
          write(6,*) "Usage: simplemm arg1 arg2 arg3 arg4"
       endif
       end program
