! If ifc (ver 7) does not compile, use -lPEPCF90 in the compiler command.
       program main
       implicit none
       integer   :: IArgC,ArgC,i
       character :: ArgV(4)*20

       ArgC = IArgC()
       if (ArgC==4) then
          do i=1,ArgC
             call GetArg(i,ArgV(i))
             write(6,*) ArgV(i)
          enddo
       else
          write(6,*) "Usage: mytest arg1 arg2 arg3 arg4"
          stop
       endif
       end program
