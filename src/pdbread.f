c Copyright (c) 1998, 2010, Mengjuei Hsieh,
c All rights reserved. - please read information in "LICENCSE.txt"
c Written by Mengjuei Hsieh, National Tsing Hua University, HsinChu, Taiwan
c         by Mengjuei Hsieh, University of California Irvine
      subroutine B_readpdb
      implicit none

      character B_line*72,B_keyword*6
      character B_atm_name*3,B_seq_name*3,B_chain_new*1,B_chain_old*1
      integer   res_new,res_old,res_atm
      integer   i,j,k
      integer nmc
      integer bgn_res,end_res,step,resi,resf
      logical debug
      real*8    coord(3)
      character*1 x_old,x_new
      data debug /.false./
      write(6,*) 'Enter the PDB file name'
      read(5,'(a)',end=3000,err=3000) pdb4c
      if (pdb4c.eq.'quit') then
         stop
      endif
      write(pdbfile,'(''/home/mjhsieh/pdb/pdb'',
     $     a4,''.ent'')') pdb4c

      write(6,'(''PDB file : '',a)') pdbfile
      open(PDB_IO,file=pdbfile,status='unknown',err = 1000)


      natom   = 0
      npro    = 0
      nres    = 0
      res_atm = 0
      mc      = 0
      nmc     = 0
      res_old = -1
      chain_old='?'
      x_old   = '?'
      mclist(0)  = 0
      reslist(0) = 0

cccccc First lets start to rewrite this into a F90 program
 100  continue

      read(PDB_IO,'(a)',end=200) line
      read(line,'(a6)') keyword

      if(keyword.eq.'HEADER'.or.keyword.eq.'COMPND'.or.
     $     keyword.eq.'COMPND'.or.keyword.eq.'SOURCE'.or.
     $     keyword.eq.'AUTHOR'.or.keyword.eq.'JRNL') then

         goto 100
      else if(keyword.eq.'REMARK') then
         
         goto 100
      else if(keyword.eq.'SEQRES') then

         goto 100
      else if(keyword.eq.'HET') then
         
         goto 100
      else if(keyword.eq.'FORMUL') then
         
         goto 100
      else if(keyword.eq.'HELIX') then
         read(line,'(21x,i4,8x,i4)') bgn_res,end_res
         do i = bgn_res,end_res
            hex0(i) = 'H'
         enddo
         goto 100
      else if(keyword.eq.'SHEET') then
         read(line,'(22x,i4,7x,i4)') bgn_res,end_res
         do i = bgn_res,end_res
            hex0(i) = 'B'
         enddo

         read(line,'(50x,i4,11x,i4)') bgn_res,end_res
         if(bgn_res.ge.end_res) then
            step = 1
         else
            step = -1
         endif
         do i = bgn_res,end_res,step
            hex0(i) = 'B'
         enddo

         goto 100
      else if(keyword.eq.'TURN') then
         
         goto 100
      else if(keyword.eq.'SSBOND') then
         
         goto 100
      else if(keyword.eq.'SITE') then
         
         goto 100
      else if(keyword.eq.'FTNOTE') then
         
         goto 100
      else if(keyword.eq.'TER') then
         goto 100
      else if(keyword.eq.'CONECT') then
         
         goto 100
      else if(keyword.eq.'MASTER') then
         
         goto 100
      else if(keyword.eq.'END') then
         
         goto 100
      else if(keyword.eq.'HETATM') then

         goto 100
      else if(keyword.eq.'ATOM') then

         read(line,
     $        '(13x,a3,1x,a3,2x,i4,a1,3x,3f8.3)',end=200,err=2000)
     $        atm_name,seq_name,res_new,x_new,
     $        coord(1),coord(2),coord(3)
         
         read(line,'(21x,a)') chain_new

         if(debug) then
            write(6,'(a)') line
         endif


         npro    = npro + 1
         if(chain_new.ne.chain_old) then
            write(6,'(/'' chain identifier '',a)') chain_new

            mc = mc + 1
            mclist(mc) = mclist(mc-1)+nmc
            mcname(mc) = chain_new
            chain_old  = chain_new
            nmc = 0
            sentinel(npro) = 1
         endif
         if(res_new.ne.res_old.or.x_new.ne.x_old) then
            nres = nres + 1
            nmc = nmc + 1
            reslist(nres) = reslist(nres-1)+res_atm
            res_name(nres) = seq_name
c            print *,'i =',nres,res_name(nres) !----MJHSIEH
            hex1(nres) = hex0(res_new)
            res_old = res_new
            x_old   = x_new
            res_atm = 0
         endif
         res_atm = res_atm + 1

         bkvcode(npro)   = atm_name
         x(3*(npro-1)+1) = coord(1)
         x(3*(npro-1)+2) = coord(2)
         x(3*(npro-1)+3) = coord(3)

         if     (atm_name.eq.'N  ') then
            N_atm(nres) = npro
         else if(atm_name.eq.'CA ') then
            CA_ATM(nres) = npro
         else if(atm_name.eq.'C  ') then
            C_atm(nres) = npro
         else if(atm_name.eq.'O  ') then
            O_atm(nres) = npro
c     CHI1
         else if(atm_name.eq.'CB ') then
            CB_atm(nres) = npro
         else if(atm_name.eq.'CG ') then
            CG_atm(nres) = npro
         else if(atm_name.eq.'SG ') then
            SG_atm(nres) = npro
         else if(atm_name.eq.'OG1') then
            OG1_atm(nres) = npro
         else if(atm_name.eq.'CG1') then
            CG1_atm(nres) = npro
         else if(atm_name.eq.'OG ') then
            OG_atm(nres) = npro
c     CHI2
         else if(atm_name.eq.'OD1') then
            OD1_atm(nres) = npro
         else if(atm_name.eq.'AD1') then
            AD1_atm(nres) = npro
         else if(atm_name.eq.'ND1') then
            ND1_atm(nres) = npro
         else if(atm_name.eq.'CD ') then
            CD_atm(nres) = npro
         else if(atm_name.eq.'CD1') then
            CD1_atm(nres) = npro
         else if(atm_name.eq.'SD ') then
            SD_atm(nres) = npro
c     CHI3
         else if(atm_name.eq.'OE1') then
            OE1_atm(nres) = npro
         else if(atm_name.eq.'AE1') then
            AE1_atm(nres) = npro
         else if(atm_name.eq.'CE ') then
            CE_atm(nres) = npro
         else if(atm_name.eq.'NE ') then
            NE_atm(nres) = npro
c     CHI4
         else if(atm_name.eq.'CZ ') then
            CZ_atm(nres) = npro
         else if(atm_name.eq.'NZ ') then
            NZ_atm(nres) = npro
c     OTHER
         else if(atm_name.eq.'CG2') then
            CG2_atm(nres) = npro
         else if(atm_name.eq.'CD2') then
            CD2_atm(nres) = npro
         else if(atm_name.eq.'CE1') then
            CE1_atm(nres) = npro
         else if(atm_name.eq.'CE2') then
            CE2_atm(nres) = npro
         else if(atm_name.eq.'CE3') then
            CE3_atm(nres) = npro
         else if(atm_name.eq.'CZ2') then
            CZ2_atm(nres) = npro
         else if(atm_name.eq.'CZ3') then
            CZ3_atm(nres) = npro
         else if(atm_name.eq.'CH2') then
            CH2_atm(nres) = npro
         else if(atm_name.eq.'AD2') then
            AD2_atm(nres) = npro
         else if(atm_name.eq.'AE2') then
            AE2_atm(nres) = npro
         else if(atm_name.eq.'NH1') then
            NH1_atm(nres) = npro
         else if(atm_name.eq.'NH2') then
            NH2_atm(nres) = npro
         else if(atm_name.eq.'ND2') then
            ND2_atm(nres) = npro
         else if(atm_name.eq.'NE1') then
            NE1_atm(nres) = npro
         else if(atm_name.eq.'NE2') then
            NE2_atm(nres) = npro
         else if(atm_name.eq.'OH ') then
            OH_atm(nres) = npro
         else if(atm_name.eq.'OE2') then
            OE2_atm(nres) = npro
         else if(atm_name.eq.'OD ') then
            OD_atm(nres) = npro
         else if(atm_name.eq.'OD2') then
            OD2_atm(nres) = npro
         else if(atm_name.eq.'OXT') then
            OXT_atm = npro
         endif

         goto 100
      else
         goto 100
      endif
 200  continue
      reslist(nres+1) = npro
      mclist(mc+1) = nres

c      do i=0,nres
c         print *,CA_ATM(i)      !mjhsieh
c      enddo

      write(6,'(/'' Total protein atoms '',i5)') npro
      write(6,'(/'' Total resideus      '',i5)') nres
c     write(6,'(10(i4,''-'',a3))') (i,res_name(i),i=1,nres)

c     write(6,'(/'' Residue list'')')
c     write(6,'(10i5)') (reslist(i),i=1,nres+1)

      write(6,'(/'' Main chain list'')')
      write(6,'(10i5)') (mclist(i),i=1,mc+1)

      close(PDB_IO)
      return

 1000 continue
      write(6,'(/'' PDB file is not found: '',a)') pdbfile
      close(PDB_IO)
      stop
 2000 continue
      write(6,'(/'' Error in reading PDB line: ''/a)') line
      close(PDB_IO)
      stop
 3000 continue
      stop
      end
