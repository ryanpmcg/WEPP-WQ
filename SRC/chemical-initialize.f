!!      include 'modparm.f'
      subroutine chemical_initialize(bddry,st,flux,ul,thetdr,dg,fc,
     &                    solthk,rmagt,smrm,mxnsl,mxplan,obmaxt,obmint,
     &                    tmin,tmax,tave,radmj,salb,month,soilw,orgmat,
     &                    LAI)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this is the program that read
!      integer :: ix
d      integer :: it, ifnum, eof, iwave, IFs input, gets array information from
!!    WEPP, initializes arrays and makes them equivalent

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none
      integer :: j,ICFLAG
      character (len=80) ::  titldum
      character (len=13) :: chemdat, mgtdat, pestidat, fertdat, cropdb    !! DSHI merged from openfile.f
      integer :: mxnsl,mxplan,month
      real bddry(mxnsl,mxplan),st(mxnsl,mxplan),flux(mxnsl,mxplan),
     &     ul(mxnsl),thetdr(mxnsl,mxplan),fc(mxnsl,mxplan),
     &     solthk(mxnsl,mxplan),dg(mxnsl,mxplan),rmagt(mxplan),
     &     smrm(3,mxplan),obmaxt(12),obmint(12),
     &     tmin,tmax,tave,radmj,salb,tmpsoil(mxplan),
     &     soilw(mxnsl,mxplan),orgmat(mxnsl,mxplan)
      real LAI, del_res, del_cov, clip
      common /chem_initialize/ ICFLAG
      common /residue/ del_res, del_cov, clip
      prog = "SWAT  Sept.'00 SUNSPARC VERSION2000"
!!    Initialize the initial system state, array allocation
      IF (ICFLAG .EQ. 0) THEN

!!      initialize variables
**      fix array size
        mcrdb = 118	!!    maximum number of crops/landcover in
!!                          database file (crop.dat)
        lubtot = 1
!!      Make arrays sizes equivalent to WEPP
	      ml = mxnsl
	      mbb = mxplan        !! number of OFEs
	      nn = mxnsl	  !!    number of soil layers in HRU

!!      process input- Only during the first call to subroutine
        call getallo   !! to clearify the # of HRU
        call allocate_parms   !! to set the arries used in the following process
        ndays = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)    !! DSHI merged from readinpt.f
        do j = 1,mxplan  !!max # of OFEs
          do i = 1, 12
    	      tmp_an(j) = tmp_an(j)+obmaxt(i)+obmint(i)
	        enddo
	        tmp_an(j) = tmp_an(j)/24.0
          tmpsoil(j) = (obmaxt(month)+obmint(month)) / 2.
          sol_nly(j) = mxnsl   !! max # of soil layers
        enddo

!!      DSHI merged from openfile.f
        cropdb = "crop2012.dat"
        fertdat = ""
        pestidat = ""
        open (22,file="file.cio")   !!DSHI changed from 2-->22 conflict with WEPP
        read (22,5100) titldum
        read (22,5000) pestidat, fertdat
d        call caps(fertdat)
d        call caps(pestidat)
        open (155,file=cropdb)
        open (177,file=pestidat)   !!DSHI changed from 17-->177 conflict with WEPP
        open (111,file=fertdat)
!!      DSHI merger end from openfile.f

        call initial         !! readin the soil property of each OFE
        do j = 1, mxplan
          sol_avbd(j) = 0.0   !! soil density
          do i = 1, mxnsl
            sol_ul(i,j) = ul(i)*1000.        !! soil water content
            sol_wpmm(i,j) = thetdr(i,j)*dg(i,j)*1000.   !! soil water centent wilt point
            sol_fc(i,j) = fc(i,j)*1000.                    !! filed capacity
            sol_sumfc(j) = sol_sumfc(j) + sol_fc(i,j)        !! water in soil profile
            sol_z(i,j) = solthk(i,j)*1000.                !! depth thickness
            sol_tmp(i,j) = tmpsoil(j)                          !! temperature
            sol_bd(i,j) = bddry(i,j)/1000.                     !!bulk density
            sol_avbd(j) = sol_avbd(j)+sol_bd(i,j)               !!average bulk density of soil profile
          enddo
          smrm(1,j) = 0
          sol_avbd(j) = sol_avbd(j)/mxnsl        !! bulk density
          sol_rsd(1,j) = smrm(1,j)*10000.0       !! organic matter
          sol_cov(j) = rmagt(j)*10000.0          !!residue
          del_res = 0.0
          del_cov = 0.0
        enddo

!!      DSHI merged from readinpt.f
        call readcrop              !! read in the landuse/landcover database
        call readpest              !! read in the pesticide database
        call readfert              !! read in the fertilizer/nutrient database

        do i = 1, lubtot   !!read in the mgt chem of each OFE
!!        DSHI merged from opensub.f & readsub.f             !! open input files for specific subbasin
          do
            read (22,*) hrutot(i)
!!          read HRU input data
            read (22,5100) titldum
              do j = 1, hrutot(i)
                ihru = 0
                ihru = nhru + j
                chemdat = ""
                mgtdat = ""
                read (22,5300) mgtdat, chemdat
d                call caps(mgtdat)
d                call caps(chemdat)
                open (133,file=chemdat)   !!DSHI changed from 13 --> 133 to avoid conflict with WEPP
                open (177,file=mgtdat)   !!DSHI changed from 17-->177 conflict with WEPP
                call readchm
                call readmgt
              end do
            exit
          end do
!!        assign subbasin values to HRUs where needed
          do j = 1, hrutot(i)
            ihru = 0
            ihru = nhru + j
            hru_sub(ihru) = i
          end do
!!        DSHI merger end from opensub.f & readsub.f
          nhru = nhru + hrutot(i)
        end do

d        if (irtpest > 0) irtpest = nope(irtpest)

        close (22)

        do i = 1, nhru
          do j = 1,mxnsl
            sol_cbn(j,i) = 100.0*orgmat(j,i)/1.73
          enddo
          call soil_chem           !! initialize soil chemical parameters
        end do
!!      DSHI merger end from readinpt.f

!!      turn off the flag for the 1st initialization
        ICFLAG = 1
        npest = 1
        nfert = 1
!!      DSHI open the file to write daily output for chemicals
        open (1111,file="dailyChemicals.txt")
        open (1112,file="dailyChemicalBalance.txt")
        open (1113,file="chemgraph.txt")
        open (1114,file="dailySurfaceChemicals.txt")
        close (1113)
        Ninitial = 0.0
        Pinitial = 0.0
        PsTinitial = 0.0
        call sumv
        Ninitial = soilN
        Pinitial = soilP
        PsTinitial = soilPsT

      ENDIF
      do i = 1, mxnsl
        sol_cbn(i,ihru) = 100.0*orgmat(i,ihru)/1.73
        sol_st(i,ihru) = st(i,ihru)*1000.
        sol_sw(ihru) = sol_sw(ihru)+soilw(i,ihru)
        sol_prk(i,ihru) = flux(i,ihru)*1000.              !!percolation from soil layer
        sol_por(i,ihru) = (2.65-sol_bd(i,ihru))/2.65       !!porosity from soil layer
c        write (*,*) mxnsl                                  !! added by Lili as the nl and nd are needed.
      enddo
      sol_avbd(ihru) = 0.0
      do i = 1, mxnsl
        sol_bd(i,ihru) = bddry(i,ihru)/1000.
        sol_avbd(ihru) = sol_avbd(ihru)+sol_bd(i,ihru)
      enddo
      sol_avbd(ihru) = sol_avbd(ihru)/mxnsl                     !! average bulk density of soil profile
      sol_rsd(1,ihru) = sol_rsd(1,ihru) + del_res               !! organic matter
      sol_cov(ihru) = sol_cov(ihru) + del_cov                   !! residue in soil layer
      sol_fon(1,ihru) = del_res * cnb(ihru) + sol_fon(1,ihru)   !! organic N stored in Fresh organic N pool
      sol_fop(1,ihru) = del_res * cpt(ihru) + sol_fop(1,ihru)   !! organic P stored in Fresh organic P pool
      del_res = 0.0
      del_cov = 0.0
      clip = 0.0
      alai(ihru) = LAI
      alb = salb
      tmn = tmin
      tmx = tmax
      tmpav = tave
      hru_ra = radmj*0.04184      !! DSHI langley-->MJ solar radiation for the day in HRU

      return
 5000 format (6a)
 5100 format (a80)
 5300 format (3a13)
      end


