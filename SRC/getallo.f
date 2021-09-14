      subroutine getallo

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   This subroutine calculates the number of HRUs, subbasins, etc. in the 
!!   simulation. These values are used to allocate array sizes.

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mbb         |none        |maximum number of subbasins
!!    mch         |none        |maximum number of channels
!!    mcrdb       |none        |max number of lu/lc defined in crop.dat
!!    mfdb        |none        |max number of fertilizers in fert.dat
!!    mhru        |none        |maximum number of HRUs in watershed
!!    mhyd        |none        |maximum number of hydrograph nodes
!!    mnr         |none        |max number of years of rotation
!!    mpdb        |none        |max number of pesticides in pest.dat
!!    mr          |none        |maximum number of reservoirs
!!    mrecc       |none        |maximum number of reccnst files
!!    mrecd       |none        |maximum number of recday files
!!    mrecm       |none        |maximum number of recmon files
!!    mrecy       |none        |maximum number of recyear files
!!    mrg         |none        |max number of rainfall/temp gages
!!    mstep       |none        |max number of time steps per day
!!    mtil        |none        |max number of tillage types in till.dat
!!    mudb        |none        |maximum number of urban land types in urban.dat
!!    myr         |none        |max number of years of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    a           |NA          |comment flag
!!    basndat     |NA          |name of basin input file (.bsn)
!!    bigsub      |NA          |name of subbasin output file (.bsb)
!!    codedat     |NA          |name of input control code file (.cod)
!!    cropdb      |NA          |name of LU/LC database input file (crop.dat)
!!    eof         |none        |end of file flag
!!    event       |NA          |name of event output file (.eve)
!!    fertdat     |NA          |name of fertilizer database file (fert.dat)
!!    i           |none        |counter
!!    icd         |none        |routing command code (.fig)
!!    iht         |none        |hydrograph storage location number (.fig)
!!    inm1        |none        |1st routing command variable (.fig)
!!    inm2        |none        |2nd routing command variable (.fig)
!!    inm3        |none        |3rd routing command variable (.fig)
!!                             |if icd=1, inm3=subbasin #
!!    lwqout      |NA          |name of lake water quality output file (.lqo)
!!    nhtot       |none        |number of relative humidity records in file
!!    nrgage      |none        |number of raingage files
!!    nrgfil      |none        |number of rain gages per file
!!    nrtot       |none        |total number of rain gages
!!    nsave       |none        |number of save commands in .fig file
!!    nstot       |none        |number of solar radiation records in file
!!    ntgage      |none        |number of temperature gage files
!!    ntgfil      |none        |number of temperature gages per file
!!    nttot       |none        |total number of temperature gages
!!    numhru      |none        |number of HRUs listed in subbasin file
!!    nwtot       |none        |number of wind speed records in file
!!    pestidat    |NA          |name of pesticide database input file(pest.dat)
!!    pestout     |NA          |name of pesticide output file (.pso)
!!    rchout      |NA          |name of reach output file (.rch)
!!    routin      |NA          |name of watershed configuration file (.fig)
!!    rsvout      |NA          |name of reservoir output file (.rsv)
!!    sbsout      |NA          |name of HRU output file (.sbs)
!!    subdat      |NA          |name of subbasin input file (.sub)
!!    tilldat     |NA          |name of tillage database input file(till.dat)
!!    title       |NA          |description lines in file.cio(1st 3 lines)
!!    titldum     |NA          |variable to read in data line
!!    urbdat      |NA          |name of urban land type database file (urban.dat)
!!    watqal      |NA          |name of watershed water quality input file (.wwq)
!!    wtrout      |NA          |name of HRU impoundment output file (.wtr)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    caps
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm


      character (len=13) :: event, pestidat, fertdat
         

cd      character (len=1) ::  a
      character (len=80) ::  titldum
cd      integer :: nsave, icd, inm1, inm2, inm3, iht, eof, numhru
      integer :: eof, numhru

!!    initialize variables
      title = ""
cd      sbsout = ""
cd      rchout = ""
cd      rsvout = ""
cd      lwqout = ""
cd      pestout = ""
      event = ""
cd      codedat = ""
cd      basndat = ""
cd      cropdb = ""
cd      tilldat = ""
      pestidat = ""
      fertdat = ""
cd      urbdat = ""
cd      routin = ""
cd      bigsub = ""
cd      watqal = ""
cd      wtrout = ""
cd      nrgage = 0
cd      ntgage = 0
cd      nrtot = 0
cd      nttot = 0
cd      nrgfil = 0
cd      ntgfil = 0
cd      nstot = 0
cd      nhtot = 0
cd      nwtot = 0

      open (22,file="file.cio")   !!DSHI changed from 2-->22 conflict with WEPP
      read (22,5100) titldum
cd      read (2,5000) bigsub, sbsout, rchout, rsvout, lwqout, wtrout
cd      read (2,5000) pestout, event, routin, codedat, basndat, watqal
cd      read (2,5000) cropdb, tilldat, pestidat, fertdat, urbdat
      read (22,5000) pestidat, fertdat
cd      read (2,5200) nrgage, ntgage, nrtot, nttot, nrgfil, ntgfil, nstot,&
cd     &              nhtot, nwtot

      
cd      call caps(codedat)
cd      call caps(cropdb)
cdd      call caps(fertdat)
cdd      call caps(pestidat)
cd      call caps(routin)
cd      call caps(tilldat)
cd      call caps(urbdat)

!!    open .cod file
cd      open (10,file=codedat)

!!    open routing file
cd      open (9,file=routin)

!!    opens database files
cd      open (15,file=cropdb)
cd      open (16,file=tilldat)
      open (177,file=pestidat)   !!DSHI changed from 17-->177 conflict with WEPP
      open (111,file=fertdat)
cd      open (112,file=urbdat)

!!    initialize variables
cd      a = ""
cd      icd = 1
cd      iht = 0
cd      inm1 = 0
cd      inm2 = 0
cd      inm3 = 0
      mhru = 0
cd      mch = 1
      mbb = 1
cd      mhyd = 1
cd      mr = 0 
cd      ml = 0
      mp = 0
cd      mcr = 0
      ma = 0
      mafert = 0
      mapest = 0
cd      mgr = 0
cd      mcut = 0
      mnr = 0
cd      mrecc = 0
cd      mrecd = 0
cd      mrecm = 0
cd      mrecy = 0
cd      nsave = 0

cd      do while (icd > 0)
cd        read (9,5002) a
cd        if (a /= "*") then
cd          backspace 9

cd          read (9,5001) a, icd, iht, inm1, inm2, inm3
cd          read (9,5001) a, icd, inm1, inm2, inm3
cd
cd          select case (icd)
cd          case (1)                      !! icd = 1  SUBBASIN command
cd            mbb = mbb + 1               !! # subbasins
cd            mch = mch + 1               !! # channels
cd          case (3)                      !! icd = 3  ROUTE RESERVOIR command
cd            mr = mr + 1
cd            read (9,5002) a
cd          case (7)                      !! icd = 7  RECALL MONTH command
cd            read (9,5002) a
cd            mrecm = mrecm + 1
cd          case (8)                      !! icd = 8  RECALL YEAR command
cd            read (9,5002) a
cd            mrecy = mrecy + 1
cd          case (9)                      !! icd = 9  SAVE command
cd            nsave = nsave + 1
cd          case (10)                     !! icd = 10 RECALL DAY command
cd            read (9,5002) a
cd            mrecd = mrecd + 1
cd          case (11)                     !! icd = 11 RECALL CONSTANT command
cd            read (9,5002) a
cd            mrecc = mrecc + 1
cd          case (14)                     !! icd = 14 SAVECONC command
cd            read (9,5002) a
cd            nsave = nsave + 1
cd          end select

cd          mhyd = Max(mhyd,iht)

cd        end if
cd      end do

!! calculate number of records in plant growth database
cd      eof = 0
cd      mcrdb = 0
cd      do
cd        read (15,6000,iostat=eof) titldum
cd        if (eof < 0) exit
cd        mcrdb = mcrdb + 1
cd      end do
cd      mcrdb = NInt(Real(mcrdb) / 4.)
cd      if (mcrdb <= 0) mcrdb = 1

!! calculate number of records in urban database
cd      eof = 0
cd      mudb = 0
cd      do
cd        read (112,6000,iostat=eof) titldum
cd        if (eof < 0) exit
cd        mudb = mudb + 1
cd      end do
cd      mudb = NInt(Real(mudb) / 2.)
cd      if (mudb <= 0) mudb = 1

!! calculate number of records in fertilizer database
      eof = 0
      mfdb = 0
      read (111,*)
      read (111,*)
      read (111,*)
      read (111,*)
      do
        read (111,6000,iostat=eof) titldum
        if (eof < 0) exit
        mfdb = mfdb + 1
      end do
      if (mfdb <= 0) mfdb = 1

!! calculate number of records in pesticide database
      eof = 0
      mpdb = 0
      read (177,*)
      read (177,*)
      read (177,*)
      read (177,*)
      read (177,*)
      do
        read (177,6000,iostat=eof) titldum   !!DSHI changed from 17-->177 conflict with WEPP
        if (eof < 0) exit
        mpdb = mpdb + 1
      end do
      if (mpdb <= 0) mpdb = 1

!! calculate number of records in tillage database
cd      eof = 0
cd      mtil = 0
cd      do
cd        read (16,6000,iostat=eof) titldum
cd        if (eof < 0) exit
cd        mtil = mtil + 1
cd      end do
cd      if (mtil <= 0) mtil = 1

!! calculate total number of HRUs in watershed
      allocate (pstflg(mpdb))
      pstflg = 0
      eof = 0
cd      do i = 1, mbb-1
      do i = 1, 1 
cd        subdat = ""
        numhru = 0
cd        read (22,6100,iostat=eof) subdat
cd        if (eof < 0) exit
cd        call caps(subdat)
cd        if (subdat /= '             ') open (1,file=subdat)
        read (22,*) numhru
        read (22,6000) titldum
        mhru = mhru + numhru
        call hruallo(numhru)
cd        close (1)
cd        read (22,6000,iostat=eof) titldum
        if (eof < 0) exit
      end do
      if (mhru <= 0) mhru = 1
      if (mbb <= 0) mbb = 1
cd      if (mch <= 0) mch = 1
cd      if (mrecc <= 0) mrecc = 1
cd      if (mrecd <= 0) mrecd = 1
cd      if (mrecm <= 0) mrecm = 1
cd      if (mrecy <= 0) mrecy = 1

cd      mhyd = mhyd + nsave + 1
cd      ml = ml + 2 
cd      mcr = mcr + 1
cdd      ma = ma + 1    !! DSHI changed as initialized to an extra 1
cd      mgr = mgr + 1
cd      mcut = mcut + 1
cdd      mnr = mnr + 1    !! DSHI changed as initialized to an extra 1
cd      mp = Sum(pstflg) + 1
      mp = Sum(pstflg)    !! DSHI changed as mp was initialized to an extra 1

!! calculate max number of climate gages
cd      mrg = 0
cd      mrg = Max(nrtot,nttot,nstot,nhtot,nwtot)
cd      if (mrg <= 0) mrg = 1

!! calculate max number of years simulated, daily time increment
cd      mstep = 0
cd      myr = 0
cd      read (10,6000) titldum
cd      read (10,*) myr
cd      read (10,6000) titldum
cd      read (10,6000) titldum
cd      read (10,6000) titldum
cd      read (10,6000) titldum
cd      read (10,6000) titldum
cd      read (10,6000) titldum
cd      read (10,6000) titldum
cd      read (10,6000) titldum
cd      read (10,6000) titldum
cd      read (10,6000) titldum
cd      read (10,*) mstep
cd      if (mstep <= 0) then
cd        mstep = 1
cd      else
cd        mstep = 1440 / mstep
cd      end if
cd      mstep = mstep + 1


      close (22)
cd      close (9)
cd      close (10)
cd      close (15)
cd      close (16)
      close (177)
      close (111)
cd      close (112)
      return
 5000 format (6a)
 5001 format (a1,9x,5i6)
 5002 format(a)
 5100 format (20a4)
 5200 format (10i4)
 6000 format (a80)
 6100 format (6x, a13)
      end












