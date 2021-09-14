      subroutine hruallo(hru)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   This subroutine calculates the number of management operation types, etc.
!!   used in the simulation. These values are used to allocate array sizes for
!!   processes occurring in the HRU.

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ma          |none        |max number of applications
!!    mcr         |none        |max number of crops grown per year
!!    mcut        |none        |max number of cuttings per year
!!    mgr         |none        |max number of grazings per year
!!    ml          |none        |max number of soil layers
!!    mnr         |none        |max number of years of rotation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_af       |none        |number of autofertilizer operations in mgt file
!!    ap_ai       |none        |number of autoirrigation operations in mgt file
!!    ap_r        |none        |number of release/impound operations in mgt file
!!    ap_s        |none        |number of sweep operations in mgt file
!!    chemdat     |NA          |HRU soil chemical data
!!    eof         |none        |end of file flag (=-1 if eof, else =0)
!!    hkll        |none        |number of harvest/kill operations in mgt file
!!    hru         |none        |number of HRUs in subbasin
!!    hrudat      |NA          |name of HRU general data file
!!    ii          |none        |counter
!!    j           |none        |counter
!!    k           |none        |counter
!!    kll         |none        |number of kill operations in mgt file
!!    lyrtot      |none        |total number of layers in profile
!!    mgt_op      |none        |manangement operation code
!!    mgtdat      |NA          |HRU management data
!!    rot         |none        |number of years in rotation used in HRU
!!    soildat     |NA          |HRU soil data
!!    titldum     |NA          |title line of .sub file (not used)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: caps

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer, intent (in) :: hru
      character (len=13) :: mgtdat, chemdat
      character (len=80) :: titldum, titledum
      integer :: eof, j, k, rot, ap_f, ap_p, ap_af
      integer :: mgt_op,mgt6i, pstnum
      


      do j = 1, hru
        mgtdat = ""
        chemdat = ""
        read (22,5300) mgtdat, chemdat
c        write(*,*) mgtdat,chemdat,hru,'LILI needs to know.'
        open (133,file=mgtdat)   !!DSHI changed from 13 --> 133 to avoid conflict with WEPP
 
       !! calculate maximum number of years in a rotation
          rot = 0
          read (133,6000) titledum
          read (133,6200) rot
c          write(*,*) rot,'years management'
          mnr = Max(mnr,rot)
        
        !! calculate maximum number of crops grown in a year
          ap_f = 0
          ap_p = 0
          ap_af = 0

          do k = 1, rot
c            do
            read (133,6300) mgt_op
c            write (*,*)  mgt_op, mgt_op,'lili needs to know.'
            mgt6i = 0
            select case (mgt_op)
              case (0) !! end of year flag
                ap_f = 0
                ap_p = 0
                ap_af = 0
                exit
              case (3) !! fertilizer operation
                ap_f = ap_f + 1
              case (4) !! pesticide operation
                ap_p = ap_p + 1
                if (mgt6i > 0) pstflg(mgt6i) = 1
              case (11) !! autofert operation
                ap_af = ap_af + 1
            end select
                ma = Max(ma,ap_f,ap_p,ap_af)
                mafert = Max(mafert,ap_f)
                mapest = Max(mapest,ap_p)
c           end do                      
c            write(*,*) mafert,mapest,'LILI needs to know' 
               if (k == rot) exit    
       end do


         open (122,file=chemdat)   !!DSHI changed from 12 --> 122 to avoid conflict with WEPP
          eof = 0
          do 
            do k = 1, 33
              read (122,6000,iostat=eof) titldum
c              write (*,*) titldum,k
              if (eof < 0) exit
            end do
            if (eof < 0) exit
            do
              pstnum = 0
              read (122,*,iostat=eof) pstnum
              if (eof < 0) exit
              if (pstnum > 0) pstflg(pstnum) = 1
            end do
            if (eof < 0) exit
          end do
        close (122)   !!DSHI changed from 12 --> 122 to avoid conflict with WEPP
        close (133)   !!DSHI changed from 13 --> 133 to avoid conflict with WEPP
d        close (14)

      end do

      return
 5000 format (6a)
 5001 format (a1,9x,5i6)
 5002 format (a)
 5100 format (20a4)
 5200 format (10i4)
 5300 format (6a13)
 6000 format (a80)
 6100 format (27x,25f12.2)
 6200 format (i3)
 6300 format (24x,i4,45x,i4)
      end

