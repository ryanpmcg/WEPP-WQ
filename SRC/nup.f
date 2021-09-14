      subroutine nup(icrop,strsn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates plant nitrogen uptake

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_n1(:)   |none           |1st shape parameter for plant N uptake
!!                                |equation
!!    bio_n2(:)   |none           |2nd shape parameter for plant N uptake
!!                                |equation
!!    bn(1,:)     |kg N/kg biomass|nitrogen uptake parameter #1: normal fraction
!!                                |of N in crop biomass at emergence
!!    bn(3,:)     |kg N/kg biomass|nitrogen uptake parameter #3: normal fraction
!!                                |of N in crop biomass at maturity
!!    fixn        |kg N/ha        |amount of nitrogen added to soil via fixation
!!                                |on the day in HRU
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    idc(:)      |none           |crop/landcover category:
!!                                |1 warm season annual legume
!!                                |2 cold season annual legume
!!                                |3 perennial legume
!!                                |4 warm season annual
!!                                |5 cold season annual
!!                                |6 perennial
!!                                |7 trees
!!    ihru        |none           |HRU number
!!    ncr(:,:,:)  |none           |land cover code from crop.dat
!!    nn          |none           |number of soil layers in profile
!!    nro(:)      |none           |sequence number of year in rotation
!!    phuacc(:)   |none           |fraction of plant heat units accumulated
!!    snup(:)     |kg N/ha        |amount of nitrogen removed from soil by
!!                                |plant uptake
!!    sol_no3(:,:)|kg N/ha        |amount of nitrogen stored in the
!!                                |nitrate pool.
!!    sol_z(:,:)  |mm             |depth to bottom of soil layer
!!    ubn         |none           |nitrogen uptake distribution parameter
!!                                |This parameter controls the amount of
!!                                |nitrogen removed from the different soil 
!!                                |layers by the plant. In particular, this
!!                                |parameter allows the amount of nitrogen
!!                                |removed from the surface layer via plant
!!                                |uptake to be controlled. While the relation-
!!                                |ship between UBN and N removed from the
!!                                |surface layer is affected by the depth of the
!!                                |soil profile, in general, as UBN increases the
!!                                |amount of N removed from the surface layer
!!                                |relative to the amount removed from the entire
!!                                |profile increases
!!    uobn        |none           |nitrogen uptake normalization parameter
!!                                |This variable normalizes the nitrogen uptake
!!                                |so that the model can easily verify that
!!                                |upake from the different soil layers sums to
!!                                |1.0
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnb(:)      |none          |fraction of plant biomass that is nitrogen
!!    nplnt       |kg N/ha       |plant uptake of nitrogen in HRU for the day
!!    snup(:)     |kg N/ha       |amount of nitrogen removed from soil by
!!                               |plant uptake
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in the layer
!!    strsn       |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |nitrogen stress
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icrop       |none          |land cover code
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layer)
!!    un2         |
!!    unmx        |kg N/ha       |maximum amount of nitrogen that can be 
!!                               |removed from soil layer
!!    uno3l       |kg N/ha       |amount of nitrogen removed from soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nfix, nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, icrop, l
      real :: un2, unmx, uno3l
      real :: strsn   !!DSHI

      nplnt = 0    !!DSHI ALERT-for multiple OFE i.e. ihru>1 initialize before the loop
      j = 0
      j = ihru
    
!!DSHI
*      icrop = 0
*      icrop = ncr(nro(j),icr(j),j)
c        write(*,*) icrop,'NItrogen crop #'
       cnb(j) = (bn(1,icrop) - bn(3,icrop)) * (1. - phuacc(j) /          &
     &         (phuacc(j) + Exp(bio_n1(icrop) - bio_n2(icrop) *         &
     &         phuacc(j)))) + bn(3,icrop)
 
      un2 = 0.
      un2 = cnb(j) * bio_ms(j)
      if (un2 < snup(j)) un2 = snup(j)
      uno3d = un2 - snup(j)
      strsn = 1.

      do l = 1, nn
        unmx = 0.
        uno3l = 0.
        unmx = uno3d * (1. - Exp(-ubn * sol_z(l,j) / sol_z(nn,j)))      &
     &                                                            / uobn
        uno3l = Min(unmx - nplnt, sol_no3(l,j))
        nplnt = nplnt + uno3l 
        sol_no3(l,j) = sol_no3(l,j) - uno3l
      end do
 

      if (nplnt < 0.) nplnt = 0.
!!DSHI

cd	idc = 4
!! if crop is a legume, call nitrogen fixation routine
cd      select case (idc(ncr(nro(j),icr(j),j)))

      fixn = 0
c      write(*,*) idc(icrop),'why important',icrop,'crop#'
     

      select case (idc(icrop))
        case (1,2,3)
c         write(*,*) idc(icrop),'idc(icrop)***important'
          call nfix
      end select
  
      nplnt = nplnt + fixn
      snup(j) = snup(j) + nplnt

!! compute nitrogen stress
cd      select case (idc(ncr(nro(j),icr(j),j)))
      select case (idc(icrop))
        case (1,2,3)
          strsn = 1.
        case default
          call nuts(snup(j),un2,strsn)
      end select
     
      return
      end

