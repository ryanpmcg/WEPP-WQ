      subroutine npup(icrop,strsp)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates plant phosphorus uptake

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none           |current year of simulation
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_p1(:)   |none           |1st shape parameter for plant P uptake
!!                                |equation
!!    bio_p2(:)   |none           |2st shape parameter for plant P uptake
!!                                |equation
!!    bp(1,:)     |kg P/kg biomass|phosphorus uptake parameter #1: normal
!!                                |fraction of P in crop biomass at emergence
!!    bp(3,:)     |kg P/kg biomass|phosphorus uptake parameter #3: normal
!!                                |fraction of P in crop biomass at maturity
!!    hru_fr(:)   |km**2/km**2    |fraction of watershed area in HRU
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    ihru        |none           |HRU number
!!    ncr(:,:,:)  |none           |land cover code from crop.dat
!!    nn          |none           |number of soil layers in profile
!!    nro(:)      |none           |sequence number of year in rotation
!!    nyskip      |none           |number of years to skip output summarization/
!!                                |printing
!!    phuacc(:)   |none           |fraction of plant heat units accumulated
!!    sol_labp(:,:)|kg P/ha       |amount of phosohorus stored in the labile P
!!                                |pool
!!    sol_z(:,:)  |mm             |depth to bottom of soil layer
!!    spup(:)     |kg P/ha        |amount of phosphorus removed from soil by
!!                                |plant uptake
!!    ubp         |none           |phosphorus uptake distribution parameter
!!                                |This parameter controls the amount of
!!                                |phosphorus removed from the different soil
!!                                |layers by the plant. In particular, this
!!                                |parameter allows the amount of phosphorus
!!                                |removed from the surface layer via plant
!!                                |uptake to be controlled. While the relation-
!!                                |ship between UBP and P uptake from the
!!                                |surface layer is affected by the depth of the
!!                                |soil profile, in general, as UBP increases
!!                                |the amount of P removed from the surface
!!                                |layer relative to the amount removed from the
!!                                |entire profile increases
!!    uobp        |none           |phosphorus uptake normalization parameter
!!                                |This variable normalizes the phosphorus
!!                                |uptake so that the model can easily verify
!!                                |that uptake from the different soil layers
!!                                |sums to 1.0
!!    wshd_pup    |kg P/ha        |average annual amount of plant uptake of 
!!                                |phosphorus 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cpt(:)      |none          |fraction of plant biomass that is phosphorus
!!    pplnt       |kg P/ha       |plant uptake of phosphorus in HRU for the day
!!    sol_labp(:,:)|kg P/ha      |amount of phosohorus stored in the labile P
!!                               |pool
!!    strsp       |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |phosphorus stress
!!    wshd_pup    |kg P/ha       |average annual amount of plant uptake of 
!!                               |phosphorus
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
     
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icrop       |none          |land cover code
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layers)
!!    uapd        |kg P/ha       |plant demand of phosphorus
!!    uapl        |kg P/ha       |amount of phosphorus removed from layer
!!    up2         |kg P/ha       |optimal plant phosphorus content
!!    upmx        |kg P/ha       |maximum amount of phosphorus that can be
!!                               |removed from the soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, icrop, l
      real :: up2, uapd, upmx, uapl
      real :: strsp   !DSHI

      pplnt = 0    !!DSHI ALERT-for multiple OFE i.e. ihru>1 initialize before the loop
	             !! include subroutine varinit from swat 
      j = 0
      j = ihru

d      icrop = 0
!!DSHI
*    icrop = ncr(nro(j),icr(j),j)
      cpt(j) = (bp(1,icrop) - bp(3,icrop)) * (1. - phuacc(j) /          &
     &     (phuacc(j) + Exp(bio_p1(icrop) - bio_p2(icrop) * phuacc(j))))&
     &     + bp(3,icrop)
      

c       cpt = 0.015
c      cpt(j) = 0.006
c       write(*,*) cpt(j), cpt(j),'cpt(j)'
c       write(*,*) phuacc(j),'phuacc(j)'

      up2 = 0.
      uapd = 0.
      up2 = cpt(j) * bio_ms(j)
c      write(*,*) up2,cpt(j),bio_ms(j),'P uptake'
 
      if (up2 < spup(j)) up2 = spup(j)
      uapd = up2 - spup(j)
      uapd = 1.5 * uapd                         !! luxury p uptake
      strsp = 1.
 
      do l = 1, nn
        upmx = 0.
        uapl = 0.
        upmx = uapd * (1. - Exp(-ubp * sol_z(l,j) / sol_z(nn,j)))       &
     &                                                            / uobp
        uapl = Min(upmx - pplnt, sol_labp(l,j))
        pplnt = pplnt + uapl
        sol_labp(l,j) = sol_labp(l,j) - uapl
      end do
      
      if (pplnt < 0.) pplnt = 0.
      spup(j) = spup(j) + pplnt
      
  !! compute phosphorus stress
      call nuts(spup(j), up2, strsp)
c      write(*,*)spup(j)
!!DSHI
!! summary calculations
      if (curyr > nyskip) then
        wshd_pup = wshd_pup + pplnt * hru_fr(j)
      end if

      return
      end
