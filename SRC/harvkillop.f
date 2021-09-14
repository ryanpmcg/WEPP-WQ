      subroutine harvkillop(icrop)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest and kill operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none           |fertilizer application efficiency calculated
!!                                |as the amount of N applied divided by the
!!                                |amount of N removed at harvest
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    cnb(:)      |none           |fraction of plant biomass that is nitrogen
!!    cnop(:,:,:) |none           |SCS runoff curve number for moisture
!!                                |condition II
!!    cnyld(:)    |kg N/kg yield  |fraction of nitrogen in yield
!!    cpyld(:)    |kg P/kg yield  |fraction of phosphorus in yield
!!    curyr       |none           |current year in simulation
!!    dm2(:,:,:)  |kg/ha          |harvested biomass (dry weight)
!!    dmanu(:)    |metric tons/ha |annual biomass (dry weight) in the HRU
!!    hia(:)      |
!!    hitar(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                                |planting
!!    hru_fr(:)   |km2/km2        |fraction of watershed in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    ihru        |none           |HRU number
!!    ncr(:,:,:)  |none           |land cover code from crop.dat
!!    ncrops(:,:,:)|
!!    nro(:)      |none           |sequence number for year in rotation
!!    nyskip      |none           |number of years to not summarize/print output
!!    rwt(:)      |none           |fraction of total plant biomass that is
!!                                |in roots
!!    snup(:)     |kg N/ha        |amount of nitrogen removed from soil by
!!                                |plant uptake
!!    sol_fon(:,:)|kg N/ha        |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha        |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool0
!!    sol_rsd(:,:)|kg/ha          |amount of organic matter in the soil
!!                                |classified as residue
!!    spup(:)     |kg P/ha        |amount of phosphorus removed from soil by
!!                                |plant uptake
!!    swh(:)      |mm H2O         |actual ET simulated during life of plant
!!    swp(:)      |mm H2O         |potential ET simulated during life of plant
!!    wshd_yldn   |kg N/ha        |amount of nitrogen removed from soil in
!!                                |watershed in the yield
!!    wshd_yldp   |kg P/ha        |amount of phosphorus removed from soil in
!!                                |watershed in the yield
!!    wsyf(:)     |(kg/ha)/(kg/ha)|Value of harvest index between 0 and HVSTI
!!                                |which represents the lowest value expected
!!                                |due to water stress
!!    yldanu(:)   |metric tons/ha |annual yield (dry weight) in the HRU
!!    yldkg(:,:,:)|kg/ha          |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alai(:)     |m**2/m**2     |leaf area index
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    dm2(:,:,:)  |kg/ha         |harvested biomass (dry weight)
!!    dmanu(:)    |metric tons/ha|annual biomass (dry weight) in the HRU
!!    hia(:)      |
!!    idorm(:)    |none          |dormancy status code:
!!                               |0 land cover growing (not dormant)
!!                               |1 land cover dormant
!!    igro(:)     |none          |land cover status code:
!!                               |0 no land cover currently growing
!!                               |1 land cover growing
!!    ncrops(:,:,:)|
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    snup(:)     |kg N/ha       |amount of nitrogen removed from soil by
!!                               |plant uptake
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool
!!    sol_rsd(:,:)|kg/ha         |amount of organic matter in the soil
!!                               |classified as residue
!!    spup(:)     |kg P/ha       |amount of phosphorus removed from soil by
!!                               |plant uptake
!!    strsw(:)    |none          |fraction of potential plant growth achieved
!!                               |on the day where the reduction is caused by
!!                               |water stress
!!    tnyld(:,:,:)|kg N/kg yield |modifier for autofertilization target
!!                               |nitrogen content for plant
!!    wshd_yldn   |kg N/ha       |amount of nitrogen removed from soil in
!!                               |watershed in the yield
!!    wshd_yldp   |kg P/ha       |amount of phosphorus removed from soil in
!!                               |watershed in the yield
!!    yldanu(:)   |metric tons/ha|annual yield (dry weight) in the HRU
!!    yldkg(:,:,:)|kg/ha         |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hiad1       |
!!    j           |none          |HRU number
!!    resnew      |
!!    wur         |
!!    yield       |kg            |yield (dry weight)
!!    yieldn      |
!!    yieldp      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
  
      integer :: j, icrop
      real :: hiad1, resnew, yield, yieldn, yieldp
      real :: strsn, strsp, strsw, strst,dryBioMass,HINDX,rootMass
      common /nutrient_stress/strsn, strsp, strsw, strst,dryBioMass,    &   !!DSHI
     &                         HINDX,rootMass      !!DSHI

      j = 0
      j = ihru

!DSHI assign harvest index from WEPP
      hiad1 = HINDX
c      write(*,*) HINDX,'I want to know this harvest index'
!! check if yield is from above or below ground
      yield = 0.
      resnew = 0.
      if (HINDX > 1.001) then
        yield = dryBioMass * (1. - 1. / (1. + hiad1))    !!(1. - 1. / (1. + hiad1)) is different from SWAT
        resnew = dryBioMass * (1. + hiad1)         !! 
      else
        yield = dryBioMass * hiad1
        resnew = (1. - hiad1) * dryBioMass
      endif


c      write(*,*) yield, resnew,'icrop'
      if (yield < 0.) yield = 0.
      if (resnew < 0.) resnew = 0.    
     
!! update residue on soil surface
      sol_rsd(1,j) = resnew + sol_rsd(1,j)
      sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
 
!! calculate nutrients removed with yield
      yieldn = 0.
      yieldp = 0.
      yieldn = yield * cnyld(icrop)
      yieldp = yield * cpyld(icrop)     
      yieldn = Min(yieldn, 0.8*snup(j)) !!! 0.8 is added according to the 2012 version SWAT code
      yieldp = Min(yieldp, 0.8*spup(j)) !!! 0.8 is added according to the 2012 version SWAT code
  
!! update fresh organic nutrient pools if needed
      if (snup(j) > yieldn) then
        sol_fon(1,j) = snup(j) - yieldn + sol_fon(1,j)
        fertn = fertn + snup(j) - yieldn  ! why this value is added here??
      endif
      
      if (spup(j) > yieldp) then
        sol_fop(1,j) = spup(j) - yieldp + sol_fop(1,j)
        fertp = fertp + spup(j) - yieldp    
      endif
c      write(*,*) sol_fon(1,j),sol_fop(1,j),'harvestN/P'

!! calculate modifier for autofertilization target nitrogen content
cd      tnyld(nro(j),icr(j),j) = 0.
cd      tnyld(nro(j),icr(j),j) = (1. - rwt(j)) * bio_ms(j) * cnb(j) *     &
cd     &                                                       auto_eff(j)

!! reset variables
cd      igro(j) = 0
cd      idorm(j) = 0
      bio_ms(j) = 0.
 
      snup(j) = 0.
      spup(j) = 0.
cd      strsw(j) = 1.
      alai(j) = 0.
cd      hia(j) = 0.
      phuacc(j) = 0.
     

      return
      end
 
