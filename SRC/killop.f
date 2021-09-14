      subroutine killop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the kill operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    cnb(:)       |none          |fraction of plant biomass that is nitrogen
!!    cpt(:)       |none          |fraction of plant biomass that is phosphorus
!!    curyr        |none          |current year of simulation
!!    icr(:)       |none          |sequence number of crop grown within the
!!                                |current year
!!    ihru         |none          |HRU number
!!    ncrops(:,:,:)|
!!    nro(:)       |none          |sequence number for year in rotation
!!    nyskip       |none          |number of years to skip output printing/
!!                                |summarization
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool0
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alai(:)      |m**2/m**2     |leaf area index
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    idorm(:)     |none          |dormancy status code:
!!                                |0 land cover growing (not dormant)
!!                                |1 land cover dormant
!!    igro(:)      |none          |land cover status code:
!!                                |0 no land cover currently growing
!!                                |1 land cover growing
!!    ncrops(:,:,:)|
!!    phuacc(:)    |none          |fraction of plant heat units accumulated
!!    snup(:)      |kg N/ha       |amount of nitrogen removed from soil by
!!                                |plant uptake
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool0
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    spup(:)      |kg P/ha       |amount of phosphorus removed from soil by
!!                                |plant uptake
!!    strsw(:)     |none          |fraction of potential plant growth achieved
!!                                |on the day where the reduction is caused by
!!                                |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    resnew      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
  
      integer :: j
      real :: resnew

      j = 0
      j = ihru

      if (curyr > nyskip) then
        ncrops(nro(j),icr(j),j) = ncrops(nro(j),icr(j),j) + 1
      endif
!! update residue on soil surface
      resnew = 0.
      resnew = bio_ms(j)
      sol_rsd(1,j) = sol_rsd(1,j) + resnew
      sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
      sol_fon(1,j) = resnew * cnb(j) + sol_fon(1,j)
      sol_fop(1,j) = resnew * cpt(j) + sol_fop(1,j)
  
!! reset variables
cd      igro(j) = 0
cd      idorm(j) = 0
      bio_ms(j) = 0.
      snup(j) = 0.
      spup(j) = 0.
cd      strsw(j) = 1.
      alai(j) = 0.
      phuacc(j) = 0.
  
      return
      end
