      subroutine orgn(iwave)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic nitrogen removed in
!!    surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha         |ha           |area of watershed in hectares
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgn(:)     |none         |organic N enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    hru_fr(:)     |none         |fraction of watershed area in HRU
!!    ihru          |none         |HRU number
!!    iwave         |none         |flag to differentiate calculation of HRU and
!!                                |subbasin sediment calculation
!!                                |iwave = 0 for HRU
!!                                |iwave = subbasin # for subbasin
!!    sedyld        |metric tons  |daily soil loss caused by water erosion in
!!                                |HRU
!!    sol_aorgn(:,:)|kg N/ha      |amount of nitrogen stored in the active
!!                                |organic (humic) nitrogen pool
!!    sol_bd(:,:)   |Mg/m**3      |bulk density of the soil
!!    sol_fon(:,:)  |kg N/ha      |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_orgn(:,:) |kg N/ha      |amount of nitrogen stored in the stable
!!                                |organic N pool
!!    sol_z(:,:)    |mm           |depth to bottom of soil layer
!!    sub_bd(:)     |Mg/m^3       |bulk density in subbasin first soil layer
!!    sub_fr(:)     |none         |fraction of watershed area in subbasin
!!    sub_orgn(:)   |kg N/ha      |amount of nitrogen stored in all organic
!!                                |pools 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedorgn       |kg N/ha      |amount of organic nitrogen in surface runoff
!!                                |in HRU for the day
!!    sol_aorgn(:,:)|kg N/ha      |amount of nitrogen stored in the active
!!                                |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha      |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_orgn(:,:) |kg N/ha      |amount of nitrogen stored in the stable
!!                                |organic N pool
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conc        |              |concentration of organic N in soil
!!    er          |none          |enrichment ratio
!!    j           |none          |HRU number
!!    wt1         |none          |conversion factor (mg/kg => kg/ha)
!!    xx          |kg N/ha       |amount of organic N in first soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: iwave
      integer :: j
      real :: xx, wt1, er, conc

      j = 0
      j = ihru

      xx = 0.
      if (iwave <= 0) then
        !! HRU calculations
        xx = sol_orgn(1,j) + sol_aorgn(1,j) + sol_fon(1,j)
      else
        !! subbasin calculations
cd        xx = sub_orgn(iwave)
      end if

      !! conversion factor
      wt1 = 0.
      if (iwave <= 0) then
        !! HRU calculations
        wt1 = sol_bd(1,j) * sol_z(1,j) / 100.
c        write(*,*) wt1,wt1,'wt1'
      else
        !! subbasin calculations
cd        wt1 = sub_bd(iwave) * sol_z(1,j) / 100.
            !! sol_z(1,j) equals 10 mm for all HRUS
      end if
      
      !! enrichment ratio
      er = 0.
      if (iwave <= 0) then
        !! HRU calculations
        if (erorgn(j) > .001) then
          er = erorgn(j)
        else
          er = enratio
        end if
      else
        !! subbasin calculations
cd        er = enratio
      end if

      conc = 0.
      conc = xx * er / wt1

      if (iwave <= 0) then
        !! HRU calculations
       if ( sedyld .gt.50) then
           sedyld = sedyld / 10000
        else 
           sedyld = sedyld
        end if
        sedorgn = .001 * conc * sedyld / (da_ha * hru_fr(j))
c        write(*,*) sedyld, sedorgn,conc,da_ha,'org_N'
        !! DSHI added to check error in MASS BALANCE
        if (sedorgn <= 1.e-6) sedorgn = 0.0
      else
        !! subbasin calculations
cd        sedorgn = .001 * conc * sedyld / (da_ha * sub_fr(iwave))
      end if
       
!! update soil nitrogen pools only for HRU calculations
      if (iwave <= 0 .and. xx > 1.e-6) then
       sol_aorgn(1,j) = sol_aorgn(1,j) - sedorgn * (sol_aorgn(1,j) / xx)
       sol_orgn(1,j) = sol_orgn(1,j) - sedorgn * (sol_orgn(1,j) / xx)
       sol_fon(1,j) = sol_fon(1,j) - sedorgn * (sol_fon(1,j) / xx)

cd       !! DSHI added to check error in MASS BALANCE
cd       ssub(35,j) = ssub(35,j) + sedorgn * (sol_aorgn(1,j) / xx) +      &
cd     &              sedorgn * (sol_orgn(1,j) / xx) +                    &
cd     &              sedorgn * (sol_fon(1,j) / xx)
       if (sol_aorgn(1,j) < 0.) then
cd         PAUSE 'sol_aorgn(1,j) < 0.'
         sedorgn = sedorgn + sol_aorgn(1,j)
         sol_aorgn(1,j) = 0.
       end if

       if (sol_orgn(1,j) < 0.) then
cd         PAUSE 'sol_orgn(1,j) < 0.'
         sedorgn = sedorgn + sol_orgn(1,j)
         sol_orgn(1,j) = 0.
       end if

       if (sol_fon(1,j) < 0.) then
cd         PAUSE 'sol_fon(1,j) < 0.'
         sedorgn = sedorgn + sol_fon(1,j)
         sol_fon(1,j) = 0.
       end if
      end if

      return
      end
