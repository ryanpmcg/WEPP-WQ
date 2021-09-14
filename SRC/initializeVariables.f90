
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables for the daily simulation of the
!!    land phase of the hydrologic cycle (the subbasin command loop)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    hhsubp(:,:) |mm H2O        |precipitation falling during hour in day in HRU
!!    ihru        |none          |HRU number
!!    nstep       |none          |number of lines of rainfall data for each day
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the day in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    sno_hru(:)  |mm H2O        |amount of water stored as snow
!!    sol_nly(:)  |none          |number of soil layers
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on any given day
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    al5         |none          |fraction of total rainfall that occurs during 0.5h of highest intensity rain
!!    alb         |none          |albedo for the day in HRU
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert application
!!    bactlchlp   |# colonies/ha |less persistent bacteria removed from soil surface layer by percolation
!!    bactlchp    |# colonies/ha |persistent bacteria removed from soil surface layer by percolation
!!    bactrolp    |# colonies/ha |less persistent bacteria transported to main channel with surface runoff
!!    bactrop     |# colonies/ha |persistent bacteria transported to main channel with surface runoff
!!    bactsedlp   |# colonies/ha |less persistent bacteria transported with sediment in surface runoff
!!    bactsedp    |# colonies/ha |persistent bacteria transported with sediment in surface runoff
!!    bsprev      |mm H2O        |surface runoff lagged from prior day of simulation
!!    bssprev     |mm H2O        |lateral flow lagged from prior day of simulation
!!    canev       |mm H2O        |amount of water evaporated from canopy storage
!!    cbodu       |mg/L          |carbonaceous biological oxygen demand of surface runoff on current day in HRU
!!    chl_a       |micrograms/L  |chlorophyll-a in surface runoff on current day in HRU
!!    cnday       |none          |curve number for current day in HRU
!!    crk         |mm H2O        |percolation due to crack flow
!!    deepstp     |mm H2O        |depth of water in deep aquifer in HRU
!!    dox         |mg/L          |dissolved oxygen concentration in the surface runoff on current day in HRU
!!    enratio     |none          |enrichment ratio calculated for day in HRU
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs on day in HRU
!!    ep_max      |mm H2O        |maximum amount of transpiration (plant et) that can occur on day in HRU
!!    es_day      |mm H2O        |actual amount of evaporation (from soil) that occurs on day in HRU
!!    etday       |mm H2O        |actual amount of evapotranspiration that occurs on day in HRU
!!    fertn       |kg N/ha       |total amount of nitrogen added to soil in HRU on day
!!    fertp       |kg P/ha       |total amount of phosphorus added to soil in HRU on day
!!    fixn        |kg N/ha       |amount of nitrogen added to soil via fixation on the day in HRU
!!    flat(:,:)   |mm H2O        |lateral flow storage array
!!    flow_cms    |m^3/s H2O     |rate of flow into main channel from HRU
!!    grazn       |kg N/ha       |amount of nitrogen added to soil in grazing on the day in HRU
!!    grazp       |kg P/ha       |amount of phosphorus added to soil in grazing on the day in HRU
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on current day
!!    hhprecip(:) |mm H2O        |precipitation falling during hour in day
!!    hhqday(:)   |mm H2O        |surface runoff from HRU for every hour in day
!!    hmntl       |kg N/ha       |amount of nitrogen moving from active organic to nitrate pool in soil profile on current day in HRU
!!    hmptl       |kg P/ha       |amount of phosphorus moving from the organic to labile pool in soil profile on current day in HRU
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates into soil (enters soil)
!!    latlyr      |mm H2O        |amount of water in lateral flow in layer in HRU for the day
!!    latno3      |kg N/ha       |amount of NO3-N in lateral flow in HRU for the day
!!    latq        |mm H2O        |amount of water in lateral flow in HRU for the day
!!    minpgw      |kg P/ha       |mineral P loading to reach in groundwater
!!    nn          |none          |number of soil layers in HRU
!!    no3gw       |kg N/ha       |nitrate loading to reach in groundwater
!!    no3pcp      |kg N/ha       |nitrate added to the soil in rainfall
!!    nplnt       |kg N/ha       |plant uptake of nitrogen in HRU for the day
!!    peakr       |m^3/s         |peak runoff rate for the day in HRU
!!    percn       |kg N/ha       |NO3-N leached from soil profile during the day
!!    pet_day     |mm H2O        |potential evapotranspiration for day in HRU
!!    pndev       |m^3 H2O       |evaporation from pond on day
!!    pndflwi     |m^3 H2O       |volume of water flowing into pond on day
!!    pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
!!    pndpcp      |m^3 H2O       |precipitation on pond during day
!!    pndsedc     |metric tons   |net change in sediment in pond during day
!!    pndsedin    |metric tons   |sediment inflow to the pond from HRU
!!    pndsedo     |metric tons   |sediment leaving pond during day
!!    pndsep      |m^3 H2O       |seepage from pond on day
!!    potevmm     |mm H2O        |volume of water evaporated from pothole expressed as depth over HRU
!!    potflwo     |mm H2O        |volume of water released to main channel from pothole exporessed as depth over HRU
!!    potpcpmm    |mm H2O        |precipitation falling on pothole water body expressed as depth over HRU
!!    potsedo     |t/ha          |sediment released to main channel from HRU
!!    potsepmm    |mm H2O        |seepage from pothole expressed as depth over HRU
!!    pplnt       |kg P/ha       |plant uptake of phosphorus in HRU for the day
!!    precip      |mm H2O        |precipitation for the day in HRU
!!    precipdt(:) |mm H2O        |precipitation for the time step during day
!!    pstsol(:)   |kg pst/ha     |soluble pesticide leached from bottom of soil profile
!!    qday        |mm H2O        |surface runoff loading to main channel for day in HRU
!!    qdfr        |none          |fraction of water yield that is surface runoff
!!    qdr         |mm H2O        |total amount of water entering main channel from HRU for the day
!!    qtile       |mm H2O        |drainage tile flow for day in HRU
!!    revapday    |mm H2O        |amount of water moving from the shallow aquifer into the soil profile or being taken up by plant roots in the shallow aquifer
!!    rmn2tl      |kg N/ha       |amount of nitrogen moving from the fresh organic (residue) to the nitrate(80%) and active organic(20%) pools in soil profile on current day in HRU
!!    rmp1tl      |kg P/ha       |amount of phosphorus moving from the labile mineral pool to the active mineral pool in the soil profile on the current day in the HRU
!!    rmptl       |kg P/ha       |amount of phosphorus moving from the fresh organic (residue) to the labile(80%) and organic(20%) pools in soil profile on current day in HRU
!!    roctl       |kg P/ha       |amount of phosphorus moving from the active mineral pool to the stable mineral pool in the soil profile on the current day in the HRU
!!    rwntl       |kg N/ha       |amount of nitrogen moving from active organic to stable organic pool in soil profile on current day in HRU
!!    sedminp     |kg P/ha       |amount of mineral phosphorus sorbed to sediment in surface runoff in HRU for day
!!    sedorgn     |kg N/ha       |amount of organic nitrogen in surface runoff in HRU for the day
!!    sedorgp     |kg P/ha       |amount of organic phosphorus in surface runoff in HRU for the day
!!    sedyld      |metric tons   |daily soil loss caused by water erosion
!!    sep         |mm H2O        |percolation from bottom of the soil layer on day in HRU
!!    sepbtm      |mm H2O        |seepage leaving the bottom of the soil profile on day in HRU
!!    shallstp    |mm H2O        |depth of water in shallow aquifer in HRU on previous day
!!    snoev       |mm H2O        |amount of water in snow lost through sublimation on current day in HRU
!!    snofall     |mm H2O        |amount of precipitation falling as freezing rain/snow on day in HRU
!!    snomlt      |mm H2O        |amount of water in snow melt for the day in HRU
!!    snoprev     |mm H2O        |amount of water stored as snow on previous day
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    soxy        |mg/L          |saturation dissolved oxygen concentration
!!    ssfp(:)     |kg pst/ha     |amount of pesticide in lateral flow in HRU for the day
!!    strsn       |none          |fraction of potential plant growth achieved on the day where the reduction is caused by nitrogen stress
!!    strsp       |none          |fraction of potential plant growth achieved on the day where the reduction is caused by phosphorus stress
!!    strstmp     |none          |fraction of potential plant growth achieved on the day in HRU where the reduction is caused by temperature stress
!!    strsw(:)    |none          |fraction of potential plant growth achieved on the day where the reduction is caused by water stress
!!    surfq       |mm H2O        |amount of water in surface runoff generated on day in HRU
!!    surqno3     |kg N/ha       |amount of NO3-N in surface runoff in HRU for the day
!!    surqsolp    |kg P/ha       |amount of soluble phosphorus in surface runoff in HRU for the day
!!    sw_excess   |mm H2O        |amount of water in soil that exceeds field capacity (gravity drained water)
!!    swprev      |mm H2O        |amount of water stored in soil profile in the HRU on the previous day
!!    tloss       |mm H2O        |amount of water removed from surface runoff via transmission losses on day in HRU
!!    twlpnd      |mm H2O        |water lost through seepage from ponds on day in HRU
!!    twlwet      |mm H2O        |water lost through seepage from wetlands on day in HRU
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    usle        |metric tons/ha|daily soil loss predicted with USLE equation
!!    usle_ei     |none          |USLE erodibility index on day for HRU
!!    vpd         |kPa           |vapor pressure deficit
!!    wdntl       |kg N/ha       |amount of nitrogen lost from nitrate pool by denitrification in soil profile on current day in HRU
!!    wetev       |m^3 H2O       |evaporation from wetland for day
!!    wetflwi     |m^3 H2O       |volume of water flowing in wetland on day
!!    wetflwo     |m^3 H2O       |volume of water flowing out wetland on day
!!    wetpcp      |m^3 H2O       |precipitation on wetland for day
!!    wetsedc     |metric tons   |net change in sediment in wetland during day
!!    wetsedi     |metric tons   |sediment loading to wetland for day
!!    wetsedo     |metric tons   |sediment loading from wetland for day
!!    wetsep      |m^3 H2O       |seepage from wetland bottom for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ly          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

SUBROUTINE initializeVariables

!! Load parameter module (modparm.f)
USE parm

!! Declare local variables
INTEGER :: j

!! Define local variables
j = 0
j = ihru

!! Initialize variables
alb = 0.
auton = 0.
autop = 0.
enratio = 0.
fertn = 0.
fertp = 0.
fixn = 0.
hmntl = 0.
hmptl = 0.
nn = 0
no3pcp = 0.
nplnt = 0.
percn = 0.
percp = 0.
pplnt = 0.
pstsol = 0.
rmn2tl = 0.
rmp1tl = 0.
rmptl = 0.
roctl = 0.
rwntl = 0.
sedminp = 0.
sedorgn = 0.
sedorgp = 0.
sedyld = 0.
pst_sed = 0.
surfq = 0.
surqno3 = 0.
surqsolp = 0.
pst_surq = 0.
pstsol = 0.
uno3d = 0.
wdntl = 0.

RETURN
END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                            !!
!!                                CODE ARCHIVE                                !!
!!                                                                            !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!al5 = 0.
!bactlchlp = 0.
!bactlchp = 0.
!bactrolp = 0.
!bactrop = 0.
!bactsedlp = 0.
!bactsedp = 0.
!bsprev = 0.
!bssprev = 0.
!canev = 0.
!cbodu = 0.
!chl_a = 0.
!cnday = 0.
!crk = 0.
!deepstp = 0.
!deepstp = deepst(j)
!dox = 0.
!ep_day = 0.
!ep_max = 0.
!es_day = 0.
!etday = 0.
!flow_cms = 0.
!grazn = 0.
!grazp = 0.
!gwseep = 0.
!hhprecip = 0.
!do ii = 1, 24
!    hhprecip(ii) = hhsubp(j,ii)
!enddo
!hhqday = 0.
!inflpcp = 0.
!latlyr = 0.
!latno3 = 0.
!latq = 0.
!minpgw = 0.
!nn = sol_nly(j)
!no3gw = 0.
!peakr = 0.
!pet_day = 0.
!pndev = 0.
!pndflwi = 0.
!pndflwo = 0.
!pndpcp = 0.
!pndsedc = 0.
!pndsedin = 0.
!pndsedo = 0.
!pndsep = 0.
!potevmm = 0.
!potflwo = 0.
!potpcpmm = 0.
!potsedo = 0.
!potsepmm = 0.
!precip = 0.
!precip = subp(j)
!precipdt = 0.
!if (nstep > 0) then
!    do ii = 1, nstep
!        precipdt(ii+1) = rainsub(j,ii)
!    end do
!end if
!qday = 0.
!qdfr = 0.
!qdr = 0.
!qtile = 0.
!revapday = 0.
!sep = 0.
!sepbtm = 0.
!shallstp = 0.
!shallstp = shallst(j)
!snoev = 0.
!snofall = 0.
!snomlt = 0.
!snoprev = 0.
!snoprev = sno_hru(j)
!soxy = 0.
!ssfp = 0.
!strsn = 1.
!strsp = 1.
!strstmp = 1.
!strsw(j) = 1.
!sw_excess = 0.
!swprev = 0.
!swprev = sol_sw(j)
!tloss = 0.
!twlpnd = 0.
!twlwet = 0.
!usle = 0.
!usle_ei = 0.
!vpd = 0.
!wetev = 0.
!wetflwi = 0.
!wetflwo = 0.
!wetpcp = 0.
!wetsedc = 0.
!wetsedi = 0.
!wetsedo = 0.
!wetsep = 0.
!do ly = 1, nn
!    flat(ly,j) = 0.
!    sol_prk(ly,j) = 0.
!enddo
