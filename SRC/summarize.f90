!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |????-??        |Reza Savabi
!!    Modification      |2021-09        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + refactored, converted to free-source format
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine performs summary calculations
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          |units              |definition         |option
!!    -----------------------------------------------------------------------
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert application
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs on day in OFE
!!    es_day      |mm H2O        |actual amount of evaporation (from soil) that occurs on day in OFE
!!    etday       |mm H2O        |actual amount of evapotranspiration that occurs on day in OFE
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on current day
!!    minpgw      |kg P/ha       |soluble P loading to reach in groundwater
!!    no3gw       |kg N/ha       |nitrate loading to reach in groundwater
!!    no3pcp      |kg N/ha       |nitrate added to the soil in rainfall
!!    pet_day     |mm H2O        |potential evapotranspiration for day in OFE
!!    pndev       |m^3 H2O       |evaporation from pond on day
!!    pndflwi     |m^3 H2O       |volume of water flowing into pond on day
!!    pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
!!    pndpcp      |m^3 H2O       |precipitation on pond during day
!!    pndsedc     |metric tons   |net change in sediment in pond during day
!!    pndsedin    |metric tons   |sediment entering pond during day
!!    pndsedo     |metric tons   |sediment leaving pond during day
!!    pndsep      |m^3 H2O       |seepage from pond on day
!!    potevmm     |mm H2O        |volume of water evaporated from pothole expressed as depth over OFE
!!    potflwi(:)  |m^3 H2O       |water entering pothole on day
!!    potflwo     |mm H2O        |discharge from pothole expressed as depth over OFE
!!    potpcpmm    |mm H2O        |precipitation falling on pothole water body expressed as depth over OFE
!!    potsedi(:)  |metric tons   |sediment entering pothole on day
!!    potsedo     |metric tons   |sediment leaving pothole on day
!!    potsepmm    |mm H2O        |seepage from pothole expressed as depth over OFE
!!    qday        |mm H2O        |surface runoff loading to main channel for day in OFE
!!    qdr         |mm H2O        |total amount of water entering main channel for day from OFE
!!    qtile       |mm H2O        |drainage tile flow for day in OFE
!!    rchrg(:)    |mm H2O        |amount of water recharging both aquifers on current day in OFE
!!    revapday    |mm H2O        |amount of water moving from the shallow aquifer into the soil profile or being taken up by plant roots in the shallow aquifer
!!    LabToActMinP      |kg P/ha       |amount of phosphorus moving from the labile mineral pool to the active mineral pool in the soil profile on the current day in the OFE
!!    ActMinToStaMinP       |kg P/ha       |amount of phosphorus moving from the active mineral pool to the stable mineral pool in the soil profile on the current day in the OFE
!!    sepbtm      |mm H2O        |seepage leaving the bottom of the soil profile on day in OFE
!!    tloss       |mm H2O        |amount of water removed from surface runoff via transmission losses on day in OFE
!!    usle        |metric tons   |daily soil loss predicted with USLE equation
!!    ssb(1)      |mm H2O        |average amount of precipitation in watershed for the day
!!    ssb(3)      |mm H2O        |surface runoff in watershed for day
!!    ssb(4)      |mm H2O        |lateral flow contribution to streamflow in watershed for day
!!    ssb(5)      |mm H2O        |water percolation past bottom of soil profile in watershed for day
!!    ssb(6)      |mm H2O        |water yield to streamflow from HRUs in watershed for day
!!    ssb(7)      |mm H2O        |actual evapotranspiration in watershed for day
!!    ssb(8)      |deg C         |average maximum temperature in watershed for the day
!!    ssb(9)      |deg C         |average minimum temperature in watershed for the day
!!    ssb(12)     |metric tons   |sediment yield from HRUs in watershed for day
!!    ssb(13)     |metric tons   |sediment loading to ponds in watershed for day
!!    ssb(14)     |metric tons   |sediment loading from ponds in watershed for day
!!    ssb(15)     |metric tons   |net change in sediment level in ponds in watershed for day
!!    ssb(16)     |metric tons   |sediment loading to wetlands for day in watershed
!!    ssb(17)     |metric tons   |sediment loading to main channels from wetlands for day in watershed
!!    ssb(18)     |metric tons   |net change in sediment in wetlands for day in watershed
!!    ssb(19)     |m^3 H2O       |evaporation from ponds in watershed for day
!!    ssb(20)     |m^3 H2O       |seepage from ponds in watershed for day
!!    ssb(21)     |m^3 H2O       |precipitation on ponds in watershed for day
!!    ssb(22)     |m^3 H2O       |volume of water entering ponds in watershed for day
!!    ssb(23)     |m^3 H2O       |volume of water leaving ponds in watershed for day
!!    ssb(24)     |m^3 H2O       |evaporation from wetlands for day in watershed
!!    ssb(25)     |m^3 H2O       |seepage from wetlands for day in watershed
!!    ssb(26)     |m^3 H2O       |precipitation on wetlands for day in watershed
!!    ssb(27)     |m^3 H2O       |volume of water entering wetlands on day in watershed
!!    ssb(28)     |m^3 H2O       |volume of water leaving wetlands on day in watershed
!!    ssb(33)     |m^3 H2O       |net change in water volume of ponds in watershed for day
!!    ssb(35)     |mm H2O        |amount of water stored in soil profile in watershed for day
!!    ssb(36)     |mm H2O        |snow melt in watershed for day
!!    ssb(37)     |mm H2O        |sublimation in watershed for day
!!    ssb(38)     |mm H2O        |average amount of tributary channel transmission losses in watershed on day
!!    ssb(39)     |mm H2O        |freezing rain/snow fall in watershed for day
!!    ssb(40)     |kg N/ha       |organic N loading to stream in watershed for day
!!    ssb(41)     |kg P/ha       |organic P loading to stream in watershed for day
!!    ssb(42)     |kg N/ha       |nitrate loading to stream in surface runoff in watershed for day
!!    ssb(43)     |kg P/ha       |soluble P loading to stream in watershed for day
!!    ssb(44)     |kg N/ha       |plant uptake of N in watershed for day
!!    ssb(45)     |kg N/ha       |nitrate loading to stream in lateral flow in watershed for day
!!    ssb(46)     |kg N/ha       |nitrate percolation past bottom of soil profile in watershed for day
!!    ssb(47-56)  |mg pst/ha     |amount of pesticide type in surface runoff contribution to stream in watershed on day (in solution)
!!    ssb(74-83)  |mg pst/ha     |amount of pesticide type in surface runoff contribution to stream in watershed on day (sorbed to sediment)
!!    ssb(84-93)  |kg pst/ha     |amount of pesticide type leached from soil profile in watershed on day
!!    ssb(94-103) |kg pst/ha     |amount of pesticide type in lateral flow contribution to stream in watershed on day
!!    ssb(104)    |mm H2O        |groundwater contribution to stream in watershed on day
!!    ssb(105)    |mm H2O        |amount of water moving from shallow aquifer to plants/soil profile in watershed on day
!!    ssb(106)    |mm H2O        |deep aquifer recharge in watershed on day
!!    ssb(107)    |mm H2O        |total amount of water entering both aquifers in watershed on day
!!    ssb(108)    |mm H2O        |potential evapotranspiration in watershed on day
!!    ssb(109)    |mm H2O        |drainage tile flow contribution to stream in watershed on day
!!    ssub(1,:)   |mm H2O        |precipitation in OFE during month
!!    ssub(2,:)   |mm H2O        |amount of precipitation falling as freezing rain/snow in OFE during month
!!    ssub(3,:)   |mm H2O        |amount of snow melt in OFE during month
!!    ssub(4,:)   |mm H2O        |amount of surface runoff to main channel from OFE during month (ignores impact of transmission losses)
!!    ssub(5,:)   |mm H2O        |amount of lateral flow contribution to main channel from OFE during month
!!    ssub(6,:)   |mm H2O        |amount of groundwater flow contribution to main channel from OFE during month
!!    ssub(7,:)   |mm H2O        |amount of water moving from shallow aquifer to plants or soil profile in OFE during month
!!    ssub(8,:)   |mm H2O        |amount of water recharging deep aquifer in OFE during month
!!    ssub(9,:)   |mm H2O        |total amount of water entering both aquifers from OFE during month
!!    ssub(10,:)  |mm H2O        |water yield (total amount of water entering main channel) from OFE during month
!!    ssub(11,:)  |mm H2O        |amount of water percolating out of the soil profile and into the vadose zone in OFE during month
!!    ssub(12,:)  |mm H2O        |actual evapotranspiration in OFE during month
!!    ssub(13,:)  |mm H2O        |amount of transmission losses from tributary channels in OFE for month
!!    ssub(14,:)  |metric tons/ha|sediment yield from OFE for month
!!    ssub(15,:)  |mm H2O        |actual amount of transpiration that occurs during month in OFE
!!    ssub(16,:)  |mm H2O        |actual amount of evaporation (from soil) that occurs during month in OFE
!!    ssub(23,:)  |mm H2O        |amount of water removed from shallow aquifer in OFE for irrigation during month
!!    ssub(24,:)  |mm H2O        |amount of water removed from deep aquifer in OFE for irrigation during month
!!    ssub(25,:)  |mm H2O        |potential evapotranspiration in OFE during month
!!    ssub(26,:)  |kg N/ha       |monthly amount of N (organic & mineral) applied in OFE during grazing
!!    ssub(27,:)  |kg P/ha       |monthly amount of P (organic & mineral) applied in OFE during grazing
!!    ssub(28,:)  |kg N/ha       |monthly amount of N (organic & mineral) auto-applied in OFE
!!    ssub(29,:)  |kg P/ha       |monthly amount of P (organic & mineral) auto-applied in OFE
!!    ssub(31,:)  |stress days   |water stress days in OFE during month
!!    ssub(32,:)  |stress days   |temperature stress days in OFE during month
!!    ssub(33,:)  |stress days   |nitrogen stress days in OFE during month
!!    ssub(34,:)  |stress days   |phosphorus stress days in OFE during month
!!    ssub(35,:)  |kg N/ha       |organic nitrogen in surface runoff in OFE during month
!!    ssub(36,:)  |kg P/ha       |organic phosphorus in surface runoff in OFE during month
!!    ssub(37,:)  |kg N/ha       |nitrate in surface runoff in OFE during month
!!    ssub(38,:)  |kg N/ha       |nitrate in lateral flow in OFE during month
!!    ssub(39,:)  |kg P/ha       |soluble phosphorus in surface runoff in OFE during month
!!    ssub(40,:)  |kg N/ha       |amount of nitrogen removed from soil by plant uptake in OFE during month
!!    ssub(41,:)  |kg N/ha       |nitrate percolating past bottom of soil profile in OFE during month
!!    ssub(42,:)  |kg P/ha       |amount of phosphorus removed from soil by plant uptake in OFE during month
!!    ssub(43,:)  |kg P/ha       |amount of phosphorus moving from labile mineral to active mineral pool in OFE during month
!!    ssub(44,:)  |kg P/ha       |amount of phosphorus moving from active mineral to stable mineral pool in OFE during month
!!    ssub(45,:)  |kg N/ha       |amount of nitrogen applied to OFE in fertilizer and grazing operations during month
!!    ssub(46,:)  |kg P/ha       |amount of phosphorus applied to OFE in fertilizer and grazing operations during month
!!    ssub(47,:)  |kg N/ha       |amount of nitrogen added to soil by fixation in OFE during month
!!    ssub(48,:)  |kg N/ha       |amount of nitrogen lost by denitrification in OFE during month
!!    ssub(49,:)  |kg N/ha       |amount of nitrogen moving from active organic to nitrate pool in OFE during month
!!    ssub(50,:)  |kg N/ha       |amount of nitrogen moving from active organic to stable organic pool in OFE during month
!!    ssub(51,:)  |kg P/ha       |amount of phosphorus moving from organic to labile mineral pool in OFE during month
!!    ssub(52,:)  |kg N/ha       |amount of nitrogen moving from fresh organic to nitrate and active organic pools in OFE during month
!!    ssub(53,:)  |kg P/ha       |amount of phosphorus moving from fresh organic to the labile mineral and organic pools in OFE during month
!!    ssub(54,:)  |kg N/ha       |amount of nitrogen added to soil in rain
!!    ssub(61,:)  |metric tons/ha|daily soil loss predicted with USLE equation
!!    ssub(62,:)  |mm H2O        |drainage tile flow contribution to main channel from OFE in month
!!    ssub(65,:)  |kg N/ha       |nitrate loading from groundwater in OFE to main channel during month
!!    ssub(66,:)  |kg P/ha       |soluble P loading from groundwater in OFE to main channel during month
!!    ssub(67,:)  |kg P/ha       |loading of mineral P attached to sediment in OFE to main channel during month
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    hru_ha      |ha            |area of OFE in hectares
!!    k46         |none          |ip + 46
!!    k73         |none          |ip + 73
!!    k83         |none          |ip + 83
!!    k93         |none          |ip + 93
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE summarize

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: ip, k46, k73, k83, k93
    REAL :: hru_ha

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Summarizing Chemical Results"

    !! Define local variables
    soilN = 0.0
    Nloss = 0.0
    Ngain = 0.0
    Nbalance = 0.0
    soilP = 0.0
    Ploss = 0.0
    Pgain = 0.0
    Pbalance = 0.0
    soilPsT = 0.0
    PsTloss = 0.0
    PsTgain = 0.0
    PsTbalance = 0.0
    k46 = 0
    k73 = 0
    k83 = 0
    k93 = 0
    hru_ha = 0.
    hru_ha = areaOFE(iterOFE) * hru_fr(iterOFE)

    !! COMMENT NEEDED
	ssub(28,iterOFE) = ssub(28,iterOFE) + auton
	ssub(29,iterOFE) = ssub(29,iterOFE) + autop
	ssub(35,iterOFE) = ssub(35,iterOFE) + sedimentOrgN(iterOFE)
	ssub(37,iterOFE) = ssub(37,iterOFE) + runoffNO3(iterOFE)
	ssub(39,iterOFE) = ssub(39,iterOFE) + runoffLabileP(iterOFE)
	ssub(40,iterOFE) = ssub(40,iterOFE) + sum(uptakeNO3(1:numsl,iterOFE))
	ssub(41,iterOFE) = ssub(41,iterOFE) + verticalNO3(numsl,iterOFE)
	ssub(42,iterOFE) = ssub(42,iterOFE) + sum(uptakeLabileP(1:numsl,iterOFE))
	ssub(43,iterOFE) = ssub(43,iterOFE) + LabToActMinP
	ssub(44,iterOFE) = ssub(44,iterOFE) + ActMinToStaMinP
	ssub(45,iterOFE) = ssub(45,iterOFE) + appliedN(iterOFE)
	ssub(46,iterOFE) = ssub(46,iterOFE) + appliedP(iterOFE)
	ssub(47,iterOFE) = ssub(47,iterOFE) + nFixation(iofe)
	ssub(48,iterOFE) = ssub(48,iterOFE) + sum(denitrification(1:numsl,iterOFE))
	ssub(49,iterOFE) = ssub(49,iterOFE) !sol_aorgn to sol_no3
	ssub(50,iterOFE) = ssub(50,iterOFE) !sol_aorgn to sol_orgn
	ssub(51,iterOFE) = ssub(51,iterOFE) !soilActOrgP to sol_labp
	ssub(54,iterOFE) = ssub(54,iterOFE) + no3pcp

	!! watershed summations
	ssb(12) = ssb(12) + sedimentYieldOFE(iterOFE)
	ssb(35) = ssb(35) + profileWater(iterOFE) * hru_fr(iterOFE)
	ssb(40) = ssb(40) + sedimentOrgN(iterOFE) * hru_fr(iterOFE)
	ssb(41) = ssb(41) + sedorgp * hru_fr(iterOFE)
	ssb(42) = ssb(42) + runoffNO3(iterOFE) * hru_fr(iterOFE)
	ssb(43) = ssb(43) + runoffLabileP(iterOFE) * hru_fr(iterOFE)
	ssb(44) = ssb(44) + nplnt * hru_fr(iterOFE)
	ssb(46) = ssb(46) + verticalNO3(numsl,iterOFE) * hru_fr(iterOFE)

	!! COMMENT NEEDED
	IF (numPest > 0) THEN
		DO ip = 1,numPest
			k46 = ip + 46
			k73 = ip + 73
			k83 = ip + 83
			ssb(k46) = ssb(k46) + runoffPesticide(iterOFE,ip)
			ssb(k73) = ssb(k73) + sedimentPesticide(ip,iterOFE)
			ssb(k83) = ssb(k83) + verticalPesticide(numsl,iterOFE,ip)
			sda(iterOFE,ip,1) = runoffPesticide(iterOFE,ip)*hru_ha*1.0e6
			sda(iterOFE,ip,2) = sedimentPesticide(ip,iterOFE)*hru_ha*1.0e6
			sda(iterOFE,ip,3) = verticalPesticide(numsl,iterOFE,ip)*hru_ha*1.0e6
			sda(iterOFE,ip,4) = sum(lateralPesticide(1:10,iterOFE,ip))*hru_ha*1.0e6
			sda(iterOFE,ip,5) = tileDrainPesticide(iterOFE,ip)*hru_ha*1.0e6
			sda(iterOFE,ip,6) = sum(sda(iterOFE,ip,1:5))
		ENDDO
	ENDIF

    !! Nitrogen in soil profile
    DO sl = 1,numsl
        soilN = soilN + sol_no3(sl,iterOFE)
        soilN = soilN + sol_nh4(sl,iterOFE)
        soilN = soilN + sol_orgn(sl,iterOFE)
        soilN = soilN + sol_aorgn(sl,iterOFE)
        soilN = soilN + sol_fon(sl,iterOFE)
    ENDDO

    ! Nitrogen loss = organicN runoff + nitrate runoff + plant uptake + leaching + denitrification
    Nloss = abs(ssub(35,iterOFE)) + abs(ssub(37,iterOFE)) + abs(ssub(40,iterOFE)) + abs(ssub(41,iterOFE)) + abs(ssub(48,iterOFE))
    ! Nitrogen gain = fertilizer + fixation + atmospheric deposition
    Ngain = abs(ssub(45,iterOFE)) + abs(ssub(47,iterOFE)) + abs(ssub(54,iterOFE))

    !! COMMENT NEEDED
    IF (soilN <= 0.0001) Nbalance = 100.0
    IF (soilN > 0.0001) Nbalance = 100.0*((Ninitial + Ngain - Nloss) / (soilN))

    !! Phosphorus in soil profile
    DO sl = 1,numsl
        soilP = soilP + sol_labp(sl,iterOFE)
        soilP = soilP + sol_actp(sl,iterOFE)
        soilP = soilP + soilTotOrgP(sl,iterOFE)
        soilP = soilP + sol_stap(sl,iterOFE)
        soilP = soilP + sol_fop(sl,iterOFE)
    ENDDO

    ! Phosphorus loss = organicP runoff + solubleP runoff + plant uptake + mineralP sediment + percolated phosphorus
    Ploss = abs(ssub(36,iterOFE)) + abs(ssub(39,iterOFE)) + abs(ssub(42,iterOFE)) + abs(ssub(67,iterOFE)) + percp
    ! Phosphorus gain = fertilizer
    Pgain =  abs(ssub(46,iterOFE))

    !! COMMENT NEEDED
    IF (soilP <= 0.0) Pbalance = 100.0
    IF (soilP > 0.0) Pbalance = 100.0*((Pinitial + Pgain - Ploss) / (soilP))

    !! Calculate pesticide contents of layers, profile and average in soil for MASS BALANCE in OFE
    DO ip = 1,numPest
        DO sl = 1,numsl
            soilPsT = soilPsT + sol_pst(sl,iterOFE,ip)
        ENDDO
        soilPsT = soilPsT + plt_pst(ip,iterOFE)
    ENDDO

    !! Pesticide lost from profile
    DO ip = 1,1 !hrupest(iterOFE)
    ! Pesticide loss = organicP runoff + solubleP runoff + plant uptake + mineralP sediment + percolated phosphorus
        PsTloss = PsTloss + abs(pdb(ip)) + abs(ssb(83+ip)) + abs(ssb(46+ip)) + abs(ssb(73+ip))
    ! Pesticide gain = organicP runoff + solubleP runoff + plant uptake + mineralP sediment + percolated phosphorus
        PsTgain = PsTgain + abs(pab(ip))
    ENDDO

    !! COMMENT NEEDED
    IF (soilPsT <= 0.0) THEN
        PsTbalance = 100.0
    ELSE
        PsTbalance = 100.0*((PsTinitial + PsTgain - PsTloss)/(soilPsT))
    ENDIF
    RETURN
END
