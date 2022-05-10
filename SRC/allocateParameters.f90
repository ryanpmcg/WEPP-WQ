!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2021-11        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + refactored, converted to free-source format, non-WEPP-WQ variables were removed
!!    + see below for a comprehensive list of WEPP-WQ array variables as well as their units, descriptions, and options
!!    + this documentation is incomplete, but can be improved over time as familiar parties contribute
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine allocates parameters declared in the parameter module.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE allocateParameters

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: ssbLength, ssubLength

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Allocating Objects with Sizes:"

    !! Define local variables
    mcrdb = 118	! maximum number of crops/landcovers in crop database file
    ssbLength = 109
    ssubLength = 67

    !! Print allocation sizes
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Overland Flow Elements:     ", numOFE
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Soil Layers:                ", numsl
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Unique Crops:               ", mcrdb
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Unique Pesticides:          ", mxnp
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Unique Fertilizers:         ", nfdb
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Maximum Operations:         ", mno
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Pesticide Operations:       ", mapest
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Fertilizer Operations:      ", mafert
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Auto-Fertilizer Operations: ", mautof
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Maximum Annual Crops:       ", numCrop

    !!  Included variables documented as follows:
    !     + this list includes only about 95% of array variables; pre-allocated arrays and scalar variables are not included in the documentation here
    !     + this list is organized from simplest to most complex structurally (i.e., 1-D, then 2-D, then 3-D, etc.)
    !     + blank initial value means uninitialized or incomplete documentation; update this comment upon completed documentation
    !     + units and descriptions are complete and accurate to the best of my knowledge, but I cannot
    !         guarantee someone who edits this code will not render that untrue in the future (RPM: 11/18/2021)
    !     + if you edit the water quality code and you found this documentation helpful, make sure the
    !         next person will have the same experience by updating the documentation

    !!  Variable                                         |DefaultValue       |Unit              |Description        |Options
    !!  ---------------------------------------------------------------------------------------------------------------------------

    ! Related to simulation years
    ALLOCATE (pcNO3arr(numYears))                       !|0.0                |(0-1)             |default fraction of fertilizer that is mineral N (NO3 + NH4)
    ALLOCATE (pcNH4arr(numYears))                       !|0.0                |(0-1)             |default fraction of fertilizer that is mineral N (NO3 + NH4)

    ! Related to OFEs
    ALLOCATE (afminn(numOFE))                           !|0.0                |(0-1)             |default fraction of fertilizer that is mineral N (NO3 + NH4)
    ALLOCATE (afminp(numOFE))                           !|0.0                |(0-1)             |default fraction of fertilizer that is mineral P
    ALLOCATE (afnh4n(numOFE))                           !|0.0                |(0-1)             |default fraction of mineral N in fertilizer that is NH4-N
    ALLOCATE (aforgn(numOFE))                           !|0.0                |(0-1)             |default fraction of fertilizer that is organic nitrogen
    ALLOCATE (aforgp(numOFE))                           !|0.0                |(0-1)             |default fraction of fertilizer that is organic phosphorus
    ALLOCATE (afrt_ly1(numOFE))                         !|0.0                |(0-1)             |fraction of fertilizer which is applied to top 10 mm of soil (the remaining fraction is applied to first soil layer)
    ALLOCATE (anano3(numOFE))                           !|                   |kg N/ha           |total amount of nitrogen applied during the year in auto-fertilization
    ALLOCATE (anion_excl(numOFE))                       !|                   |(0-1)             |fraction of porosity from which anions are excluded
    ALLOCATE (appliedN(numOFE))                         !|0.0                |kg/ha             |amount of nitrogen fertilizer applied in the OFE for the day
    ALLOCATE (appliedP(numOFE))                         !|0.0                |kg/ha             |amount of phosphorus fertilizer applied in the OFE for the day
    ALLOCATE (areaOFE(numOFE))                          !|                   |ha                |explicit area of the OFE
    ALLOCATE (auto_eff(numOFE))                         !|                   |none              |fertilizer application efficiency calculated as the amount of N applied divided by the amount of N removed at harvest
    ALLOCATE (auto_nmxa(numOFE))                        !|                   |kg NO3-N/ha       |maximum NO3-N content allowed to be applied in one year
    ALLOCATE (auto_nmxs(numOFE))                        !|                   |kg NO3-N/ha       |maximum NO3-N content allowed in one fertilizer application
    ALLOCATE (auto_nstr(numOFE))                        !|-1.0               |none              |nitrogen stress factor which triggers auto fertilization
    ALLOCATE (avgAirTemp(numOFE))                       !|                   |deg C             |average air temperature of the OFE for the day
    ALLOCATE (avgBulkDensity(numOFE))                   !|                   |Mg/m^3        	|average bulk density of soil profile
    ALLOCATE (bio_ms(numOFE))                           !|                   |kg/ha				|land cover/crop biomass (dry weight)
    ALLOCATE (cmn(numOFE))                              !|0.003              |(0-1)             |rate factor for humus mineralization on active organic N
    ALLOCATE (cnb(numOFE))                              !|0.0                |(0-1)             |fraction of plant biomass that is nitrogen
    ALLOCATE (cpt(numOFE))                              !|                   |(0-1)             |fraction of plant biomass that is phosphorus
    ALLOCATE (cumAreaOFE(numOFE))                       !|                   |ha                |cumulative area of the OFE (and upslope OFEs)
    ALLOCATE (cumLengthOFE(numOFE))                     !|                   |m                 |cumulative length of the OFE (and upslope OFEs)
    ALLOCATE (dryBioMass(numOFE))                       !|                   |??                |?? !! COMMENT NEEDED
    ALLOCATE (enrichmentRatio(numOFE))                  !|NULL               |none              |enrichment ratio
    ALLOCATE (hru_fr(numOFE))                           !|1.0                |(0-1)             |fraction of watershed area in OFE
    ALLOCATE (icr(numOFE))                              !|1                  |none              |sequence number of crop grown within the current year
    ALLOCATE (leafAreaIndex(numOFE))                    !|                   |none              |leaf area index
    ALLOCATE (lengthOFE(numOFE))                        !|                   |m                 |explicit length of the OFE
    ALLOCATE (minAirTemp(numOFE))                       !|                   |deg C             |minimum air temperature of the OFE for the day
    ALLOCATE (maxAirTemp(numOFE))                       !|                   |deg C             |maximum air temperature of the OFE for the day
    ALLOCATE (newResidue(numOFE))                       !|                   |kg/ha             |new residue on the soil surface of the OFE for the day
    ALLOCATE (nfert(numOFE))                            !|                   |none              |sequence number of fertilizer application within the current year
    ALLOCATE (nFixation(numOFE))                        !|                   |kg/ha             |amount of nitrogen fixed for the day
    ALLOCATE (npest(numOFE))                            !|                   |none              |sequence number of pesticide application within the current year
    ALLOCATE (nperco(numOFE))                           !|                   |none              |nitrogen percolation coefficient (0-1)
                                                        !|                                                          |0:concentration of soluble N in surface runoff is zero
                                                        !|                                                          |1:percolate has same concentration of soluble N as surface runoff
    ALLOCATE (nro(numOFE))                              !|                   |none              |chemical management sequence year
    ALLOCATE (nops(numOFE))                             !|                   |None              |Number of chemical management operations for each OFE
    ALLOCATE (percop(numOFE))                           !|                   |none              |pesticide percolation coefficient (0-1)
                                                        !|                                                          |0: concentration of pesticide in surface runoff is zero
                                                        !|                                                          |1: percolate has same concentration of pesticide as surface runoff
    ALLOCATE (phoskd(numOFE))                           !|m**3/Mg            |None              |Phosphorus soil partitioning coefficient: Ratio of phosphorus attached to sediment to phosphorus dissolved in soil water
    ALLOCATE (phuacc(numOFE))                           !|                   |None              |Fraction of plant heat units accumulated
    ALLOCATE (plantN(numOFE))                           !|                   |kg/ha             |cumulative plant nitrogen content
    ALLOCATE (plantP(numOFE))                           !|                   |kg/ha             |cumulative plant phosphorus content
    ALLOCATE (pperco(numOFE))                           !|                   |none              |phosphorus percolation coefficient (0-1)
                                                        !|                                                          |0:concentration of soluble P in surface runoff is zero
                                                        !|                                                          |1:percolate has same concentration of soluble P as surface runoff
    ALLOCATE (profileWater(numOFE))                     !|                   |mm H2O            |amount of water stored in the OFE soil profile for the day
    ALLOCATE (profileWaterFC(numOFE))                   !|                   |mm H2O            |amount of water stored in the OFE soil profile at field capacity (-0.033 MPa) for the day
    ALLOCATE (pai(numOFE))                              !|                   |none              |Phosphorus availability index. The fraction of fertilizer P remaining in labile pool after initial rapid phase of P sorption (SWAT EQN 3:2.3.1)
    ALLOCATE (rtn(numOFE))                              !|NULL               |(0-1)             |Fraction of nitrogen in the active organic pool
    ALLOCATE (rtof(numOFE))                             !|NULL               |(0-1)             |Fraction of fresh organic content in compost application
    ALLOCATE (rootMass(numOFE))                         !|MISSING
    ALLOCATE (runoffNO3(numOFE))                        !|                   |kg/ha             |Nitrate in runoff
    ALLOCATE (runoffLabileP(numOFE))                    !|                   |kg/ha             |Soluble phosphate in runoff
    ALLOCATE (sedimentConcentration(numOFE))            !|                   |Mg                |sediment yield for the day
    ALLOCATE (sedimentYieldOFE(numOFE))                 !|                   |Mg                |sediment yield for the day
    ALLOCATE (sedimentOrgN(numOFE))                     !|NULL               |kg/ha             |organic nitrogen in sediment/runoff
    ALLOCATE (sedimentMinP(numOFE))                     !|NULL               |kg/ha             |mineral phosphorus sorbed to sediment
    ALLOCATE (sedimentOrgP(numOFE))                     !|NULL               |kg/ha             |organic phosphorus in sediment/runoff
    ALLOCATE (soilAlbedo(numOFE))                       !|                   |(0-1)             |soil albedo of the OFE soil surface for the day
    ALLOCATE (soilCarbon(numsl,numOFE))                 !|                   |%                 |percent organic carbon in soil layer for the day
    ALLOCATE (solarRadiation(numOFE))                   !|                   |MJ/m^2            |solar radiation for the day in OFE
    ALLOCATE (surfaceFlow(numOFE))                      !|                   |mm                |volume of runoff for the day
    ALLOCATE (surfaceResidue(numOFE))                   !|                   |kg/ha         	|amount of residue on soil surface for the day
    ALLOCATE (surfaceTemp(numOFE))                      !|                   |deg C             |soil surface temperature
    ALLOCATE (tileDrainFlow(numOFE))                    !|                   |mm                |volume of tile drainage flow for the day
    ALLOCATE (tileDrainNO3(numOFE))                     !|                   |kg/ha             |Nitrate in tile flow
    ALLOCATE (tileDrainLabileP(numOFE))                 !|                   |kg/ha             |Soluble phosphate in tile flow
    ALLOCATE (ubn(numOFE))                              !|                   |kg/ha             |nitrogen uptake distribution parameter (controls amount of nitrogen removed from soil layers by the plant)
    ALLOCATE (ubp(numOFE))                              !|                   |kg/ha             |phosphorus uptake distribution parameter: This parameter controls the amount of phosphorus removed from the different soil layers by the plant. In particular, this parameter allows the amount of phosphorus removed from the surface layer via plant uptake to be controlled. While the relationship between UBP and P uptake from the surface layer is affected by the depth of the soil profile, in general, as UBP increases the amount of P removed from the surface layer relative to the amount removed from the entire profile increases
    ALLOCATE (uobn(numOFE))                             !|                   |kg/ha             |nitrogen uptake normalization parameter (normalizes nitrogen uptake so the model can verify uptake from different soil layers sums to 1.0)
    ALLOCATE (uobp(numOFE))                             !|                   |kg/ha             |phosphorus uptake normalization parameter (normalizes phosphorus uptake so the model can verify uptake from different soil layers sums to 1.0)
    ALLOCATE (Nstress(numOFE))                          !|                   |(0-1)             |fraction of potential plant growth achieved on the day where the reduction is caused by water stress nitrogen
    ALLOCATE (Pstress(numOFE))                          !|                   |(0-1)             |fraction of potential plant growth achieved on the day where the reduction is caused by water stress phosphorus
    ALLOCATE (Tstress(numOFE))                          !|                   |(0-1)             |fraction of potential plant growth achieved on the day where the reduction is caused by water stress temperature
    ALLOCATE (Wstress(numOFE))                          !|                   |(0-1)             |fraction of potential plant growth achieved on the day where the reduction is caused by water stress
    ALLOCATE (widthOFE(numOFE))                         !|                   |m                 |explicit width of the OFE (assumed to be the same as the hillslope width)
    ALLOCATE (yield(numOFE))                            !|                   |kg/ha             |total yield
    ALLOCATE (yieldN(numOFE))                           !|                   |kg/ha             |total nitrogen content in yield
    ALLOCATE (yieldP(numOFE))                           !|                   |kg/ha             |total phosphorus content in yield

    ! Related to pesticides
    ALLOCATE (ap_ef(mxnp))                              !|                   |none              |application efficiency (0-1)
    ALLOCATE (decay_f(mxnp))                            !|                   |none              |exponential of the rate constant for degradation of the pesticide on foliage
    ALLOCATE (decay_s(mxnp))                            !|                   |none              |exponential of the rate constant for degradation of the pesticide in soil
    ALLOCATE (hlife_f(mxnp))                            !|                   |days              |half-life of pesticide on foliage
    ALLOCATE (hlife_s(mxnp))                            !|                   |days              |half-life of pesticide in soil
    ALLOCATE (henry(mxnp))                              !|                   |none              |Henry's Constant for the chemical
    ALLOCATE (pab(mxnp))                                !|                   |kg/ha             |total amount of pesticide type applied in watershed during simulation
    ALLOCATE (pdb(mxnp))                                !|                   |kg pst/ha         |amount of pesticide lost through degradation in watershed
    ALLOCATE (skoc(mxnp))                               !|                   |(mg/kg)/(mg/L)    |soil adsorption coefficient normalized for soil organic carbon content
    ALLOCATE (wof(mxnp))                                !|                   |(0-1)             |fraction of pesticide on foliage which is washed-off by a rainfall event
    ALLOCATE (wsol(mxnp))                               !|                   |mg/L (ppm)        |solubility of chemical in water

    ! Related to crops
    ALLOCATE (bio_e(mcrdb))                             !|                   |(kg/ha)/(MJ/m**2) |biomass-energy ratio: The potential (unstressed) growth rate per unit of intercepted photosynthetically active radiation.
    ALLOCATE (bio_n1(mcrdb))                            !|                   |none              |1st shape parameter for plant N uptake equation
    ALLOCATE (bio_n2(mcrdb))                            !|                   |none              |2nd shape parameter for plant N uptake equation
    ALLOCATE (bio_p1(mcrdb))                            !|                   |none              |1st shape parameter for plant P uptake equation
    ALLOCATE (bio_p2(mcrdb))                            !|                   |none              |2st shape parameter for plant P uptake equation
    ALLOCATE (blai(mcrdb))                              !|                   |none              |maximum (potential) leaf area index
    ALLOCATE (chtmx(mcrdb))                             !|                   |m                 |maximum canopy height
    ALLOCATE (cnyld(mcrdb))                             !|                   |(0-1)             |fraction of nitrogen in yield
    ALLOCATE (cpnm(mcrdb))                              !|                   |NA                |four character code to represent crop name
    ALLOCATE (cpyld(mcrdb))                             !|                   |(0-1)             |fraction of phosphorus in yield
    ALLOCATE (cvm(mcrdb))                               !|                   |none              |natural log of USLE_C
    ALLOCATE (dlai(mcrdb))                              !|                   |(0-1)             |fraction of growing season when leaf area declines
    ALLOCATE (gsi(mcrdb))                               !|                   |m/s               |maximum stomatal conductance
    ALLOCATE (harvestIndex(mcrdb))                      !|                   |(kg/ha)/(kg/ha)   |harvest index: crop yield/aboveground biomass
    ALLOCATE (idc(mcrdb))                               !|                   |none              |crop/landcover category:
																								!                   |1 warm season annual legume
																								!                   |2 cold season annual legume
																								!                   |3 perennial legume
																								!                   |4 warm season annual
																								!                   |5 cold season annual
																								!                   |6 perennial
																								!                   |7 trees
    ALLOCATE (leaf1(mcrdb))                             !|                   |none              |1st shape parameter for leaf area development equation
    ALLOCATE (leaf2(mcrdb))                             !|                   |none              |2nd shape parameter for leaf area development equation
    ALLOCATE (rdmx(mcrdb))                              !|                   |m                 |maximum root depth
    ALLOCATE (rsdco_pl(mcrdb))                          !|                   |none              |plant residue decomposition coefficient. The fraction of residue which will decompose in a day assuming optimal moisture, temperature, C:N ratio, and C:P ratio
    ALLOCATE (t_base(mcrdb))                            !|                   |deg C             |minimum temperature for plant growth
    ALLOCATE (t_opt(mcrdb))                             !|                   |deg C             |optimal temperature for plant growth
    ALLOCATE (vpd2(mcrdb))                              !|                   |(kg/ha)/          |rate of decline in biomass-energy ratio per unit increase in vapor pressure deficit (when vapor pressure deficit exceeds VPTH)
														                     !((MJ/m**2)*kPa)
    ALLOCATE (vpth(mcrdb))                              !|                   |kPa               |threshold vapor pressure deficit value. the value above which leaf conductance is sensitive (ie is affected) by vapor pressure deficit
    ALLOCATE (wac21(mcrdb))                             !|                   |none              |1st shape parameter for radiation use efficiency equation.
    ALLOCATE (wac22(mcrdb))                             !|                   |none              |2nd shape parameter for radiation use efficiency equation.
    ALLOCATE (wavp(mcrdb))                              !|                   |none              |Rate of decline in radiation use efficiency as a function of vapor pressure deficit
    ALLOCATE (wsyf(mcrdb))                              !|                   |(kg/ha)/(kg/ha)   |Value of harvest index between 0 and HVSTI which represents the lowest value expected due to water stress
    ALLOCATE (bn(3,mcrdb))                              !|                   |kg N/kg biomass   |nitrogen uptake parameter #1: normal fraction of N in crop biomass at emergence, 0.5 maturity, and maturity (rank-1, positions 1-3, respectively)
    ALLOCATE (bp(3,mcrdb))                              !|                   |kg P/kg biomass   |phosphorus uptake parameter #1: normal fraction of P in crop biomass at emergence, 0.5 maturity, and maturity (rank-1, positions 1-3, respectively)

    ! Related to fertilizers
    ALLOCATE (bactkddb(nfdb))                           !|                   |none              |bacteria partition coefficient: 1: all bacteria in solution 0: all bacteria sorbed to soil particles
    ALLOCATE (bactlpdb(nfdb))                           !|                   |# bact/kg manure  |concentration of less persistent bacteria in manure(fertilizer)
    ALLOCATE (bactpdb(nfdb))                            !|                   |# bact/kg manure  |concentration of persistent bacteria in manure(fertilizer)
    ALLOCATE (fminn(nfdb))                              !|                   |(0-1)             |database fraction of fertilizer that is mineral N (NO3 + NH4)
    ALLOCATE (fminp(nfdb))                              !|                   |(0-1)             |database fraction of fertilizer that is mineral P
    ALLOCATE (fnh4n(nfdb))                              !|                   |(0-1)             |database fraction of mineral N in fertilizer that is NH4-N
    ALLOCATE (forgn(nfdb))                              !|                   |(0-1)             |database fraction of fertilizer that is organic nitrogen
    ALLOCATE (forgp(nfdb))                              !|                   |(0-1)             |database fraction of fertilizer that is organic phosphorus
    ALLOCATE (fertnm(nfdb))                             !|""                 |string            |name of fertilizer

    ! Related to soil layers, OFEs
    ALLOCATE (bottomDepth(numsl,numOFE))                !|from WEPP          |mm                |depth to bottom of soil layer
    ALLOCATE (conv_wt(numsl,numOFE))                    !|from bulk density  |None              |Factor which converts kg/kg soil to kg/ha
    ALLOCATE (denitrification(numsl,numOFE))            !|0.0                |kg/ha             |denitrification in the soil layer for the day
    ALLOCATE (dryBulkDensity(numsl,numOFE))             !|from WEPP          |Mg/m**3           |dry bulk density of soil layer for the day
    ALLOCATE (activeOrgNApplied(numsl,numOFE))          !|NULL               |kg/ha             |active organic nitrogen applied for the day
    ALLOCATE (activeOrgPApplied(numsl,numOFE))          !|NULL               |kg/ha             |active organic phosphorus applied for the day
    ALLOCATE (freshOrgNApplied(numsl,numOFE))           !|NULL               |kg/ha             |fresh organic nitrogen applied for the day
    ALLOCATE (freshOrgPApplied(numsl,numOFE))           !|NULL               |kg/ha             |fresh organic phosphorus applied for the day
    ALLOCATE (stableOrgNApplied(numsl,numOFE))          !|NULL               |kg/ha             |stable organic nitrogen applied for the day
    ALLOCATE (stableOrgPApplied(numsl,numOFE))          !|NULL               |kg/ha             |stable organic phosphorus applied for the day
    ALLOCATE (lateralFlow(numsl,numOFE))                !|from WEPP          |mm                |lateral flow of water in soil layer for the day
    ALLOCATE (lateralLabileP(numsl,numOFE))             !|0.0                |kg/ha             |Soluble phosphate in lateral flow
    ALLOCATE (lateralNO3(numsl,numOFE))                 !|0.0                |kg/ha             |Nitrate in lateral flow
    ALLOCATE (mobileNO3(numsl,numOFE))                  !|0.0                |kg/ha             |Nitrate in mobile water
    ALLOCATE (nActOrgMin(numsl,numOFE))                 !|0.0                |kg/ha             |Mineralization from the active organic nitrogen pool to the nitrate pool; negative values indicate reverse direction
    ALLOCATE (nStaOrgMin(numsl,numOFE))                 !|0.0                |kg/ha             |Mineralization from the stable organic to active organic nitrogen pool; negative values indicate reverse direction
    ALLOCATE (pActOrgMin(numsl,numOFE))                 !|0.0                |kg/ha             |Mineralization from the active organic phosphorus pool to the nitrate pool; negative values indicate reverse direction
    ALLOCATE (pStaOrgMin(numsl,numOFE))                 !|0.0                |kg/ha             |Mineralization from the stable organic to active organic phosphorus pool; negative values indicate reverse direction
    ALLOCATE (soilActOrgP(numsl,numOFE))                !|from org. N pools  |kg/ha             |amount of phosphorus stored in the active organic pool
    ALLOCATE (soilStaOrgP(numsl,numOFE))                !|from org. N pools  |kg/ha             |amount of phosphorus stored in the stable organic pool
    ALLOCATE (soilTotOrgP(numsl,numOFE))                !|from .cem file     |kg/ha             |amount of phosphorus stored in the active and stable organic pool
    ALLOCATE (soilPorosity(numsl,numOFE))               !|from bulk density  |(0-1)             |total porosity of soil layer expressed as a fraction of the total volume for the day
    ALLOCATE (soilResidue(numsl,numOFE))                !|from WEPP          |kg/ha             |amount of organic matter in the soil classified as residue for the day
    ALLOCATE (soilThickness(numsl,numOFE))              !|from WEPP          |mm                |thickness of the soil layer for the day
    ALLOCATE (soilTemperature(numsl,numOFE))            !|from air temp.     |deg C             |temperature of the soil layer for the day
    ALLOCATE (soilWaterPA(numsl,numOFE))                !|from WEPP          |mm H2O            |amount of water stored in the soil layer less WP water (PA = Plant Available) for the day
    ALLOCATE (soilWaterFC(numsl,numOFE))                !|from WEPP          |mm H2O            |amount of water stored in the soil layer at -0.033 MPa (FC = Field Capacity) for the day
    ALLOCATE (soilWaterMA(numsl,numOFE))                !|from WEPP          |mm H2O            |amount of water stored in the soil layer between -1.5 MPa and -0.033 MPa (MA = Maximum Plant Available; FC minus WP) for the day
    ALLOCATE (soilWaterST(numsl,numOFE))                !|from WEPP          |mm H2O            |amount of water stored in the soil layer at saturation (ST = Saturation) for the day
    ALLOCATE (soilWaterWP(numsl,numOFE))                !|from WEPP          |mm H2O            |amount of water stored in the soil layer at -1.5 MPa (WP = Wilting Point) for the day
    ALLOCATE (soilWater(numsl,numOFE))                  !|from WEPP          |mm H2O            |amount of water stored in the soil layer for the day
    ALLOCATE (sol_actp(numsl,numOFE))                   !|from labile P      |kg/ha             |amount of phosphorus stored in the active (inorganic/mineral) pool
    ALLOCATE (sol_aorgn(numsl,numOFE))                  !|from organic N     |kg/ha             |amount of nitrogen stored in the active organic (humic) pool
    ALLOCATE (sol_fon(numsl,numOFE))                    !|from residue       |kg/ha             |amount of nitrogen stored in the fresh organic (residue) pool
    ALLOCATE (sol_fop(numsl,numOFE))                    !|from residue       |kg/ha             |amount of phosphorus stored in the fresh organic (residue) pool
    ALLOCATE (sol_labp(numsl,numOFE))                   !|from .cem file     |kg/ha             |amount of phosphorus stored in the labile (mineral/solution) pool
    ALLOCATE (sol_nh4(numsl,numOFE))                    !|0.0                |kg/ha             |amount of nitrogen stored in the ammonium pool in soil layer
    ALLOCATE (sol_no3(numsl,numOFE))                    !|from .cem file     |kg/ha             |amount of nitrogen stored in the nitrate pool in soil layer
    ALLOCATE (sol_orgn(numsl,numOFE))                   !|from .cem file     |kg/ha             |amount of nitrogen stored in the stable organic N pool
    ALLOCATE (sol_stap(numsl,numOFE))                   !|from active org. P |kg/ha             |amount of phosphorus stored in the stable (inorganic/mineral) pool
    ALLOCATE (uptakeNO3(numsl,numOFE))                  !|0.0                |kg/ha             |amount of nitrate taken up by plant growth
    ALLOCATE (uptakeLabileP(numsl,numOFE))              !|0.0                |kg/ha             |amount of phosphorus taken up by plant growth
    ALLOCATE (verticalFlow(numsl,numOFE))               !|from WEPP          |mm                |vertical flow of water in soil layer for the day
    ALLOCATE (verticalLabileP(numsl,numOFE))            !|0.0                |kg/ha             |Soluble phosphate in vertical flow
    ALLOCATE (verticalNO3(numsl,numOFE))                !|0.0                |kg/ha             |Nitrate in vertical flow

    ! Related to pesticides, OFEs, soil layers
    ALLOCATE (lateralPesticide(numsl,numOFE,mxnp))      !|0.0                |kg/ha             |Pesticide in lateral flow
    ALLOCATE (mobilePesticide(numsl,numOFE,mxnp))       !|0.0                |kg/ha             |Pesticide in mobile water
    ALLOCATE (sol_kp(numsl,numOFE,mxnp))                !|from soil C & Koc  |(mg/kg)/(mg/L)    |pesticide sorption coefficient, Kp; the ratio of the concentration in the solid phase to the concentration in solution
    ALLOCATE (sol_pst(numsl,numOFE,mxnp))               !|from .cem file     |kg/ha             |amount of pesticide in layer
    ALLOCATE (verticalPesticide(numsl,numOFE,mxnp))     !|0.0                |kg/ha             |Pesticide in vertical flow

    ! Related to operations, applications, and OFEs
    ALLOCATE (frminn(mno,mafert,numOFE))                !|                   |(0-1)             |read fraction of fertilizer that is mineral N (NO3 + NH4)
    ALLOCATE (frminp(mno,mafert,numOFE))                !|                   |(0-1)             |read fraction of fertilizer that is mineral P
    ALLOCATE (frnh4n(mno,mafert,numOFE))                !|                   |(0-1)             |read fraction of mineral N in fertilizer that is NH4-N
    ALLOCATE (frorgn(mno,mafert,numOFE))                !|                   |(0-1)             |read fraction of fertilizer that is organic nitrogen
    ALLOCATE (frorgp(mno,mafert,numOFE))                !|                   |(0-1)             |read fraction of fertilizer that is organic phosphorus
    ALLOCATE (fcompost(mno,mafert,numOFE))              !|                   |(0-1)             |read fraction of fertilizer that is compost
    ALLOCATE (frt_kg(mno,mafert,numOFE))                !|                   |kg/ha             |amount of fertilizer applied to OFE
    ALLOCATE (frt_ly1(mno,mafert,numOFE))               !|                   |(0-1)             |fraction of fertilizer which is applied to the top 10 mm of soil (the remaining fraction is applied to the first soil layer)
    ALLOCATE (iafer(mno,mautof,numOFE))                 !|                   |julian date       |date of automated fertilizer application
    ALLOCATE (ifert(mno,mafert,numOFE))                 !|                   |julian date       |date of fertilizer application
    ALLOCATE (ipst(mno,mapest,numOFE))                  !|400                |julian date       |date of pesticide application
    ALLOCATE (pst_kg(mno,mapest,numOFE))                !|                   |kg/ha             |amount of pesticide applied to OFE
    ALLOCATE (ipest(mno,mapest,numOFE))                 !|                   |none              |pesticide identification number from pest.dat

    ! Related to operations, crops grown per year, and OFEs
    ALLOCATE (ncr(mno,numCrop,numOFE))                  !|                   |none              |land cover code from crop.dat


    ! Related to pesticides, OFEs
    ALLOCATE (dkf(mxnp,numOFE))                         !|                   |kg/ha             |amount of pesticide lost through degradation from plant foliage
    ALLOCATE (dkg(mxnp,numOFE))                         !|                   |kg/ha             |amount of pesticide lost through degradation from soil
    ALLOCATE (plt_pst(mxnp,numOFE))                     !|                   |kg/ha             |pesticide on plant foliage
    ALLOCATE (pst_enr(mxnp,numOFE))                     !|                   |none              |pesticide enrichment ratio
    ALLOCATE (sedimentPesticide(mxnp,numOFE))           !|                   |kg/ha             |pesticide loading from OFE sorbed onto sediment
    ALLOCATE (runoffPesticide(numOFE,mxnp))             !|                   |kg/ha             |Pesticide in runoff
    ALLOCATE (sda(numOFE,mxnp,6))                       !|                   |mg                |a summary structure for pesticide movement in each OFE (3rd rank 1-6 corresponds to: runoff, sediment, vertical, lateral, tile, and total)
    ALLOCATE (tileDrainPesticide(numOFE,mxnp))          !|                   |kg/ha             |Pesticide in tile flow

    ! Related to other variables
    ALLOCATE (avgMonthlyAirTemp(12))				    !|                   |deg C             |average monthly air temperature in the hillslope
    ALLOCATE (ndays(13))                                !|                   |julian date       |julian date for last day of preceding month (where the array location is the number of the month). The dates are for leap years
	!ALLOCATE (sub_fr(:))     						    !|                   |(0-1)             |fraction of watershed area in hillslope
    ALLOCATE (ssb(ssbLength))                           !|                   |varies            |a summary structure for the hillslope or watershed simulation
    ALLOCATE (ssub(ssubLength,numOFE))                  !|                   |varies            |a summary structure for each OFE

    CALL initializeObjects

    RETURN
END
