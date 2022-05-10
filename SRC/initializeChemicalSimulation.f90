!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2021-11        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + uses free-source-format (132 character maximum per line)
!!    + there are two portions of this code:
!!        - the first is a one-time initialization in which nothing can be based on OFE iterators (iplane--WEPP or iofe--WEPPWQ)
!!        - the second is a daily initialization which is based on OFE iterators when applicable
!!        - both portions should be run before the first call to the main chemical subroutine
!!    + the appearance, simplicity, and maintainability of this code is limited by WEPP's old Fortran 77 conventions in that:
!!    + it should be a top priority to convert WEPP code to free-source modern Fortran (like this code was) so that:
!!        - lines and therefore variable names can be longer and more descriptive
!!        - a single parameter module can be used throughout all the code (removing the need to pass arguments between subroutines)
!!        - all redundant names, code, and documentation can be eliminated
!!    + in a multiple OFE scenario, runoff (in mm) needs to be recomputed for the specific OFE
!!        - WEPP provides values for runoff based on the beginning of the hillslope to the end of the OFE
!!        - this was not a problem for single-OFE simultaions, but multiple-OFE simulations need OFE-specific runoff values
!!        - the appropriate change has been incorporated to convert from mm (based on effective length) to mm (based on only the particular OFE of interest)
!!
!!    !! DEVELOPMENT NEEDED:
!!    + initialize plantN and plantP cumulative variables based on optimal N and P amounts for the simulation start date
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine initializes the chemical simulation in WEPP-WQ which includes:
!!      + Parsing inputs for determining allocation parameters
!!      + Allocating structures for the simulation
!!      + Initializing those structures where inputs are not provided
!!      + Updating daily variables from WEPP
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE initializeChemicalSimulation (nplane, iplane, ncrop, nyear, jyear, year, month, sdate, day, bddry, st, flux, ul,       &
                                         thetdr, dg, fc, solthk, FPHU, smrm, WQ_widthOFE, WQ_lengthOFE, runoff, rmagt, obmaxt,    &
                                         obmint, tmin, tmax, tave, radmj, salb, sedyldMT, OFEsedyldMT, rain, orgmat, LAI,         &
                                         wq_lateralFlow, jdharv, jdplt, drainq, WEPP_strsn, WEPP_strsp, WEPP_strsw, WEPP_strst,   &
                                         WEPP_dryBioMass, WEPP_HINDX, WEPP_rootMass, WEPP_del_res, WEPP_del_cov, WEPP_cropName,   &
                                         WQ_widthOFE1)

    !! Load parameter module
    USE parm

    !! Declare local variables
    IMPLICIT NONE
    CHARACTER (LEN=51):: WEPP_cropName
    INTEGER :: nplane, iplane, ncrop, nyear, jyear, year, month, day, tdplt, sdate, jdharv, iro, jdplt, getJulianDate,            &
               chemicalInitializationFlag
    REAL :: bddry(numsl,nplane), st(numsl,nplane), flux(numsl,nplane), ul(numsl,nplane), thetdr(numsl,nplane), fc(numsl),         &
            solthk(numsl,nplane), dg(numsl,nplane), rmagt, smrm(3,nplane), FPHU, WQ_widthOFE(nplane), WQ_lengthOFE(nplane),       &
            runoff, obmaxt(12), LAI, obmint(12), tmin, tmax, tave, radmj, salb, sedyldMT, OFEsedyldMT, rain, drainq,              &
            orgmat(numsl,nplane), wq_lateralFlow(numsl,nplane), WEPP_strsn, WEPP_strsp, WEPP_strsw, WEPP_strst, WEPP_HINDX,       &
            WEPP_rootMass, WEPP_dryBioMass, WEPP_del_res, WEPP_del_cov, WQ_widthOFE1

    !! Store and initialize the initialization flag
    SAVE chemicalInitializationFlag
    DATA chemicalInitializationFlag /0/

    !! Set parameter for WEPP error file messaging and stop behavior
    verbose = .TRUE.                                            ! Set to (.FALSE. / .TRUE.) for messages (off / on), respectively
    stopBehavior = 2                                            ! Set to (0 / 1 / 2) to (stop / continue with errors / continue with repair), respectively


    !!! -------------------- ONE-TIME INITIALIZATION -------------------- !!!

    !! Initialize the initial system state, array allocation
    IF (chemicalInitializationFlag .EQ. 0) THEN

        !! Print status update
        IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Initializing Chemical Model"

        !! Update daily variable inputs from WEPP
        numsl = 10                                                  ! Total number of unique soil layers in the hillslope simulation; WEPP always uses 10 soil layers
        numOFE = nplane                                             ! Total number of unique OFEs in the hillslope simulation
        numCrop = ncrop                                             ! Total number of unique crops in the hillslope simulation
        numYears = nyear                                            ! Total number of years in the hillslope simulation
        startYear = year                                            ! First year of the hillslope simulation
        startMonth = month                                          ! First month of the hillslope simulation
        startDay = day                                              ! First day of the hillslope simulation

        !! Determine array sizes, allocate, and initialize
        CALL getAllocationSizes
        CALL allocateParameters

        !! Compute OFE lengths, widths, and areas
        DO ofe = 1,numOFE
            cumLengthOFE(ofe) = WQ_lengthOFE(ofe)                                        ! cumulative OFE length in meters
            IF (ofe == 1) lengthOFE(ofe) = WQ_lengthOFE(ofe)                             ! incremental OFE length in meters if there is only one OFE
            IF (ofe == 1) widthOFE(ofe) = WQ_widthOFE1                                   ! incremental OFE width in meters if there is only one OFE
            IF (ofe > 1) lengthOFE(ofe) = WQ_lengthOFE(ofe) - WQ_lengthOFE(ofe-1)        ! incremental OFE length in meters if there are multiple OFEs
            IF (ofe > 1) widthOFE(ofe) = WQ_widthOFE(numOFE)                             ! incremental OFE width in meters if there are multiple OFEs
            areaOFE(ofe) = lengthOFE(ofe)*widthOFE(ofe)/10000.0                               ! incremental OFE area in hectares
            cumAreaOFE(ofe) = cumLengthOFE(ofe)*widthOFE(ofe)/10000.0                         ! cumulative OFE area in hectares
        ENDDO

        !! Compute hillslope length, width, and area
        hillslopeLength = cumLengthOFE(numOFE)                                          ! units in meters
        hillslopeWidth = widthOFE(numOFE)                                               ! units in meters
        hillslopeArea = hillslopeLength*hillslopeWidth/10000.0                          ! units in hectares

        !! Initialize air and soil temperatures
        IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Initializing Temperatures"
        avgMonthlyAirTemp(1:12) = (obmaxt(1:12) + obmint(1:12))/2.0
        avgAnnualAirTemp = sum(avgMonthlyAirTemp(1:12))/12.0
        soilTemperature(1:numsl,1:numOFE) = avgMonthlyAirTemp(month)

        !! Initialize structures determined by WEPP simulation once
        IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Initializing Constants"
        bottomDepth(1:numsl,1:numOFE) = solthk(1:numsl,1:numOFE)*1000.0                                     !Convert units from m to mm
        soilThickness(1:numsl,1:numOFE) = dg(1:numsl,1:numOFE)*1000.0                                       !Convert units from m to mm

        !! Read databases and store required simulation information
        IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Reading Chemical Databases"
        CALL readCropDatabaseFile
        CALL readPesticideDatabaseFile
        CALL readFertilizerDatabaseFile

        !! Read inputs and store required simulation information
        CALL readAtmosphericInputFile

        !! Read chemical inputs for each OFE
        IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Reading OFE Inputs"
        DO ofe = 1,numOFE
            CALL readSoilChemicalInputFile
            CALL readChemicalManagementInputFile
        ENDDO

        !! Initialize required inputs for soil chemistry initialization
        soilCarbon(1:numsl,1:numOFE) = 100.0*orgmat(1:numsl,1:numOFE)/1.73                                  !Convert units from
        dryBulkDensity(1:numsl,1:numOFE) = bddry(1:numsl,1:numOFE)/1000.0                                   !Convert units from kg/m**3 to Mg/m**3
        soilResidue(1:numsl,1:numOFE) = smrm(1:numsl,1:numOFE)*10000.0                                      !Convert units from kg/m**3 to kg/ha

        !! Compute soil carbon
        DO ofe = 1,numOFE
            CALL initializeSoilChemistry
        ENDDO

        !! Turn off the flag for the 1st initialization
        chemicalInitializationFlag = 1
        npest = 1
        nfert = 1

        !! Open output files
        OPEN (1111,FILE="dailyChemicals.txt")

        !! Open and close chemical graph file (used by WEPP-WQ interface)
        OPEN (1113,FILE="chemgraph.txt") ! RPM is not sure how much this can be changed; Check with JRF about interface compatibility
        CLOSE (1113)

        !! Reset, compute, and store initial chemical states
        IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Summarizing Initial Conditions"
        DO ofe=1,numOFE
            iterOFE = ofe
            CALL summarize
            Ninitial = soilN
            Pinitial = soilP
            PsTinitial = soilPsT
        ENDDO

    ENDIF


    !!! -------------------- DAILY UPDATE -------------------- !!!

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Updating Chemical Variables With Daily WEPP Inputs"

    !! Update scalar structures daily
    iwave = 0
    iofe = iplane                                               ! Current OFE in hillslope simulation
    calendarDay = day
    calendarMonth = month
    calendarYear = year
    julianDay = getJulianDate(calendarDay, calendarMonth)
    simulationDay = day
    simulationJulianDay = sdate
    simulationMonth = month
    simulationYear = MAX(jyear,1)
    cropName = WEPP_cropName
    Nstress(iofe) = WEPP_strsn
    Pstress(iofe) = WEPP_strsp
    Wstress(iofe) = WEPP_strsw
    Tstress(iofe) = WEPP_strst
    dryBioMass(iofe) = WEPP_dryBioMass
    rootMass(iofe) = WEPP_rootMass
    nro(iofe) = simulationYear
    julianDayPlant = jdplt
    julianDayHarvest = jdharv
    minAirTemp(iofe) = tmin
    maxAirTemp(iofe) = tmax
    avgAirTemp(iofe) = tave
    leafAreaIndex(iofe) = LAI
    soilAlbedo(iofe) = salb
    solarRadiation = radmj*0.04184                                                                      ! Convert units from langleys to MJ/m**2
    runoffConversionFactor = cumAreaOFE(iofe)/areaOFE(iofe)                                             ! WEPP runoff is computed over the effective length of the OFE, not the explicit length; this is a conversion factor

    !! Update array structures daily
    tileDrainFlow(iofe) = drainq*1000.0                                                                 !Convert units from m to mm
    surfaceFlow(iofe) = runoff*1000.0*runoffConversionFactor                                            !Convert units from m to mm
    sedimentYieldHillslope = sedyldMT
    sedimentYieldOFE(iofe) = OFEsedyldMT
    bio_ms(iofe) = dryBioMass(iofe) + rootMass(iofe)
    phuacc(iofe) = FPHU
    precipitation = rain*1000.0
    dryBulkDensity(1:numsl,1:numOFE) = bddry(1:numsl,1:numOFE)/1000.0                                   !Convert units from kg/m**3 to Mg/m**3
    lateralFlow(1:numsl,1:numOFE) = wq_lateralFlow(1:numsl,1:numOFE)*1000.0                             !Convert units from m to mm
    verticalFlow(1:numsl,1:numOFE) = flux(1:numsl,1:numOFE)*1000.0                                      !Convert units from m to mm
    soilCarbon(1:numsl,1:numOFE) = 100.0*orgmat(1:numsl,1:numOFE)/1.73                                  !Convert units from
    soilResidue(1:numsl,1:numOFE) = smrm(1:numsl,1:numOFE)*10000.0                                      !Convert units from kg/m**2 to kg/ha
    soilPorosity(1:numsl,1:numOFE) = (2.65 - dryBulkDensity(1:numsl,1:numOFE))/2.65                     !Calculated in g/cm**3 or Mg/m**3
    surfaceResidue(iofe) = rmagt*10000.0                                                                !Convert units from kg/m**2 to kg/ha
    soilWaterFC(1:numsl,iofe) = fc(1:numsl)*1000.0                                                      !Convert units from m to mm
    soilWaterPA(1:numsl,1:numOFE) = st(1:numsl,1:numOFE)*1000.0                                         !Convert units from m to mm
    soilWaterST(1:numsl,1:numOFE) = ul(1:numsl,1:numOFE)*1000.0                                         !Convert units from m to mm
    soilWaterWP(1:numsl,1:numOFE) = thetdr(1:numsl,1:numOFE)*soilThickness(1:numsl,1:numOFE)            !Convert units from m to mm
    soilWaterMA(1:numsl,1:numOFE) = soilWaterFC(1:numsl,1:numOFE) - soilWaterWP(1:numsl,1:numOFE)
    soilWater(1:numsl,1:numOFE) = soilWaterPA + soilWaterWP
    profileWaterFC(iofe) = sum(soilWaterFC(1:numsl,iofe))
    profileWater(iofe) = sum(soilWater(1:numsl,iofe))
    avgBulkDensity(iofe) = sum(dryBulkDensity(1:numsl,iofe))/numsl

    !! Optional error handling or efficiency optimization
    !    + insignificant values are set to zero to save computation time
    !    + unexpected values are allowed to exist per instruction from Dennis C. Flanagan to not interrupt execution due to this issue
    !    + should that decision change in the future, error handling of WEPP variables should go here
    IF (surfaceFlow(iofe) < 1.0e-10) surfaceFlow(iofe) = 0.0
    IF (sedimentYieldHillslope < 1.0e-10) sedimentYieldHillslope = 0.0
    IF (sedimentYieldOFE(iofe) < 1.0e-10) sedimentYieldOFE(iofe) = 0.0
    IF (lateralFlow(sl,iofe) < 1.0e-10) lateralFlow = 0.0  !! CHECK NEEDED: RPM: I am not sure WEPP's lateral flow calculations are any good as there seem to be many unrealistic values coming from the water balance routine.
    IF (tileDrainFlow(iofe) < 1.0e-10) tileDrainFlow(iofe) = 0.0

    !! Update subsurface soil arrays with daily WEPP predictions
    surfaceResidue(iofe) = surfaceResidue(iofe) + WEPP_del_cov                                          ! Add surface residue to the surface
    soilResidue(1,iofe) = soilResidue(1,iofe) + WEPP_del_res                                            ! Add submerged residue to the first soil layer
    sol_fon(1,iofe) = sol_fon(1,iofe) + WEPP_del_res*cnb(iofe)                                          ! Add fresh nitrogen to the first soil layer
    sol_fop(1,iofe) = sol_fop(1,iofe) + WEPP_del_res*cpt(iofe)                                          ! Add fresh phosphorus to the first soil layer

    RETURN

END SUBROUTINE
