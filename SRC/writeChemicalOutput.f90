!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2021-10        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + uses free-source-format (132 character maximum per line)
!!    + added support for multiple pesticides, multiple OFEs, and multiple soil layers
!!    + reformatted output to be more user-friendly and produce properly aligned output
!!    + the next revision of this code should move to a variable width formatting option to reduce code maintenance costs
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine writes daily chemical outputs for nitrogen, phosphorus, and pesticides
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                  |units          |definition
!!    -----------------------------------------------------------------------
!!    chemicalHeaderFlag    |none           |flag indicating header write status
!!    day                   |none           |current simulation day
!!    dkg(i,k)              |kg/ha          |degraded pesticide
!!    month                 |none           |current simulation month
!!    pstsol(i)             |kg/ha          |leached pesticide
!!    year                  |none           |current simulation year
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE writeChemicalOutput

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: chemicalHeaderFlag, ip, counter, searchStart, arrayLength
    INTEGER, DIMENSION (:), ALLOCATABLE :: whichPest, searchArray
    SAVE chemicalHeaderFlag
    DATA chemicalHeaderFlag /0/

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Writing Chemical Output"

    !!! -------------------- SEARCH FOR PESTICIDE INDICES -------------------- !!!

    !! Count the number of pesticides active in the simulation and allocate location structures
    numPest = SUM(pstflg)
    WRITE(*,*) numPest
    ALLOCATE (whichPest(numPest))

    !! Search for all active pesticides and store indexed locations
    IF (numPest > 0) THEN

        ! Reset counters
        counter = 0
        searchStart = 0

        ! Perform the search
        DO

            ! Set counters
            counter = counter + 1                                                                                                   ! Increase active pesticide array counter
            searchStart = searchStart + 1                                                                                           ! Increase search location counter
            arrayLength = SIZE(pstflg(searchStart:mxnp), 1)                                                                         ! Update the explicit search array size
            CALL findFirstMatch(pstflg(searchStart:mxnp), arrayLength, 1, whichPest(counter))                                       ! Find the first match in the search space if any

            ! Report any search error and abort the pesticide reporting operation
            IF (whichPest(counter) == 0) THEN
                WRITE (*,*) "Critical Error: An expected active pesticide could not be found during output generation;"
                WRITE (*,*) "                pesticide reporting is turned off to allow the simulation to proceed."
                numPest = 0
                EXIT
            ENDIF

            ! Update the index location, search start counter, and exit condition
            whichPest(counter) = whichPest(counter) + searchStart - 1
            searchStart = whichPest(counter)
            IF (counter == numPest) EXIT

        ENDDO

    ENDIF


    !!! -------------------- WRITE FILE HEADER INFORMATION -------------------- !!!

    !! Write the file header and update the header flag
    IF (chemicalHeaderFlag == 0) THEN

        ! Write column names
        IF (numPest > 1) THEN
            WRITE (1111,1000,ADVANCE="no")              ! Fixed names, don't advance the write line
            DO ip=1,(numPest-1)                         ! Iterate variable names
                WRITE (1111,1001,ADVANCE="no")          ! Variable names, don't advance the write line
            ENDDO                                       ! Exit loop
            WRITE (1111,1001)                           ! Final names, advance the write line
        ELSEIF (numPest <= 1) THEN
            WRITE (1111,1000,ADVANCE="no")              ! Fixed names, don't advance the write line
            WRITE (1111,1001)                           ! Final names, advance the write line
        ENDIF


        ! Write separator line
        IF (numPest > 1) THEN
            WRITE (1111,2000,ADVANCE="no")              ! Fixed names, don't advance the write line
            DO ip=1,(numPest-1)                         ! Iterate variable names
                WRITE (1111,2001,ADVANCE="no")          ! Variable names, don't advance the write line
            ENDDO                                       ! Exit loop
            WRITE (1111,2001)                           ! Final names, advance the write line
        ELSEIF (numPest <= 1) THEN
            WRITE (1111,2000,ADVANCE="no")              ! Fixed names, don't advance the write line
            WRITE (1111,2001)                           ! Final names, advance the write line
        ENDIF

        ! Set header flag status
        chemicalHeaderFlag = 1
    ENDIF


    !!! -------------------- WRITE DAILY INFORMATION -------------------- !!!

    !! Loop over soil layers
    DO sl = 1,numsl

        !! Write surface data (soil surface layer)
        IF (sl == 1) THEN

            ! Write fixed data columns
            WRITE(*,*) soilWater(sl,iofe), appliedN(iofe), appliedP(iofe) !, sedimentPesticide(iofe,ip), decayPesticide(sl,iofe,ip)
            WRITE (1111,3000,ADVANCE="no") calendarYear, calendarMonth, calendarDay, iofe, sl,                                    &
                                           lengthOFE(iofe), widthOFE(iofe), bottomDepth(sl,iofe),                                 &
                                           dryBulkDensity(sl,iofe)*1000.0, soilPorosity(sl,iofe), soilCarbon(sl,iofe),            &
                                           soilResidue(sl,iofe), surfaceResidue(iofe), Wstress(iofe),                             &
                                           soilWaterPA(sl,iofe), precipitation, surfaceFlow(iofe), sedimentYieldOFE(iofe)*1000.0, &
                                           sedimentConcentration(iofe)*1000.0, enrichmentRatio(iofe), verticalFlow(sl,iofe),      &
                                           lateralFlow(sl,iofe), tileDrainFlow(iofe), Tstress(iofe), soilTemperature(sl,iofe),    &
                                           Nstress(iofe), sol_no3(sl,iofe), sol_nh4(sl,iofe), sol_fon(sl,iofe),                   &
                                           sol_aorgn(sl,iofe), sol_orgn(sl,iofe), uptakeNO3(sl,iofe), plantN(iofe),               &
                                           nFixation(iofe), denitrification(sl,iofe), wetDepositionNO3, dryDepositionNO3,         &
                                           wetDepositionNH4, dryDepositionNH4, runoffNO3(iofe), sedimentOrgN(iofe),               &
                                           verticalNO3(sl,iofe), lateralNO3(sl,iofe), tileDrainNO3(iofe), Pstress(iofe),          &
                                           sol_labp(sl,iofe), sol_actp(sl,iofe), sol_stap(sl,iofe), sol_fop(sl,iofe),             &
                                           soilActOrgP(sl,iofe), soilStaOrgP(sl,iofe), uptakeLabileP(sl,iofe), plantP(iofe),      &
                                           runoffLabileP(iofe), sedimentMinP(iofe), sedimentOrgP(iofe), verticalLabileP(sl,iofe), &
                                           lateralLabileP(sl,iofe), tileDrainLabileP(iofe)

            ! Write variable data columns
            IF (numPest > 1) THEN
                DO iter = 1,(numPest-1)
                    ip = whichPest(iter)
                    WRITE (1111,3001,ADVANCE="no") sol_pst(sl,iofe,ip), dkg(ip,iofe), runoffPesticide(iofe,ip),                   &
                                                   verticalPesticide(sl,iofe,ip), lateralPesticide(sl,iofe,ip),                   &
                                                   tileDrainPesticide(iofe,ip)
                ENDDO
                ip = whichPest(numPest)
                WRITE (1111,3001) sol_pst(sl,iofe,ip), dkg(ip,iofe), runoffPesticide(iofe,ip), verticalPesticide(sl,iofe,ip),     &
                                  lateralPesticide(sl,iofe,ip), tileDrainPesticide(iofe,ip)
            ELSEIF (numPest == 1) THEN
                ip = whichPest(numPest)
                WRITE (1111,3001) sol_pst(sl,iofe,ip), dkg(ip,iofe), runoffPesticide(iofe,ip), verticalPesticide(sl,iofe,ip),     &
                                  lateralPesticide(sl,iofe,ip), tileDrainPesticide(iofe,ip)
            ELSEIF (numPest == 0) THEN
                WRITE (1111,3001) 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
            ENDIF

        !! Write the subsurface data for all soil layers
        ELSE

            ! Write fixed data columns
            WRITE (1111,4000,ADVANCE="no") calendarYear, calendarMonth, calendarDay, iofe, sl,                                    &
                                           lengthOFE(iofe), widthOFE(iofe), bottomDepth(sl,iofe),                                 &
                                           dryBulkDensity(sl,iofe)*1000.0, soilPorosity(sl,iofe), soilCarbon(sl,iofe),            &
                                           soilResidue(sl,iofe), soilWaterPA(sl,iofe),                                            &
                                           verticalFlow(sl,iofe), lateralFlow(sl,iofe), soilTemperature(sl,iofe),                 &
                                           sol_no3(sl,iofe), sol_nh4(sl,iofe), sol_fon(sl,iofe), sol_aorgn(sl,iofe),              &
                                           sol_orgn(sl,iofe), uptakeNO3(sl,iofe), denitrification(sl,iofe), verticalNO3(sl,iofe), &
                                           lateralNO3(sl,iofe), sol_labp(sl,iofe), sol_actp(sl,iofe), sol_stap(sl,iofe),          &
                                           sol_fop(sl,iofe), soilActOrgP(sl,iofe), soilStaOrgP(sl,iofe), uptakeLabileP(sl,iofe),  &
                                           verticalLabileP(sl,iofe), lateralLabileP(sl,iofe)

            ! Write variable data columns
            IF (numPest > 1) THEN
                DO iter = 1,(numPest-1)
                    ip = whichPest(iter)
                    WRITE (1111,4001,ADVANCE="no") sol_pst(sl,iofe,ip), verticalPesticide(sl,iofe,ip), lateralPesticide(sl,iofe,ip)
                ENDDO
                ip = whichPest(numPest)
                WRITE (1111,4001) sol_pst(sl,iofe,ip), verticalPesticide(sl,iofe,ip), lateralPesticide(sl,iofe,ip)
            ELSEIF (numPest == 1) THEN
                ip = whichPest(numPest)
                WRITE (1111,4001) sol_pst(sl,iofe,ip), verticalPesticide(sl,iofe,ip), lateralPesticide(sl,iofe,ip)
            ELSEIF (numPest == 0) THEN
                WRITE (1111,4001) 0.0, 0.0, 0.0
            ENDIF

        ENDIF
    ENDDO
    RETURN


    !!! -------------------- SPECIFY OUTPUT FORMATTING -------------------- !!!

    !! NOTES:
    !  + numbers beginning with 1 are related to the header (variable names)
    !  + numbers beginning with 2 are related to the header (width delimiters and units)
    !  + numbers beginning with 3 are data format statements for fixed (non-pesticide) data
    !  + numbers beginning with 4 are data format statements for repeating (pesticide) data
    !  + numbers ending in 000 are of a fixed nature (non-pesticide data)
    !  + numbers ending in 001 are of a repeating nature (pesticide data)

    !! SPACING:   4       8       6&6        13       15&15         11         9        15         18        15         24
    !                   9        17        16        15         18         21        14        11       12&12         10
    !              11&11&11         10        16        11       13&13         12        15  11&11&11&11        10        15
    !                 11&11        13&13         12        15
    !                  11        16        14        16        15         18

    1000 FORMAT ("YEAR   MONTH   DAY   OFE   SOIL-LAYER   OFE-LENGTH   OFE-WIDTH   BOTTOM-DEPTH   BULK-DENSITY   POROSITY   ",    &
                 "CARBON   SOIL-RESIDUE   SURFACE-RESIDUE   WATER-STRESS   PLANT-AVAILABLE-WATER   PRECIPITATION   RUNOFF   ",    &
                 "SEDIMENT-YIELD   SEDIMENT-CONCENTRATION   ENRICHMENT-RATIO   VERTICAL-FLOW   LATERAL-FLOW   TILE-DRAIN-FLOW   ",&
                 "TEMPERATURE-STRESS   TEMPERATURE   NITROGEN-STRESS   NITRATE-N   AMMONIUM-N   FRESH-N   ACTIVE-N   STABLE-N   ",&
                 "UPTAKE-N   PLANT-N   FIXED-N   DENITRIFIED-N   WET-DEPOSITED-NITRATE   DRY-DEPOSITED-NITRATE   ",               &
                 "WET-DEPOSITED-AMMONIUM   DRY-DEPOSITED-AMMONIUM   RUNOFF-N   SEDIMENT-N   VERTICAL-N   LATERAL-N   ",           &
                 "TILE-DRAIN-N   PHOSPHORUS-STRESS   LABILE-P   ACTIVE-MINERAL-P   STABLE-MINERAL-P   FRESH-ORGANIC-P   ",        &
                 "ACTIVE-ORGANIC-P   STABLE-ORGANIC-P   UPTAKE-P   PLANT-P   RUNOFF-P   SEDIMENT-MINERAL-P   ",                   &
                 "SEDIMENT-ORGANIC-P   VERTICAL-P   LATERAL-P   TILE-DRAIN-P")
    2000 FORMAT ("(#)-   -(#)-   (#)   (#)   ---(#)----   ---(m)----   ---(m)---   ----(mm)----   -(kg/m**3)--   -(0-1)--   ",    &
                 "-(%)--   --(kg/ha)---   ----(kg/ha)----   ---(0-1)----   --------(mm)---------   ----(mm)-----   -(mm)-   ",    &
                 "-----(kg)-----   ------(kg/m**3)-------   -----(none)-----   ----(mm)-----   ----(mm)----   -----(mm)------   ",&
                 "------(0-1)-------   ----(C)----   -----(0-1)-----   -(kg/ha)-   -(kg/ha)--   (kg/ha)   (kg/ha)-   (kg/ha)-   ",&
                 "(kg/ha)-   (kg/ha)   (kg/ha)   ---(kg/ha)---   -------(kg/ha)-------   -------(kg/ha)-------   ",               &
                 "-------(kg/ha)--------   -------(kg/ha)--------   (kg/ha)-   -(kg/ha)--   -(kg/ha)--   -(kg/ha)-   ",           &
                 "--(kg/ha)---   ------(0-1)------   (kg/ha)-   ----(kg/ha)-----   ----(kg/ha)-----   ----(kg/ha)----   ",        &
                 "----(kg/ha)-----   ----(kg/ha)-----   (kg/ha)-   (kg/ha)   (kg/ha)-   -----(kg/ha)------   ",                   &
                 "-----(kg/ha)------   -(kg/ha)--   -(kg/ha)-   --(kg/ha)---")
    3000 FORMAT (i4, 6x, i2, 2(4x, i2), 11x, i2, 5x, f8.3, 4x, f8.3, 2(7x, f8.3), 3x, f8.3, 1x, f8.3, 7x, f8.3, 8x, f10.3,        &
                 7x, f8.3, 16x, f8.3, 8x, f8.3, 1x, f8.3, 9x, f8.3, 17x, f8.3, 11x, f8.3, 8x, f8.3, 7x, f8.3, 10x, f8.3,          &
                 13x, f8.3, 6x, f8.3, 10x, f8.3, 4x, f8.3, 5x, f8.3, 2x, f8.3, 3(3x, f8.3), 2(2x, f8.3), 8x, f8.3, 2(16x, f8.3),  &
                 2(17x, f8.3), 3x, f8.3, 2(5x, f8.3), 4x, f8.3, 7x, f8.3, 12x, f8.3, 3x, f8.3, 2(11x, f8.3), 10x, f8.3,           &
                 2(11x, f8.3), 3x, f8.3, 2x, f8.3, 3x, f8.3, 2(13x, f8.3), 5x, f8.3, 4x, f8.3, 7x, f8.3)
    4000 FORMAT (i4, 6x, i2, 2(4x, i2), 11x, i2, 5x, f8.3, 4x, f8.3, 2(7x, f8.3), 3x, f8.3, 1x, f8.3, 7x, f8.3, 18x, 15x,         &
                 16x, f8.3, 16x, 9x, 17x, 25x, 19x, 8x, f8.3, 7x, f8.3, 18x, 21x, 6x, f8.3, 18x, 4x, f8.3, 5x, f8.3, 2x, f8.3,    &
                 2(3x, f8.3), 3x, f8.3, 10x, 10x, 8x, f8.3, 24x, 24x, 25x, 25x, 11x, 13x, 5x, f8.3, 4x, f8.3, 15x, 20x, 3x, f8.3, &
                 2(11x, f8.3), 10x, f8.3, 2(11x, f8.3), 3x, f8.3, 10x, 11x, 21x, 21x, 5x, f8.3, 4x, f8.3, 15x)

    1001 FORMAT ("   SOC-Pest   DEGRADED-Pest   RUNOFF-Pest   VERTICAL-Pest   LATERAL-Pest   TILE-DRAIN-Pest")
    2001 FORMAT ("   (kg/ha)-   ---(kg/ha)---   --(kg/ha)--   ---(kg/ha)---   --(kg/ha)---   ----(kg/ha)----")
    3001 FORMAT (3x, f8.3, 8x, f8.3, 6x, f8.3, 8x, f8.3, 7x, f8.3, 10x, f8.3)
    4001 FORMAT (3x, f8.3, 16x, 14x, 8x, f8.3, 7x, f8.3, 18x)

END SUBROUTINE
