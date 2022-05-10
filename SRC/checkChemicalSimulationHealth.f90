
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2022-01        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + uses free-source-format (132 character maximum per line)
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine checks the most important WEPP-WQ arrays for validity and stops upon invalid values.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE checkChemicalSimulationHealth

    !! Load parameter module
    USE parm

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Checking Chemical Simulation Health"

    !! Reset the stop condition
    stopCondition = 1

    !!! -------------------- HANDLE SCALAR VALUES -------------------- !!!

    IF (iofe < 1) THEN
        WRITE(*,*) 'The variable "iofe" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (simulationYear < 0 ) THEN
        WRITE(*,*) 'The variable "simulationYear" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (simulationMonth < 1 .OR. simulationMonth > 12) THEN
        WRITE(*,*) 'The variable "simulationMonth" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (simulationDay < 1 .OR. simulationDay > 366) THEN
        WRITE(*,*) 'The variable "simulationDay" is invalid.'
        stopCondition = stopBehavior
    ENDIF


    !!! -------------------- HANDLE 1-D ARRAY VALUES -------------------- !!!

    IF (nFixation(iofe) < 0.0 .OR. ISNAN(nFixation(iofe))) THEN
        WRITE(*,*) 'A value in "nFixation" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (Wstress(iofe) < 0.0 .OR. ISNAN(Wstress(iofe))) THEN
        WRITE(*,*) 'A value in "Wstress" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (Tstress(iofe) < 0.0 .OR. ISNAN(Tstress(iofe))) THEN
        WRITE(*,*) 'A value in "Tstress" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (Nstress(iofe) < 0.0 .OR. ISNAN(Nstress(iofe))) THEN
        WRITE(*,*) 'A value in "Nstress" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (Pstress(iofe) < 0.0 .OR. ISNAN(Pstress(iofe))) THEN
        WRITE(*,*) 'A value in "Pstress" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (surfaceFlow(iofe) < 0.0 .OR. ISNAN(surfaceFlow(iofe))) THEN
        WRITE(*,*) 'A value in "surfaceFlow" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (sedimentYieldOFE(iofe) < 0.0 .OR. ISNAN(sedimentYieldOFE(iofe))) THEN
        WRITE(*,*) 'A value in "sedimentYieldOFE" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (runoffNO3(iofe) < 0.0 .OR. ISNAN(runoffNO3(iofe))) THEN
        WRITE(*,*) 'A value in "runoffNO3" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (sedimentOrgN(iofe) < 0.0 .OR. ISNAN(sedimentOrgN(iofe))) THEN
        WRITE(*,*) 'A value in "sedimentOrgN" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (runoffLabileP(iofe) < 0.0 .OR. ISNAN(runoffLabileP(iofe))) THEN
        WRITE(*,*) 'A value in "runoffLabileP" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (sedimentMinP(iofe) < 0.0 .OR. ISNAN(sedimentMinP(iofe))) THEN
        WRITE(*,*) 'A value in "sedimentMinP" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (sedimentOrgP(iofe) < 0.0 .OR. ISNAN(sedimentOrgP(iofe))) THEN
        WRITE(*,*) 'A value in "sedimentOrgP" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (tileDrainFlow(iofe) < 0.0 .OR. ISNAN(tileDrainFlow(iofe))) THEN
        WRITE(*,*) 'A value in "tileDrainFlow" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (tileDrainNO3(iofe) < 0.0 .OR. ISNAN(tileDrainNO3(iofe))) THEN
        WRITE(*,*) 'A value in "tileDrainNO3" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (tileDrainLabileP(iofe) < 0.0 .OR. ISNAN(tileDrainLabileP(iofe))) THEN
        WRITE(*,*) 'A value in "tileDrainLabileP" is invalid.'
        stopCondition = stopBehavior
    ENDIF

    IF (surfaceResidue(iofe) < 0.0 .OR. ISNAN(surfaceResidue(iofe))) THEN
        WRITE(*,*) 'A value in "surfaceResidue" is invalid.'
        stopCondition = stopBehavior
    ENDIF


    !!! -------------------- HANDLE 2-D ARRAY VALUES -------------------- !!!

    DO sl = 1,numsl
        IF (bottomDepth(sl,iofe) < 0.0 .OR. ISNAN(bottomDepth(sl,iofe))) THEN
            WRITE(*,*) 'A value in "bottomDepth" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (dryBulkDensity(sl,iofe) < 0.0 .OR. ISNAN(dryBulkDensity(sl,iofe))) THEN
            WRITE(*,*) 'A value in "dryBulkDensity" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (soilPorosity(sl,iofe) < 0.0 .OR. ISNAN(soilPorosity(sl,iofe))) THEN
            WRITE(*,*) 'A value in "soilPorosity" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (soilCarbon(sl,iofe) < 0.0 .OR. ISNAN(soilCarbon(sl,iofe))) THEN
            WRITE(*,*) 'A value in "soilCarbon" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (soilResidue(sl,iofe) < 0.0 .OR. ISNAN(soilResidue(sl,iofe))) THEN
            WRITE(*,*) 'A value in "soilResidue" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (soilTemperature(sl,iofe) < 0.0 .OR. ISNAN(soilTemperature(sl,iofe))) THEN
            WRITE(*,*) 'A value in "soilTemperature" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (soilWaterPA(sl,iofe) < 0.0 .OR. ISNAN(soilWaterPA(sl,iofe))) THEN
            WRITE(*,*) 'A value in "soilWaterPA" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (verticalFlow(sl,iofe) < 0.0 .OR. ISNAN(verticalFlow(sl,iofe))) THEN
            WRITE(*,*) 'A value in "verticalFlow" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (lateralFlow(sl,iofe) < 0.0 .OR. ISNAN(lateralFlow(sl,iofe))) THEN
            WRITE(*,*) 'A value in "lateralFlow" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (sol_no3(sl,iofe) < 0.0 .OR. ISNAN(sol_no3(sl,iofe))) THEN
            WRITE(*,*) 'A value in "sol_no3" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (sol_nh4(sl,iofe) < 0.0 .OR. ISNAN(sol_nh4(sl,iofe))) THEN
            WRITE(*,*) 'A value in "sol_nh4" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (sol_fon(sl,iofe) < 0.0 .OR. ISNAN(sol_fon(sl,iofe))) THEN
            WRITE(*,*) 'A value in "sol_fon" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (sol_aorgn(sl,iofe) < 0.0 .OR. ISNAN(sol_aorgn(sl,iofe))) THEN
            WRITE(*,*) 'A value in "sol_aorgn" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (sol_orgn(sl,iofe) < 0.0 .OR. ISNAN(sol_orgn(sl,iofe))) THEN
            WRITE(*,*) 'A value in "sol_orgn" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (uptakeNO3(sl,iofe) < 0.0 .OR. ISNAN(uptakeNO3(sl,iofe))) THEN
            WRITE(*,*) 'A value in "uptakeNO3" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (denitrification(sl,iofe) < 0.0 .OR. ISNAN(denitrification(sl,iofe))) THEN
            WRITE(*,*) 'A value in "denitrification" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (verticalNO3(sl,iofe) < 0.0 .OR. ISNAN(verticalNO3(sl,iofe))) THEN
            WRITE(*,*) 'A value in "verticalNO3" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (lateralNO3(sl,iofe) < 0.0 .OR. ISNAN(lateralNO3(sl,iofe))) THEN
            WRITE(*,*) 'A value in "lateralNO3" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (uptakeLabileP(sl,iofe) < 0.0 .OR. ISNAN(uptakeLabileP(sl,iofe))) THEN
            WRITE(*,*) 'A value in "uptakeLabileP" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (sol_labp(sl,iofe) < 0.0 .OR. ISNAN(sol_labp(sl,iofe))) THEN
            WRITE(*,*) 'A value in "sol_labp" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (sol_actp(sl,iofe) < 0.0 .OR. ISNAN(sol_actp(sl,iofe))) THEN
            WRITE(*,*) 'A value in "sol_actp" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (sol_stap(sl,iofe) < 0.0 .OR. ISNAN(sol_stap(sl,iofe))) THEN
            WRITE(*,*) 'A value in "sol_stap" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (sol_fop(sl,iofe) < 0.0 .OR. ISNAN(sol_fop(sl,iofe))) THEN
            WRITE(*,*) 'A value in "sol_fop" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (soilActOrgP(sl,iofe) < 0.0 .OR. ISNAN(soilActOrgP(sl,iofe))) THEN
            WRITE(*,*) 'A value in "soilActOrgP" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (soilStaOrgP(sl,iofe) < 0.0 .OR. ISNAN(soilStaOrgP(sl,iofe))) THEN
            WRITE(*,*) 'A value in "soilStaOrgP" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (verticalLabileP(sl,iofe) < 0.0 .OR. ISNAN(verticalLabileP(sl,iofe))) THEN
            WRITE(*,*) 'A value in "verticalLabileP" is invalid.'
            stopCondition = stopBehavior
        ENDIF

        IF (lateralLabileP(sl,iofe) < 0.0 .OR. ISNAN(lateralLabileP(sl,iofe))) THEN
            WRITE(*,*) 'A value in "lateralLabileP" is invalid.'
            stopCondition = stopBehavior
        ENDIF

    ENDDO

END SUBROUTINE
