
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
!!    + search 'stopBehavior' to control how invalid values are handled in the simulation
!!    + this only 'repairs' some values, and it does so rather crudely
!!    + more variables can be supported over time with more intelligent repair mechanisms
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine attempts to fix the most important WEPP-WQ arrays as non-intrusively as possible.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE repairChemicalSimulation

    !! Load parameter module
    USE parm

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Repairing Chemical Simulation"

    !! Reset the stop condition
    stopCondition = 0

    !!! -------------------- HANDLE SCALAR VALUES -------------------- !!!

    IF (iofe < 1)                                           STOP 'The variable "iofe" cannot be repaired.'
    IF (simulationYear < 0 )                                STOP 'The variable "simulationYear" cannot be repaired.'
    IF (simulationMonth < 1 .OR. simulationMonth > 12)      STOP 'The variable "simulationMonth" cannot be repaired.'
    IF (simulationDay < 1 .OR. simulationDay > 366)         STOP 'The variable "simulationDay" cannot be repaired.'


    !!! -------------------- HANDLE 1-D ARRAY VALUES -------------------- !!!

    IF (nFixation(iofe) < 0.0 .OR. ISNAN(nFixation(iofe)))					nFixation(iofe) = 0.0
    IF (Wstress(iofe) < 0.0 .OR. ISNAN(Wstress(iofe)))                      Wstress(iofe) = 0.0
    IF (Tstress(iofe) < 0.0 .OR. ISNAN(Tstress(iofe)))                      Tstress(iofe) = 0.0
    IF (Nstress(iofe) < 0.0 .OR. ISNAN(Nstress(iofe)))                      Nstress(iofe) = 0.0
    IF (Pstress(iofe) < 0.0 .OR. ISNAN(Pstress(iofe)))                      Pstress(iofe) = 0.0
    IF (surfaceFlow(iofe) < 0.0 .OR. ISNAN(surfaceFlow(iofe)))              surfaceFlow(iofe) = 0.0
    IF (sedimentYieldOFE(iofe) < 0.0 .OR. ISNAN(sedimentYieldOFE(iofe)))    sedimentYieldOFE(iofe) = 0.0
    IF (runoffNO3(iofe) < 0.0 .OR. ISNAN(runoffNO3(iofe)))                  runoffNO3(iofe) = 0.0
    IF (sedimentOrgN(iofe) < 0.0 .OR. ISNAN(sedimentOrgN(iofe)))            sedimentOrgN(iofe) = 0.0
    IF (runoffLabileP(iofe) < 0.0 .OR. ISNAN(runoffLabileP(iofe)))          runoffLabileP(iofe) = 0.0
    IF (sedimentMinP(iofe) < 0.0 .OR. ISNAN(sedimentMinP(iofe)))            sedimentMinP(iofe) = 0.0
    IF (sedimentOrgP(iofe) < 0.0 .OR. ISNAN(sedimentOrgP(iofe)))            sedimentOrgP(iofe) = 0.0
    IF (tileDrainFlow(iofe) < 0.0 .OR. ISNAN(tileDrainFlow(iofe)))          tileDrainFlow(iofe) = 0.0
    IF (tileDrainNO3(iofe) < 0.0 .OR. ISNAN(tileDrainNO3(iofe)))            tileDrainNO3(iofe) = 0.0
    IF (tileDrainLabileP(iofe) < 0.0 .OR. ISNAN(tileDrainLabileP(iofe)))    tileDrainLabileP(iofe) = 0.0
    IF (surfaceResidue(iofe) < 0.0 .OR. ISNAN(surfaceResidue(iofe)))        surfaceResidue(iofe) = 0.0


    !!! -------------------- HANDLE 2-D ARRAY VALUES -------------------- !!!

    DO sl = 1,(numsl-1)
        IF (bottomDepth(sl,iofe) < 0.0 .OR. ISNAN(bottomDepth(sl,iofe)))            bottomDepth(sl,iofe) = bottomDepth(sl,iofe+1)
        IF (dryBulkDensity(sl,iofe) < 0.0 .OR. ISNAN(dryBulkDensity(sl,iofe)))      dryBulkDensity(sl,iofe) = 1.26
        IF (soilPorosity(sl,iofe) < 0.0 .OR. ISNAN(soilPorosity(sl,iofe)))          soilPorosity(sl,iofe) = soilPorosity(sl+1,iofe)
        IF (soilCarbon(sl,iofe) < 0.0 .OR. ISNAN(soilCarbon(sl,iofe)))              soilCarbon(sl,iofe) = soilCarbon(sl+1,iofe)
        IF (soilResidue(sl,iofe) < 0.0 .OR. ISNAN(soilResidue(sl,iofe)))            soilResidue(sl,iofe) = soilResidue(sl+1,iofe)
        IF (soilTemperature(sl,iofe) < 0.0 .OR. ISNAN(soilTemperature(sl,iofe)))    soilTemperature(sl,iofe) = avgAnnualAirTemp
        IF (soilWaterPA(sl,iofe) < 0.0 .OR. ISNAN(soilWaterPA(sl,iofe)))            soilWaterPA(sl,iofe) = soilWaterPA(sl+1,iofe)
        IF (verticalFlow(sl,iofe) < 0.0 .OR. ISNAN(verticalFlow(sl,iofe)))          verticalFlow(sl,iofe) = 0.0
        IF (lateralFlow(sl,iofe) < 0.0 .OR. ISNAN(lateralFlow(sl,iofe)))            lateralFlow(sl,iofe) = 0.0
        IF (sol_no3(sl,iofe) < 0.0 .OR. ISNAN(sol_no3(sl,iofe)))                    sol_no3(sl,iofe) = sol_no3(sl+1,iofe)
        IF (sol_nh4(sl,iofe) < 0.0 .OR. ISNAN(sol_nh4(sl,iofe)))                    sol_nh4(sl,iofe) = sol_nh4(sl+1,iofe)
        IF (sol_fon(sl,iofe) < 0.0 .OR. ISNAN(sol_fon(sl,iofe)))                    sol_fon(sl,iofe) = sol_fon(sl+1,iofe)
        IF (sol_aorgn(sl,iofe) < 0.0 .OR. ISNAN(sol_aorgn(sl,iofe)))                sol_aorgn(sl,iofe) = sol_aorgn(sl+1,iofe)
        IF (sol_orgn(sl,iofe) < 0.0 .OR. ISNAN(sol_orgn(sl,iofe)))                  sol_orgn(sl,iofe) = sol_orgn(sl+1,iofe)
        IF (uptakeNO3(sl,iofe) < 0.0 .OR. ISNAN(uptakeNO3(sl,iofe)))                uptakeNO3(sl,iofe) = 0.0
        IF (denitrification(sl,iofe) < 0.0 .OR. ISNAN(denitrification(sl,iofe)))    denitrification(sl,iofe) = 0.0
        IF (verticalNO3(sl,iofe) < 0.0 .OR. ISNAN(verticalNO3(sl,iofe)))            verticalNO3(sl,iofe) = 0.0
        IF (lateralNO3(sl,iofe) < 0.0 .OR. ISNAN(lateralNO3(sl,iofe)))              lateralNO3(sl,iofe) = 0.0
        IF (uptakeLabileP(sl,iofe) < 0.0 .OR. ISNAN(uptakeLabileP(sl,iofe)))        uptakeLabileP(sl,iofe) = 0.0
        IF (sol_labp(sl,iofe) < 0.0 .OR. ISNAN(sol_labp(sl,iofe)))                  sol_labp(sl,iofe) = sol_labp(sl+1,iofe)
        IF (sol_actp(sl,iofe) < 0.0 .OR. ISNAN(sol_actp(sl,iofe)))                  sol_actp(sl,iofe) = sol_actp(sl+1,iofe)
        IF (sol_stap(sl,iofe) < 0.0 .OR. ISNAN(sol_stap(sl,iofe)))                  sol_stap(sl,iofe) = sol_stap(sl+1,iofe)
        IF (sol_fop(sl,iofe) < 0.0 .OR. ISNAN(sol_fop(sl,iofe)))                    sol_fop(sl,iofe) = sol_fop(sl+1,iofe)
        IF (soilActOrgP(sl,iofe) < 0.0 .OR. ISNAN(soilActOrgP(sl,iofe)))            soilActOrgP(sl,iofe) = soilActOrgP(sl+1,iofe)
        IF (soilStaOrgP(sl,iofe) < 0.0 .OR. ISNAN(soilStaOrgP(sl,iofe)))            soilStaOrgP(sl,iofe) = soilStaOrgP(sl+1,iofe)
        IF (verticalLabileP(sl,iofe) < 0.0 .OR. ISNAN(verticalLabileP(sl,iofe)))    verticalLabileP(sl,iofe) = 0.0
        IF (lateralLabileP(sl,iofe) < 0.0 .OR. ISNAN(lateralLabileP(sl,iofe)))      lateralLabileP(sl,iofe) = 0.0
    ENDDO

END SUBROUTINE

