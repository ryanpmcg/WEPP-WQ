!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2021-08        |Ryan McGehee (original code)
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + uses free source format
!!    + added new deposition options (linear and explicit)
!!    + added ammonium wet deposition
!!    + added dry deposition for NO3 and NH4
!!    + refactored code, updated documentation, added comments
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds nitrate from rainfall to the soil profile
!!
!!    ~ ~ ~ ASSUMPTIONS ~ ~ ~
!!    + rainfall nitrogen concentration is spatially uniform
!!    + rainfall nitrogen concentration changes temporally with annual granularity
!!    + unspecified wet deposition method results in no wet atmospheric deposition
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeAtmosphericDeposition

    !! Load parameter module
    USE parm

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Atmospheric Deposition"

    !! Determine rcn based on user-specified input (AtmosDepMethod)
    IF (wetDepositionMethod.EQ.1) THEN
        precipConcNO3 = pcNO3
        precipConcNH4 = pcNH4
    ELSEIF (wetDepositionMethod.EQ.2) THEN
        precipConcNO3 = pcNO3i + (pcNO3f - pcNO3i) * (simulationYear/totyr)
        precipConcNH4 = pcNH4i + (pcNH4f - pcNH4i) * (simulationYear/totyr)
    ELSEIF (wetDepositionMethod.EQ.3) THEN
        precipConcNO3 = pcNO3arr(simulationYear)
        precipConcNH4 = pcNH4arr(simulationYear)
    ELSE
        precipConcNO3 = 0.0
        precipConcNH4 = 0.0
    ENDIF

    !! Calculate nitrate and ammonium wet and dry deposition and add to soil surface layer
    wetDepositionNO3 = 0.01 * precipConcNO3 * precipitation                                             ! Convert units from (mg/L)*(mm) to kg/ha
    wetDepositionNH4 = 0.01 * precipConcNH4 * precipitation                                             ! Convert units from (mg/L)*(mm) to kg/ha
    sol_no3(1,iofe) = sol_no3(1,iofe) + wetDepositionNO3
    sol_no3(1,iofe) = sol_no3(1,iofe) + dryDepositionNO3
    sol_nh4(1,iofe) = sol_nh4(1,iofe) + wetDepositionNH4
    sol_nh4(1,iofe) = sol_nh4(1,iofe) + dryDepositionNH4

    RETURN
END
