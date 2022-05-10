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
!!    This subroutine calculates the amount of organic and mineral phosphorus attached to sediment in surface runoff
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name              |units              |definition         |options
!!    -----------------------------------------------------------------------
!!    availableP        |kg/ha              |amount of phosphorus sorbed (adsorbed or absorbed) in the top soil layer
!!    concentration     |g/Mg               |concentration of P in soil
!!    convFactor        |none               |conversion factor (mg/kg => kg/ha)
!!    mineralP          |kg/ha              |amount of mineral phosphorus sorbed to sediment in surface runoff in OFE for day
!!    organicP          |kg/ha              |amount of organic phosphorus in sediment in surface runoff in OFE for the day
!!    orgFraction       |kg/ha              |fraction of organic phosphorus in soil
!!    sedimentP         |kg/ha              |total amount of P removed in sediment erosion
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine computeSedimentPhosphorusTransport

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: availableP, convFactor, concentration, mineralP, organicP, orgFraction, sedimentP

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Sediment Phosphorus Transport"


    !!! -------------------- COMPUTE SEDIMENT PHOSPHORUS POTENTIAL -------------------- !!!

    availableP = soilActOrgP(1,iofe) + soilStaOrgP(1,iofe) + sol_fop(1,iofe) + sol_actp(1,iofe) + sol_stap(1,iofe)
    orgFraction = (soilActOrgP(1,iofe) + soilStaOrgP(1,iofe) + sol_fop(1,iofe)) / availableP
    convFactor = dryBulkDensity(1,iofe) * bottomDepth(1,iofe) / 100.0
    concentration = availableP / convFactor
    sedimentP = 0.001 * concentration * enrichmentRatio(iofe) * sedimentYieldOFE(iofe) / areaOFE(iofe)
    sedimentOrgP(iofe) = sedimentP * orgFraction
    sedimentMinP(iofe) = sedimentP * (1.0 - orgFraction)


    !!! -------------------- UPDATE PHOSPHORUS POOLS -------------------- !!!

    mineralP = sol_actp(1,iofe) + sol_stap(1,iofe)
    organicP = soilActOrgP(1,iofe) + soilStaOrgP(1,iofe) + sol_fop(1,iofe)

    ! Active organic pool
    amount = sedimentOrgP(iofe) * (soilActOrgP(1,iofe) / organicP)
    soilActOrgP(1,iofe) = soilActOrgP(1,iofe) - amount

    ! Stable organic pool
    amount = sedimentOrgP(iofe) * (soilStaOrgP(1,iofe) / organicP)
    soilStaOrgP(1,iofe) = soilStaOrgP(1,iofe) - amount

    ! Fresh organic pool
    amount = sedimentOrgP(iofe) * (sol_fop(1,iofe) / organicP)
    sol_fop(1,iofe) = sol_fop(1,iofe) - amount

    ! Active mineral pool
    amount = sedimentMinP(iofe) * (sol_actp(1,iofe) / mineralP)
    sol_actp(1,iofe) = sol_actp(1,iofe) - amount

    ! Stable mineral pool
    amount = sedimentMinP(iofe) * (sol_stap(1,iofe) / mineralP)
    sol_stap(1,iofe) = sol_stap(1,iofe) - amount


    !!! -------------------- BALANCE PHOSPHORUS POOLS AND RECOMPUTE -------------------- !!!

    ! Active organic pool
    IF (soilActOrgP(1,iofe) < 0.0) THEN
        sedimentOrgP(iofe) = sedimentOrgP(iofe) + soilActOrgP(1,iofe)
        soilActOrgP(1,iofe) = 0.0
    END IF

    ! Stable organic pool
    IF (soilStaOrgP(1,iofe) < 0.0) THEN
        sedimentOrgP(iofe) = sedimentOrgP(iofe) + soilStaOrgP(1,iofe)
        soilStaOrgP(1,iofe) = 0.0
    END IF

    ! Fresh organic pool
    IF (sol_fop(1,iofe) < 0.0) THEN
        sedimentOrgP(iofe) = sedimentOrgP(iofe) + sol_fop(1,iofe)
        sol_fop(1,iofe) = 0.0
    END IF

    ! Active mineral pool
    IF (sol_actp(1,iofe) < 0.0) THEN
        sedimentMinP(iofe) = sedimentMinP(iofe) + sol_actp(1,iofe)
        sol_actp(1,iofe) = 0.0
    END IF

    ! Stable mineral pool
    IF (sol_stap(1,iofe) < 0.0) THEN
        sedimentMinP(iofe) = sedimentMinP(iofe) + sol_stap(1,iofe)
        sol_stap(1,iofe) = 0.0
    END IF


    !!! -------------------- END CALCULATIONS -------------------- !!!

    RETURN
END
