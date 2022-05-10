!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2021-12        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates the amount of organic nitrogen removed as sediment in surface runoff
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                  |units              |definition         |option
!!    -----------------------------------------------------------------------
!!    amount                |kg/ha              |a temporary term for updating N pools
!!    concentrationOrgN     |g/Mg               |concentration of organic N in soil
!!    convFactor            |none               |conversion factor (g/Mg => kg/ha)
!!    totalOrgN             |kg/ha              |sum of organic nitrogen pools (fresh, active, and stable)
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeSedimentNitrogenTransport

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: amount, concentrationOrgN, convFactor, totalOrgN

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Sediment Nitrogen Transport"

    !! Compute organic nitrogen moved as sediment (in runoff)
    totalOrgN = sol_orgn(1,iofe) + sol_aorgn(1,iofe) + sol_fon(1,iofe)
    convFactor = dryBulkDensity(1,iofe) * bottomDepth(1,iofe) / 100.0
    concentrationOrgN = totalOrgN / convFactor
    sedimentOrgN(iofe) = 0.001 * concentrationOrgN * enrichmentRatio(iofe) * sedimentYieldOFE(iofe) / areaOFE(iofe)

    !! Update soil nitrogen pools
    IF (totalOrgN > 0.0) THEN

        ! Active organic nitrogen
        amount = sedimentOrgN(iofe) * (sol_aorgn(1,iofe) / totalOrgN)
        IF (amount >= sol_aorgn(1,iofe)) THEN
            sol_aorgn(1,iofe) = 0.0
        ELSE
            sol_aorgn(1,iofe) = sol_aorgn(1,iofe) - amount
        END IF

        ! Stable organic nitrogen
        amount = sedimentOrgN(iofe) * (sol_orgn(1,iofe) / totalOrgN)
        IF (amount >= sol_orgn(1,iofe)) THEN
            sol_orgn(1,iofe) = 0.0
        ELSE
            sol_orgn(1,iofe) = sol_orgn(1,iofe) - amount
        END IF

        ! Fresh organic nitrogen
        amount = sedimentOrgN(iofe) * (sol_fon(1,iofe) / totalOrgN)
        IF (amount >= sol_fon(1,iofe)) THEN
            sol_fon(1,iofe) = 0.0
        ELSE
            sol_fon(1,iofe) = sol_fon(1,iofe) - amount
        END IF

    ENDIF
    RETURN
END
