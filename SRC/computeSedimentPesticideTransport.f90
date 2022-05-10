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
!!    This subroutine calculates pesticide transported with suspended sediment
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name              |units              |definition         |options
!!    -----------------------------------------------------------------------
!!    bottom            |mm                 |a temporary term for the equation divisor
!!    concentration     |g/Mg               |concentration of pesticide in soil
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeSedimentPesticideTransport

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: bottom, concentration

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Sediment Pesticide Transport"

    !! Compute for each active pesticide
    DO ip = 1,mxnp
        IF (pstflg(ip) > 0) THEN

            !! Compute pesticide in runoff/sediment mixture
            bottom = soilWaterST(1,iofe) + sol_kp(1,iofe,ip)*dryBulkDensity(1,iofe)*soilThickness(1,iofe)
            concentration = 100 * sol_kp(1,iofe,ip) * sol_pst(1,iofe,ip) / (bottom + 1.0e-10)
            sedimentPesticide(ip,iofe) = 0.001 * concentration * enrichmentRatio(iofe) * sedimentYieldOFE(iofe) / areaOFE(iofe)
            sedimentPesticide(ip,iofe) = Max(sedimentPesticide(ip,iofe), 0.0)

            !! Update pesticide amount
            IF (sedimentPesticide(ip,iofe) > sol_pst(1,iofe,ip)) THEN
                sedimentPesticide(ip,iofe) = sol_pst(1,iofe,ip)
                sol_pst(1,iofe,ip) = 0.0
            ENDIF

        ENDIF
    ENDDO

    RETURN
END
