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
!!    This subroutine calculates the amount of pesticide washed off the plant foliage and onto the soil
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computePesticideWashoff

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: washoffPesticide

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Pesticide Washoff"

    !! Compute washoff for each pesticide
    DO ip = 1,mxnp
        IF (pstflg(ip) > 0) THEN
            washoffPesticide = wof(ip) * plt_pst(ip,iofe)
            IF (washoffPesticide < 1.0e-06) washoffPesticide = 0.0
            IF (washoffPesticide > plt_pst(ip,iofe)) washoffPesticide = plt_pst(ip,iofe)
            sol_pst(1,iofe,ip) = sol_pst(1,iofe,ip) + washoffPesticide
            plt_pst(ip,iofe) = plt_pst(ip,iofe) - washoffPesticide
        ENDIF
    ENDDO

    RETURN
END SUBROUTINE
