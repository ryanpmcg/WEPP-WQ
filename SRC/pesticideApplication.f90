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
!!    This subroutine applies pesticide
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                  |units              |definition
!!    -----------------------------------------------------------------------
!!    gc                    |none               |fraction of ground covered by plant foliage
!!    pesticideID           |none               |pesticide identification number from pest.dat
!!    deliveredPesticide    |kg/ha              |amount of pesticide applied to OFE
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE pesticideApplication

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: a, b, c, ip, pesticideID
    REAL :: deliveredPesticide, gc, erfc

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Simulating Pesticide Application"

    !! Define local variables
    a = simulationYear  !sequence number of year in rotation
    b = nfert(iofe)     !sequence number of fertilizer application within the year
    c = (iofe)          !OFE
    pesticideID = ipest(a,b,c)
    ip = pstflg(pesticideID)
    deliveredPesticide = pst_kg(a,b,c) * ap_ef(pesticideID)

    !! Calculate ground cover
    gc = (1.99532 - erfc(1.333 * leafAreaIndex(iofe) - 2.0)) / 2.1
    IF (gc < 0.0) gc = 0.0

    !! Update pesticide levels on ground and foliage
    plt_pst(ip,iofe) = plt_pst(ip,iofe) + gc * deliveredPesticide
    sol_pst(1,ofe,ip) = sol_pst(1,ofe,ip) + (1.0 - gc) * deliveredPesticide

    !! Perform summary calculations
    pab(ip) = pab(ip) + pst_kg(a,b,c) * ap_ef(pesticideID) * hru_fr(iofe)

    !! Update sequence number for pesticide application
    npest(iofe) = npest(iofe) + 1

    RETURN
END
