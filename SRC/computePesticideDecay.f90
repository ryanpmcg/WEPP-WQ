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
!!    This subroutine calculates degradation of pesticide in the soil and on the plants
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          |units              |definition
!!    -----------------------------------------------------------------------
!!    k             |none               |pesticide counter
!!    kk            |none               |pesticide reference number (from database)
!!    x1            |kg/ha              |amount of pesticide present at beginning of day
!!    xx            |kg/ha              |amount of pesticide present at end of day
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computePesticideDecay

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: k, kk
    REAL :: x1, xx

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Pesticide Decay"

    !! COMMENT NEEDED
    DO k = 1,mxnp
        kk = 0
        kk = pstflg(k)
        IF (kk > 0) THEN

            !! Calculate degradation in soil
            DO sl = 1,numsl
                x1 = 0.0
                x1 = sol_pst(sl,iofe,k)

                !! COMMENT NEEDED
                IF (x1 >= 1.e-12) THEN
                    xx = 0.0
                    xx = x1 * decay_s(kk)
                    dkg(k,iofe) = dkg(k,iofe) + x1 - xx
                    pdb(k) = pdb(k) + (x1 - xx) * hru_fr(iofe)
                    sol_pst(sl,iofe,k) = xx
                ENDIF
            ENDDO

            !! Calculate degradation off plant foliage
            x1 = 0.0
            x1 = plt_pst(k,iofe)

            !! COMMENT NEEDED
            IF (x1 >= 1.e-12) THEN
                xx = 0.0
                xx = x1 * decay_f(kk)
                dkf(k,iofe) = dkf(k,iofe) + x1 - xx
                pdb(k) = pdb(k) + (x1 - xx) * hru_fr(iofe)
                plt_pst(k,iofe) = xx
            ENDIF
        ENDIF
    ENDDO
    RETURN
END
