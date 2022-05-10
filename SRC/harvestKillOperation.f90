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
!!    + new nutrient budgeting variables and conventions have been incorporated for:
!!        - incremental and cumulative plant nutrient content
!!        - total nutrient content in yields
!!        - OFE specific results
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine performs the harvest and kill operations
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE harvestKillOperation

    !! Load parameter module
    USE parm

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Simulating Harvest/Kill Operation"

    !! Check if yield is from above or below ground
    IF (HINDX > 1.001) THEN
        yield(iofe) = dryBioMass(iofe) * (1.0 - 1.0 / (1.0 + HINDX)) ! This is different from SWAT code
        newResidue(iofe) = dryBioMass(iofe) * (1.0 + HINDX)
    ELSE
        yield(iofe) = dryBioMass(iofe) * HINDX
        newResidue(iofe) = (1.0 - HINDX) * dryBioMass(iofe)
    ENDIF

    !! Set to zero if negative
    IF (yield(iofe) < 0.0) yield(iofe) = 0.0
    IF (newResidue(iofe) < 0.0) newResidue(iofe) = 0.0

    !! Update residue on soil surface
    soilResidue(1,iofe) = newResidue(iofe) + soilResidue(1,iofe)
    soilResidue(1,iofe) = Max(soilResidue(1,iofe),0.0)

    !! Calculate nutrients removed with yield
    yieldN(iofe) = yield(iofe) * cnyld(icrop)
    yieldP(iofe) = yield(iofe) * cpyld(icrop)
    yieldN(iofe) = Min(yieldN(iofe), 0.8*plantN(iofe)) ! 0.8 is added according to the 2012 version SWAT code
    yieldP(iofe) = Min(yieldP(iofe), 0.8*plantP(iofe)) ! 0.8 is added according to the 2012 version SWAT code

    !! Update fresh organic nutrient pools if needed
    IF (plantN(iofe) > yieldN(iofe)) sol_fon(1,iofe) = sol_fon(1,iofe) + plantN(iofe) - yieldN(iofe)
    IF (plantP(iofe) > yieldP(iofe)) sol_fop(1,iofe) = sol_fop(1,iofe) + plantP(iofe) - yieldP(iofe)

    !! Reset variables
    bio_ms(iofe) = 0.0
    plantN = 0.0
    plantP = 0.0
    leafAreaIndex(iofe) = 0.0
    phuacc(iofe) = 0.0

    RETURN
END
