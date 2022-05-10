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
!!    + the plant nutrient balances assume that the mass of N/P uptake does not change forms in the plant
!!      - i.e., the kg LabP/ha becomes kg P/ha
!!      - I believe this needs a molar mass conversion to various P forms in the plant and new variables to track those
!!      - OR an empirical conversion factor to account for this complexity
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates plant phosphorus uptake
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                  |units              |definition         |option
!!    -----------------------------------------------------------------------
!!    deficitP              |kg/ha              |amount of labile phosphorus required for optimal growth for the day
!!    luxuryP               |kg/ha              |amount of labile phosphorus required for luxury growth for the day
!!    plantP                |kg/ha              |amount of total phosphorus in the live plant material for the OFE for the day
!!    requiredUptakeP       |kg/ha              |amount of cumulative labile phosphorus required for optimal plant growth
!!    uptakeLabPmax         |kg/ha              |maximum amount of labile phosphorus that can be removed from soil layer for the day
!!    uptakeSum             |kg/ha              |amount of labile phosphorus that is removed from the soil profile (all layers)
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computePhosphorusUptake

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: deficitP, luxuryP, requiredUptakeP, uptakeLabPmax, uptakeSum

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Plant Phosphorus Uptake"

    !! Compute required and deficit phosphorus for the plant
    cpt(iofe) = (bp(1,icrop) - bp(3,icrop)) * (1.0 - phuacc(iofe) /                                                               &
                (phuacc(iofe) + Exp(bio_p1(icrop) - bio_p2(icrop) * phuacc(iofe)))) + bp(3,icrop)
    requiredUptakeP = cpt(iofe) * bio_ms(iofe)
    deficitP = requiredUptakeP - plantP(iofe)
    luxuryP = 1.5 * deficitP

    !! Compute the actual phosphorus uptake for the plant
    DO sl = 1,numsl
        uptakeLabPmax = luxuryP * (1.0 - Exp(-ubp(iofe) * bottomDepth(sl,iofe) / bottomDepth(numsl,iofe))) / uobp(iofe)
        uptakeLabileP(sl,iofe) = Max(uptakeLabPmax, 0.0)
        uptakeLabileP(sl,iofe) = Min(uptakeLabileP(sl,iofe), sol_labp(sl,iofe))
        sol_labp(sl,iofe) = sol_labp(sl,iofe) - uptakeLabileP(sl,iofe)
    ENDDO

    !! Compute phosphorus stress
    uptakeSum = Sum(uptakeLabileP(1:numsl,iofe))
    plantP(iofe) = plantP(iofe) + uptakeSum
    CALL computeNutrientStress(plantP(iofe), requiredUptakeP, "P")

    RETURN
END
