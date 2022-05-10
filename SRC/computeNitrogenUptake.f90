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
!!      - i.e., the kg NO3/ha becomes kg N/ha, which does not seem right to me
!!      - I believe this needs a molar mass conversion to various N forms in the plant and new variables to track those
!!      - OR an empirical conversion factor to account for this complexity
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates plant nitrogen uptake
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                  |units              |definition         |option
!!    -----------------------------------------------------------------------
!!    deficitN              |kg/ha              |amount of nitrate required for optimal growth for the day
!!    plantN                |kg/ha              |amount of total nitrogen in the live plant material for the OFE for the day
!!    requiredUptakeN       |kg/ha              |amount of cumulative nitrate required for optimal plant growth
!!    uptakeNO3max          |kg/ha              |maximum amount of nitrate that can be removed from soil layer for the day
!!    uptakeSum             |kg/ha              |amount of nitrate that is removed from the soil profile (all layers)
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeNitrogenUptake

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: deficitN, requiredUptakeN, uptakeNO3max, uptakeSum

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Plant Nitrogen Uptake"

    !! Compute required and deficit nitrogen for the plant
    cnb(iofe) = (bn(1,icrop) - bn(3,icrop)) * (1.0 - phuacc(iofe) / (phuacc(iofe) +                                               &
                Exp(bio_n1(icrop) - bio_n2(icrop) * phuacc(iofe)))) + bn(3,icrop)
    requiredUptakeN = cnb(iofe) * bio_ms(iofe)
    deficitN = requiredUptakeN - plantN(iofe)

    !! Compute the actual nitrogen uptake for the plant
    DO sl = 1,numsl
        uptakeNO3max = deficitN * (1.0 - Exp(-ubn(iofe) * bottomDepth(sl,iofe) / bottomDepth(numsl,iofe))) / uobn(iofe)
        uptakeNO3(sl,iofe) = Max(uptakeNO3max, 0.0)
        uptakeNO3(sl,iofe) = Min(uptakeNO3(sl,iofe), sol_no3(sl,iofe))
        sol_no3(sl,iofe) = sol_no3(sl,iofe) - uptakeNO3(sl,iofe)
    ENDDO

    !! If crop is a legume, call nitrogen fixation routine and update the uptake budget
    SELECT CASE (idc(icrop))
        CASE (1,2,3)
            CALL computeNitrogenFixation
            Nstress = 1.0
			uptakeNO3(1,iofe) = uptakeNO3(1,iofe) + nFixation(iofe)
			plantN(iofe) = plantN(iofe) + nFixation(iofe)
        CASE DEFAULT
            nFixation(iofe) = 0.0
            uptakeSum = Sum(uptakeNO3(1:numsl,iofe))
            plantN(iofe) = plantN(iofe) + uptakeSum
            CALL computeNutrientStress(plantN(iofe), requiredUptakeN, "N")
    END SELECT

    RETURN

END SUBROUTINE
