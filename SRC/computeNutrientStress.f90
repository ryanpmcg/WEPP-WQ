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
!!    This function calculates the plant stress factor caused by limited supply of nitrogen or phosphorus
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          |units              |definition
!!    -----------------------------------------------------------------------
!!    u1            |kg/ha              |actual amount of element in plant
!!    u2            |kg/ha              |optimal amount of element in plant
!!    uu            |none               |fraction of optimal plant growth achieved where reduction is caused by plant element deficiency
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeNutrientStress(uptakeSum, requiredUptake, nutrient)

    !! Load parameter module
    USE PARM

    !! Declare local variables
    IMPLICIT NONE
    CHARACTER (LEN=1) :: nutrient, Nchar, Pchar
    REAL, INTENT (IN) :: uptakeSum, requiredUptake
    REAL :: stress

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Computing Nutrient Stress"

    !! Define local variables
    Nchar = "N"
    Pchar = "P"
    stress = 200.0 * (uptakeSum/(requiredUptake + 0.0001) - 0.5)

    !! Compute new stress factor
    IF (stress <= 0.0) THEN
        stress = 0.0
    ELSE
        IF (stress < 99.0) THEN
            stress = stress / (stress + Exp(3.535 - 0.02597 * stress))
        ELSE
            stress = 1.0
        ENDIF
    ENDIF

    !! Set growth achieved to 100% when optimal amount is trace (precision not supported by this program)
    IF (requiredUptake <= 1.0e-6) stress = 1.0

    !! Update the correct stress factor
    IF (nutrient == Nchar) Nstress = stress
    IF (nutrient == Pchar) Pstress = stress

    RETURN

END SUBROUTINE
