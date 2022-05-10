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
!!    + corrected code to limit flux values by the appropriate pools when the fluxes are negative
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine computes phosphorus flux between the labile, active mineral, and stable mineral p pools.
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          	|units              |definition
!!    -----------------------------------------------------------------------
!!    betaActStaMinP	|1/day              |a rate constant controlling the movement of phosphorus from the active mineral to stable mineral pool
!!    LabToActMinP		|kg/ha				|amount of phosphorus moving from the labile mineral to the active mineral pool in the soil layer (negative means opposite)
!!    ActMinToStaMinP	|kg/ha				|amount of phosphorus moving from the active mineral to the stable mineral pool in the soil layer (negative means opposite)
!!    paiRatio			|none               |the phosphorus availability index ratio (calculated as PAI/(1-PAI))
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computePhosphorusMineralization

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: betaActStaMinP, paiRatio

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Phosphorus Mineralization"

    !! Define local variables
    betaActStaMinP = 0.0006
    IF (pai(iofe) < 1.0) paiRatio = pai(iofe) / (1.0 - pai(iofe))
	IF (pai(iofe) == 1.0) paiRatio = 0.0

    !! Compute changes by soil layer
    DO sl = 1,numsl

        !! Compute labile and active mineral phosphorus pool changes
        LabToActMinP = (sol_labp(sl,iofe) - sol_actp(sl,iofe) * paiRatio)
        IF (LabToActMinP > 0.0) LabToActMinP = LabToActMinP * 0.1
        IF (LabToActMinP < 0.0) LabToActMinP = LabToActMinP * 0.6
        LabToActMinP = Min(LabToActMinP, sol_labp(sl,iofe))
        LabToActMinP = Max(LabToActMinP, -sol_actp(sl,iofe))

        !! Compute active mineral and stable mineral phosphorus pool changes
        ActMinToStaMinP = betaActStaMinP * (4.0 * sol_actp(sl,iofe) - sol_stap(sl,iofe))
        IF (ActMinToStaMinP < 0.0) ActMinToStaMinP = ActMinToStaMinP * 0.1
        ActMinToStaMinP = Min(ActMinToStaMinP, sol_actp(sl,iofe))
        ActMinToStaMinP = Max(ActMinToStaMinP, -sol_stap(sl,iofe))

        !! Update phosphorus pools
        sol_stap(sl,iofe) = sol_stap(sl,iofe) + ActMinToStaMinP
        sol_actp(sl,iofe) = sol_actp(sl,iofe) - ActMinToStaMinP + LabToActMinP
        sol_labp(sl,iofe) = sol_labp(sl,iofe) - LabToActMinP

        !! Set pools to zero in case of rounding error causing slight negative values
        IF (sol_stap(sl,iofe) < 0.0) sol_stap(sl,iofe) = 0.0
        IF (sol_actp(sl,iofe) < 0.0) sol_actp(sl,iofe) = 0.0
        IF (sol_labp(sl,iofe) < 0.0) sol_labp(sl,iofe) = 0.0

    ENDDO
    RETURN
END
