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
!!    + local variables are self-describing (not needing documentation)
!!    + reference SWAT 2009 5.2 for fixation equations of which this is an exact copy
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine estimates nitrogen fixation by legumes
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeNitrogenFixation

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: requiredUptakeN, soilWaterFactor, profileNO3, NO3Factor, growthFactor, fixationFactor

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Computing Nitrogen Fixation"

    !! Handle NO3 supply and demand
    IF (deficitN > Sum(uptakeNO3(1:numsl,iofe))) requiredUptakeN = deficitN - Sum(uptakeNO3(1:numsl,iofe))
    IF (deficitN <= Sum(uptakeNO3(1:numsl,iofe))) nFixation(iofe) = 0.0

    !! Compute soil water, factor
    soilWaterFactor = profileWater(iofe) / (0.85 * profileWaterFC(iofe))

    !! Compute NO3 factor
    profileNO3 = Sum(sol_no3(1:numsl,iofe))
    IF (profileNO3 > 300.0) NO3Factor = 0.0
    IF (profileNO3 > 100.0 .AND. profileNO3 <= 300.0) NO3Factor = 1.5 - 0.0005 * profileNO3
    IF (profileNO3 <= 100.0) NO3Factor = 1.0

    !! Compute growth stage factor
    IF (phuacc(iofe) > 0.15 .AND. phuacc(iofe) <= 0.30) THEN
        growthFactor = 6.67 * phuacc(iofe) - 1.0
    ELSEIF (phuacc(iofe) > 0.30 .AND. phuacc(iofe) <= 0.55) THEN
        growthFactor = 1.0
    ELSEIF (phuacc(iofe) > 0.55 .AND. phuacc(iofe) <= 0.75) THEN
        growthFactor = 3.75 - 5.0 * phuacc(iofe)
    ENDIF

    !! Compute nitrogen fixation
    fixationFactor = Min(1.0, soilWaterFactor, NO3Factor) * growthFactor
    fixationFactor = Max(0.0, fixationFactor)
    nFixation(iofe) = Min(6.0, fixationFactor * requiredUptakeN)

    RETURN
END
