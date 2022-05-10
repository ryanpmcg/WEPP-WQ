!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2022-01        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine zeros all surface transport variables.
!!
!!    NOTES:
!!    + Only WEPP-WQ global variables (accessed across multiple subroutines) need to be reset here.
!!    + Local variables (accessed only within one subroutine) should be set at each call to the subroutine.
!!    + Do not reset WEPP variables here; those should be handled outside the WEPP-WQ routines before being passed in.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE zeroChemicalVariables

    !! Load parameter module
    USE parm

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Zeroing Chemical Variables"

    !! Zero uptake variables
    uptakeNO3(1:numsl,iofe) = 0.0

    !! Zero runoff variables
    runoffNO3(iofe) = 0.0
    runoffLabileP(iofe) = 0.0
    runoffPesticide(iofe,1:mxnp) = 0.0

    !! Zero mobile variables
    mobileNO3(1:numsl,iofe) = 0.0
    mobilePesticide(1:numsl,iofe,1:mxnp) = 0.0

    !! Zero lateral variables
    lateralNO3(1:numsl,iofe) = 0.0
    lateralLabileP(1:numsl,iofe) = 0.0
    lateralPesticide(1:numsl,iofe,1:mxnp) = 0.0

    !! Zero vertical variables
    verticalNO3(1:numsl,iofe) = 0.0
    verticalLabileP(1:numsl,iofe) = 0.0
    verticalPesticide(1:numsl,iofe,1:mxnp) = 0.0

    !! Zero tile-drain variables
    tileDrainNO3(iofe) = 0.0
    tileDrainLabileP(iofe) = 0.0
    tileDrainPesticide(iofe,1:mxnp) = 0.0

    !! Zero sediment variables
    sedimentConcentration(iofe) = 0.0
    enrichmentRatio(iofe) = 0.0
    sedimentOrgN(iofe) = 0.0
    sedimentOrgP(iofe) = 0.0
    sedimentMinP(iofe) = 0.0
    sedimentPesticide(1:mxnp,iofe) = 0.0

    !! Zero application variables
    appliedN(iofe) = 0.0
    appliedP(iofe) = 0.0

    !! Zero nitrogen variables
    nFixation(iofe) = 0.0
    denitrification(1:numsl,iofe) = 0.0

    !! Zero phosphorus variables
    LabToActMinP = 0.0
    ActMinToStaMinP = 0.0

    !! Zero pesticide variables

    RETURN
END

