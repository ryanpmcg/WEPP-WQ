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
!!    + Only global variables (accessed across multiple subroutines) need to be reset here.
!!    + Local variables (accessed only within one subroutine) should be set at each call to the subroutine.
!!    + Do not reset WEPP variables here; those should be handled outside the WEPP-WQ routines before being passed in.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE zeroChemicalTransformationVariables

    !! Load parameter module
    USE parm

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Zeroing Chemical Transformation Variables"




    RETURN
END


