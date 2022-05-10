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
!!    + changed file unit due to conflict with interface graphics file
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This function corrects rate constants for temperature (equation is III-52 from QUAL2E)
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name        |units         |definition
!!    -----------------------------------------------------------------------
!!    r20         |1/day         |value of the reaction rate coefficient at the standard temperature (20 degrees C)
!!    thk         |none          |temperature adjustment factor (empirical constant for each reaction coefficient)
!!    tmp         |deg C         |temperature on current day
!!    theta       |1/day         |value of the reaction rate coefficient at the local temperature
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE getCropNumber

    !! Load parameter module
    USE PARM

    !! Declare local variables
    CHARACTER (LEN=51) :: nameCrop
    INTEGER :: cropNumber, eof

    !! Open and read crop database
    OPEN (1115,FILE="cropReferenceNumbers.dat")
    DO
        READ (1115,*,IOSTAT=eof) nameCrop, cropNumber
        IF (eof < 0) EXIT
        IF (ADJUSTL(ADJUSTR(cropName)) .EQ. ADJUSTL(ADJUSTR(nameCrop))) THEN
            icrop = cropNumber
            EXIT
        ENDIF
    ENDDO

    !! Close file and return data
    CLOSE (1115)
    RETURN
END SUBROUTINE
