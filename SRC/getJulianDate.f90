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
!!    This subroutine computes the julian date given the month and the day of the month
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name        |units         |definition
!!    -----------------------------------------------------------------------
!!    i           |none          |day
!!    jdt         |julian date   |julian date
!!    m           |none          |month
!!    ndays(:)    |julian date   |julian date for last day of preceding month (where the array location is the number of the month). The dates are for leap years (numdays=ndays)
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


FUNCTION getJulianDate(i,m)

    !! Declare local variables
    INTEGER, dimension (13) :: ndays
    INTEGER, intent (in) :: m, i
    INTEGER :: jdt

    !! Initialize local variables
    ndays = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
    jdt = 0

    !! Compute Julian date
    IF (m /= 0) THEN
        IF (m <= 2) jdt = ndays(m) + i
        IF (m > 2) jdt = ndays(m) - 1 + i
    ENDIF
    RETURN
END
