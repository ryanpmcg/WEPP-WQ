!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    Created by:       Ryan P. McGehee
!!    Last Modified:    November 2021
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This file contains subroutines related to array searching.
!!
!!    ~ ~ ~ NOTES ~ ~ ~
!!    + This code is written in free source format.
!!    + Descriptive variables are used such that documentation is not necessary.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! The following subroutine searches an explicit 1-D integer array for the first match and returns that value.
SUBROUTINE findFirstMatch(searchArray, arrayLength, searchValue, location)

    !! Declare inputs
    INTEGER, INTENT (IN) :: arrayLength, searchValue
    INTEGER, INTENT (OUT) :: location
    INTEGER, INTENT (IN), DIMENSION (:) :: searchArray(arrayLength)

    !! Perform the search
    DO iter=1,arrayLength

        ! Return the value when found
        IF (searchArray(iter) == searchValue) THEN
            location = iter
            RETURN
        ENDIF

        ! Return zero when not found
        IF (iter == arrayLength) location = 0

    END DO
    RETURN

END SUBROUTINE
