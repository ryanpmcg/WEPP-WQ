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
!!    This function !!COMMENT NEEDED
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          |units              |definition
!!    -----------------------------------------------------------------------
!!    c1            |                   |
!!    c2            |                   |
!!    c3            |                   |
!!    c4            |                   |
!!    erf           |                   |
!!    erfc          |                   |
!!    x             |none               |variable to hold intermediate calculation result
!!    xx            |                   |
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


FUNCTION erfc(xx)

    !! Declare local variables
    real, intent (in) :: xx
    real :: c1, c2, c3, c4, x, erf, erfc

    !! Define local variables
    x = 0.0
    erf = 0.0
    erfc = 0.0
    c1 = 0.19684
    c2 = 0.115194
    c3 = 0.00034
    c4 = 0.019527

    !! COMMENT NEEDED
    x = Abs(1.4142 * xx)
    erf = 1.0 - (1.0 + c1 * x + c2 * x * x + c3 * x**3 + c4 * x**4) **(-4)
    if (xx < 0.0) erf = -erf
    erfc = 1.0 - erf

    RETURN
END
