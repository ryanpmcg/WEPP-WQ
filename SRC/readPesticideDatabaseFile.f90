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
!!    + made inputs more user-friendly (added in-file instructions)
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads parameters from the toxin/pesticide database
!!
!!    ~ ~ ~ ASSUMPTIONS ~ ~ ~
!!    The first-order rate law for the decay of pesticides is dP/dt = -kP where P is the amount of pesticide, t is the time
!!    and k is the rate constant for degradation. To calculate the amount of pesticide at any time after application the equation
!!    P(t) = P_o*Exp(-kt) is used where P_o is the original amount of pesticide. k can be calculated with the equation k = 0.693/hlife.
!!    decay_f or decay_s = Exp(-k)
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          |units              |definition         |option
!!    -----------------------------------------------------------------------
!!    eof           |none               |end of file flag
!!    pid           |none               |number of pesticide/toxin
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE readPesticideDatabaseFile

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: ip, pid, eof

    !! Open and read database
    eof = 0                                         ! Reset EOF indicator
    OPEN (177,FILE=pesticideDatabase)               ! Open file

    !! Skip header and instruction lines
    DO iter=1,10
        READ (177,*)                                ! Skip line; File header
    ENDDO

    !! Read data and compute decay parameters
    DO ip = 1,mxnp
        pid = 0                                     ! Reset pesticide code
        READ (177,*,IOSTAT=eof) pid, pname(pid), skoc(pid), wof(pid), hlife_f(pid), hlife_s(pid), ap_ef(pid), wsol(pid), henry(pid)
        IF (eof < 0) EXIT                           ! Exit if EOF reached

        !! Compute pesticide decay based on stated assumptions
        ! Decay on foliage
        IF (hlife_f(pid) > 0.0) THEN
            decay_f(pid) = Exp(-0.693/hlife_f(pid))
        ELSE
            decay_f(pid) = 0.0
        ENDIF

        ! Decay on soil
        IF (hlife_s(pid) > 0.0) THEN
            decay_s(pid) = Exp(-0.693/hlife_s(pid))
        ELSE
            decay_s(pid) = 0.0
        ENDIF

    ENDDO

    !! Close file and return data
    CLOSE (177)
    RETURN
END
