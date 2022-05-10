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
!!    This subroutine reads input parameters from the fertilizer/manure (i.e. nutrient) database
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          |units              |definition
!!    -----------------------------------------------------------------------
!!    ifnum         |none               |number of fertilizer/manure. reference
!!    it            |none               |counter which represents the array storage number of the pesticide data the array storage number is used by the model to access data for a specific fertilizer
!!    nfdb          |none               |maximum number of fertilizers in database
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE readFertilizerDatabaseFile

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: it, ifnum, eof

    !! Open and read database
    eof = 0                                         ! Reset EOF indicator
    OPEN (111,FILE=fertilizerDatabase)              ! Open file

    !! Skip header and instruction lines
    DO iter=1,10
        READ (111,*)                                ! Skip line; File header
    ENDDO

    !! Read fertilizer data
    DO it = 1,nfdb
        ifnum = 0                                   ! Reset fertilizer code
        READ (111,*,IOSTAT=eof) ifnum, fertnm(it), fminn(it), fminp(it), forgn(it), forgp(it), fnh4n(it), bactpdb(it),            &
                                bactlpdb(it), bactkddb(it)
        IF (eof < 0) EXIT                           ! Exit if EOF reached
    ENDDO

    !! Close file and return data
    CLOSE (111)
    RETURN
END
