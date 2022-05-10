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
!!   This subroutine calculates the number of OFEs, hillslopes, etc. in the simulation. These values are used to allocate array sizes.
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name        |units       |definition
!!    -----------------------------------------------------------------------
!!    chmmgtdat   |none        |chemical management data files array
!!    eof         |none        |end of file flag
!!    fertdat     |none        |name of fertilizer database file (fert.dat)
!!    nfdb        |none        |max number of fertilizers in fert.dat
!!    mxnp        |none        |max number of pesticides in simulation (from pest.dat)
!!    pestdat     |none        |name of pesticide database input file(pest.dat)
!!    solchmdat   |none        |soil chemistry data files array
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE getAllocationSizes

    !! Load parameter module
    USE parm
    INTEGER :: eof

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Determining Allocation Sizes"

    !! Define local variables
    cropDatabase = ""
    fertilizerDatabase = ""
    numOFE = 0
    pesticideDatabase = ""

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                        READ CONTROL FILE PARAMETERS                        !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !! Read database files and number of OFEs
    OPEN (22,FILE="control.txt")                    ! Open file
    DO iter=1,6
        READ (22,*)                                 ! Skip line; File header
    ENDDO
    READ (22,1000) cropDatabase                     ! Read pesticide database file path
    READ (22,1000) pesticideDatabase                ! Read pesticide database file path
    READ (22,1000) fertilizerDatabase               ! Read fertilizer database file path
    READ (22,*)                                     ! Skip line; Empty
    READ (22,*)                                     ! Skip line; Instruction
    READ (22,*)                                     ! Skip line; Instruction denotation
    READ (22,*) atmosphereInput                     ! Read atmosphere input file name
    READ (22,*)                                     ! Skip line; Empty
    READ (22,*)                                     ! Skip line; Instruction
    READ (22,*)                                     ! Skip line; Instruction denotation
    READ (22,*) numOFE                              ! Read number of OFEs

    !! Allocate and initialize OFE input arrays
    ALLOCATE (chmmgtdat(numOFE))
    ALLOCATE (solchmdat(numOFE))
    chmmgtdat = ""
    solchmdat = ""

    !! Read inputs for each OFE
    DO ofe = 1,numOFE
        READ (22,*)                                 ! Skip line; Empty
        READ (22,*)                                 ! Skip line; Instruction
        READ (22,*)                                 ! Skip line; Instruction denotation
        READ (22,1000) chmmgtdat(ofe)               ! Read OFE chemical management input
        READ (22,1000) solchmdat(ofe)               ! Read OFE soil chemical input
    ENDDO
    CLOSE (22)                                      ! Close file


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                        READ PESTICIDE DATABASE FILE                        !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !! Reset pesticide file read status variables
    eof = 0
    mxnp = 0

    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Parsing Pesticide Database"
    OPEN (177,FILE=pesticideDatabase)               ! Open file
    DO iter = 1,10
        READ (177,*)                                ! Skip line; File header
    ENDDO

    !! Calculate number of records in pesticide database (read until the end of file)
    DO
        READ (177,*,IOSTAT=eof)                     ! Set EOF status
        IF (eof < 0) EXIT                           ! Check EOF status
        mxnp = mxnp + 1                             ! Increase pesticide count
    ENDDO
    CLOSE (177)                                     ! Close file

    !! Set pesticide read status variable
    IF (mxnp <= 0) mxnp = 1


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                        READ FERTILIZER DATABASE FILE                       !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !! Reset fertilizer file read status variables
    eof = 0
    nfdb = 0

    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Parsing Fertilizer Database"
    OPEN (111,FILE=fertilizerDatabase)              ! Open file
    DO iter = 1,10
        READ (111,*)                                ! Skip line; File header
    ENDDO

    !! Calculate number of records in fertilizer database (read until the end of file)
    DO
        READ (111,*,IOSTAT=eof)                     ! Set EOF status
        IF (eof < 0) EXIT                           ! Check EOF status
        nfdb = nfdb + 1                             ! Increase fertilizer count
    ENDDO
    CLOSE (111)                                     ! Close file

    !! Set fertilizer read status variable
    IF (nfdb <= 0) nfdb = 1


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                        ALLOCATE OFE DATA STRUCTURES                        !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !! Allocate pesticide flag array and initialize
    !  Variable array                         |Unit              |Description
    !  -----------------------------------------------------------------------
    ALLOCATE (pstflg(mxnp))                  !|None              |Indicates pesticide (referenced to database) is active in the simulation
    pstflg = 0

    !! Read inputs and allocate structures for each OFE
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Reading OFE Inputs"
    DO ofe = 1,numOFE
        CALL readOFEInputs
    ENDDO

    RETURN

    1000 format (a100)

END
