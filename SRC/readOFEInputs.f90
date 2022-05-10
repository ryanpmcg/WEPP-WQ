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
!!    This subroutine calculates the number of management operation types used in the simulation. These values are used to allocate array sizes for
!!    processes occurring in the OFE.
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name        |units       |definition
!!    -----------------------------------------------------------------------
!!    ap_af       |none        |number of autofertilizer operations in mgt file
!!    ap_f        |none        |number of fertilizer operations in mgt file
!!    ap_p        |none        |number of pesticide operations in mgt file
!!    chmmgtdat   |none        |chemical management data files array
!!    eof         |none        |end of file flag
!!    fertdat     |none        |name of fertilizer database file (fert.dat)
!!    iter        |none        |counter
!!    mno         |none        |max number of operations
!!    numOFE      |none        |number of OFEs
!!    ofe         |none        |the current OFE
!!    ops         |none        |number of operations used in OFE
!!    pestdat     |none        |name of pesticide database input file(pest.dat)
!!    pstflg      |none        |pesticide flag array (array of 0s and 1s to indicate pesticide simulation status)
!!    solchmdat   |none        |soil chemistry data files array
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE readOFEInputs

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: ops, ap_f, ap_p, ap_af, mon, day, mgt_op, mgt2, mgt6, fyear, eof
    REAL :: husc, mgt1, mgt3, mgt4, mgt5, mgt7, mgt8, mgt9, mgt10


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                        READ SOIL CHEMICAL INPUT FILE                       !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    IF (verbose .EQV. .TRUE.) WRITE (*,*) "              Parsing Soil Chemical Inputs"
    OPEN(133,FILE=solchmdat(ofe))                   ! Open soil chemical data (for the current OFE)
    eof = 0                                         ! Reset EOF flag

    !! Skip to pesticide data (after parameters and nutrient data)
    DO iter = 1,45
        READ (133,*,IOSTAT=eof)                     ! Skip header lines
    ENDDO

    !! Read pesticide data
    DO iter = 1,mxnp
        pstnum = 0                                  ! Reset pesticide number
        READ (133,*,IOSTAT=eof) pstnum              ! Read pesticide number
        IF (eof < 0) EXIT
        IF (pstnum > 0) pstflg(pstnum) = 1
    ENDDO
    CLOSE (133)                                     ! Close file


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                     READ CHEMICAL MANAGEMENT INPUT FILE                    !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ops = 0                                         ! Reset operation count

    !! Read database files and number of OFEs
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "              Parsing Chemical Management Inputs"
    OPEN (199,FILE=chmmgtdat(ofe))                  ! Open chemical management data (for the current OFE)

    !! Skip to chemical management data
    DO iter = 1,6
        READ (199,*)                                ! Skip line
    ENDDO

    !! Read operation data
    READ (199,*) ops                                ! Read number of operations
    mno = Max(mno,ops)                              ! Update maximum operations
    ap_f = 0                                        ! Reset fertilizer applications
    ap_p = 0                                        ! Reset pesticide applications
    ap_af = 0                                       ! Reset autofertilizer applications

    !! Skip to operations data
    DO iter = 1,3
        READ (199,*)                                ! Skip line
    ENDDO

    !! Read through operations
    DO iter = 1,ops
        READ (199,*) fyear, mon, day, husc, mgt_op, mgt1, mgt2, mgt3, mgt4, mgt5, mgt6, mgt7, mgt8, mgt9, mgt10

        !! Handle various management operation types
        SELECT CASE (mgt_op)
            CASE (3)                                ! Handle fertilizer operation
                ap_f = ap_f + 1
            CASE (4)                                ! Handle pesticide operation
                ap_p = ap_p + 1
            CASE (11)                               ! Handle autofertilizer operation
                ap_af = ap_af + 1
        END SELECT

        !! Update maximum parameters
        mafert = Max(mafert,ap_f)
        mapest = Max(mapest,ap_p)
        mautof = Max(mautof,ap_af)
        if (iter == ops) EXIT                       ! Exit upon last operation

    ENDDO
    CLOSE (199)                                     ! Close file

    RETURN

END
