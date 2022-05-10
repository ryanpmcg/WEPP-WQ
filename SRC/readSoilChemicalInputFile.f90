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
!!    This subroutine reads data from the OFE/subbasin soil chemical input file (.chm). This file contains initial amounts of pesticides/nutrients
!!    in the first soil layer. (Specifics about the first soil layer are given in the .sol file.) All data in the .chm file is optional input.
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          |units              |definition         |options
!!    -----------------------------------------------------------------------
!!    eof           |none               |end of file flag
!!    mxnp          |none               |maximum number of pesticides used in watershed
!!    newpest       |none               |new pesticide flag
!!    pltpst        |kg/ha              |pesticide on plant foliage
!!    pstenr        |none               |pesticide enrichment ratio
!!    pstnum        |none               |pesticide number
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE readSoilChemicalInputFile

    !! Load parameter module
    USE parm

    !! Declare local variables
    CHARACTER (LEN=1000) :: string
    INTEGER :: colonIndex, newpest, ip, eof
    REAL :: pltpst, pstenr

    !! Initialize variables
    eof = 0

    !! Open input file
    OPEN (133,FILE=solchmdat(ofe))

    !! Skip to parameter data
    DO iter = 1,8
        READ(133,*)
    END DO

    !! Read parameters
    READ (133,*, IOSTAT=eof) rtn(ofe)
    READ (133,*, IOSTAT=eof) rtof(ofe)
    READ (133,*, IOSTAT=eof) cmn(ofe)
    READ (133,*, IOSTAT=eof) ubn(ofe)
    READ (133,*, IOSTAT=eof) ubp(ofe)
    READ (133,*, IOSTAT=eof) nperco(ofe)
    READ (133,*, IOSTAT=eof) pperco(ofe)
    READ (133,*, IOSTAT=eof) phoskd(ofe)
    READ (133,*, IOSTAT=eof) pai(ofe)
    READ (133,*, IOSTAT=eof) percop(ofe)
    READ (133,*, IOSTAT=eof) anion_excl(ofe)

    !! Set default values for undefined parameters
    IF (cmn(ofe) < 0.0) cmn(ofe) = 0.0003
    IF (ubn(ofe) < 0.0) ubn(ofe) = 20.0
    IF (ubp(ofe) < 0.0) ubp(ofe) = 20.0
    IF (nperco(ofe) < 0.0) nperco(ofe) = 0.20
    IF (pperco(ofe) < 0.0) pperco(ofe) = 10.0
    IF (phoskd(ofe) < 0.0) phoskd(ofe) = 175.0
    IF (pai(ofe) <= 0.0) pai(ofe) = 0.1
    IF (percop(ofe) < 0.0) percop(ofe) = 0.5
    IF (anion_excl(ofe) < 0.0) anion_excl(ofe) = 0.5
    IF (anion_excl(ofe) > 1.0) anion_excl(ofe) = 1.0

    !! Calculate normalization parameters for water, nitrogen, and phosphorus uptake (the uptake distribution for water is hardcoded)
    ubw = 10.0
    uobw = 1.0 - exp(-ubw)
    uobn(ofe) = 1.0 - exp(-ubn(ofe))
    uobp(ofe) = 1.0 - exp(-ubp(ofe))

    !! Skip to soil nutrient data and read
    DO iter=1,7
        READ(133,*)
    ENDDO
    READ (133,1000,IOSTAT=eof) string
    colonIndex = index(string, ":")
    READ (string((colonIndex + 1):1000), *) (sol_no3(sl,ofe),sl=1,numsl)
    READ (133,1000,IOSTAT=eof) string
    colonIndex = index(string, ":")
    READ (string((colonIndex + 1):1000), *) (sol_nh4(sl,ofe),sl=1,numsl)
    READ (133,1000,IOSTAT=eof) string
    colonIndex = index(string, ":")
    READ (string ((colonIndex + 1):1000), *) (sol_orgn(sl,ofe),sl=1,numsl)
    READ (133,1000,IOSTAT=eof) string
    colonIndex = index(string, ":")
    READ (string ((colonIndex + 1):1000), *) (sol_labp(sl,ofe),sl=1,numsl)
    READ (133,1000,IOSTAT=eof) string
    colonIndex = index(string, ":")
    READ (string ((colonIndex + 1):1000), *) (soilTotOrgP(sl,ofe),sl=1,numsl)

    !! Skip to soil pesticide data
    DO iter = 1,7
        READ(133,*)
    ENDDO

    !! Read soil pesticide data
    DO ip = 1,mxnp

        ! Reset parameters
        newpest = 0
        pstnum = 0
        pltpst = 0.0
        pstenr = 0.0

        ! Read line and exit if EOF reached
        READ (133,*,IOSTAT=eof) pstnum, pltpst, pstenr, (sol_pst(sl,ofe,pstnum), sl=1,numsl)
        IF (pstnum > 0) pstflg(pstnum) = 1                 ! change pesticide status in flag array
        plt_pst(pstnum,ofe) = pltpst                       ! record plant pesticide amount
        pst_enr(pstnum,ofe) = pstenr                       ! record pesticide enrichment ratio
        IF (eof < 0) EXIT
    ENDDO

    1000 FORMAT (A1000)

    !! Close file and return data
    CLOSE (133)
    RETURN
END
