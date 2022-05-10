!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
!!    + converted to free-source-format (132 character maximum per line)
!!    + I have not checked this file extensively
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads data from the WEPP-WQ chemical management input file (.chmmgt).
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name            |units            |definition
!!    -----------------------------------------------------------------------
!!    day             |none             |day operation occurs
!!    readYear        |none             |year of the operation in the management file
!!    fyear           |none             |sequence number of year for fertilizer array indexing
!!    husc            |none             |heat unit scheduling for operation expressed as fraction of total heat units of crop at maturity
!!    ifn             |none             |number of fertilizer application in year
!!    iro             |none             |counter for the application years of fertilizer and pesticide
!!    ip              |none             |pesticide counter
!!    mxnp            |none             |number of different pesticides used in the simulation
!!    mgt_op          |none             |operation code number
!!                                                  |0 end of rotation year
!!                                                  |1 plant/beginning of growing season
!!                                                  |2 irrigation operation
!!                                                  |3 fertilizer application
!!                                                  |4 pesticide application
!!                                                  |5 harvest and kill operation
!!                                                  |6 tillage operation
!!                                                  |7 harvest only operation
!!                                                  |8 kill/end of growing season
!!                                                  |9 grazing operation
!!                                                  |10 auto irrigation initialization
!!                                                  |11 auto fertilizer initialization
!!                                                  |12 street sweeping operation
!!    mgt#            |none             |management parameter from the .mgt file (definition changes depending on mgt_op)
!!    mon             |none             |month operation occurs
!!    nafer           |none             |number of auto fertilization operation in year
!!    npst            |none             |number of pesticide application in year
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE readChemicalManagementInputFile

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: iop, mon, day, mgt_op, mgt2, mgt6, ifn, npst, nafer, readYear, fyear, jdt, getJulianDate
    REAL :: husc, mgt1, mgt3, mgt4, mgt5, mgt7, mgt8, mgt9, mgt10

    !! Define local variables
    ifn = 0
    npst = 0
    nafer = 0

    !! Open input file
    OPEN (199,FILE=chmmgtdat(ofe))

    !! Skip to operations data and read
    DO iter = 1,6
        READ (199,*)
    ENDDO
    READ (199,*) nops(ofe)


    !! Skip to scheduled management practices and read
    DO iter = 1,3
        READ (199,*)
    ENDDO

    DO iop = 1, nops(ofe)
        mon = 0
        day = 0
        husc = 0.0
        mgt_op = 0
        mgt1 = 0.0
        mgt2 = 0
        mgt3 = 0.0
        mgt4 = 0.0
        mgt5 = 0.0
        mgt6 = 0
        mgt7 = 0.0
        mgt8 = 0.0
        mgt9 = 0.0
        mgt10 = 0.0

        !! Read data and recompute the year
        READ (199,*) readYear, mon, day, husc, mgt_op, mgt1, mgt2, mgt3, mgt4, mgt5, mgt6, mgt7, mgt8, mgt9, mgt10
        fyear = readYear - startYear + 1

        !! Handle various management operations
        SELECT CASE (mgt_op)

            ! Fertilizer operation
            CASE (3)
                ifn = ifn + 1
                ifert(fyear,ifn,ofe) = getJulianDate(day,mon)
                frt_ly1(fyear,ifn,ofe) = mgt1

                IF (frt_ly1(fyear,ifn,ofe) <= 1.0e-6) frt_ly1(fyear,ifn,ofe) = 1.0e-6
                frt_kg(fyear,ifn,ofe) = mgt3

                IF (mgt2 <= 0) THEN                         ! No fertilizer id # given
                    frorgn(fyear,ifn,ofe) = mgt7*0.01
                    frminn(fyear,ifn,ofe) = mgt4*0.01
                    frnh4n(fyear,ifn,ofe) = mgt9*0.01
                    frminp(fyear,ifn,ofe) = mgt5*0.01
                    frorgp(fyear,ifn,ofe) = mgt8*0.01
                    fcompost(fyear,ifn,ofe) = mgt10
                ELSE                                        ! Fertilizer id # given
                    frorgn(fyear,ifn,ofe) = forgn(mgt2)
                    frminn(fyear,ifn,ofe) = fminn(mgt2)
                    frnh4n(fyear,ifn,ofe) = fnh4n(mgt2)
                    frminp(fyear,ifn,ofe) = fminp(mgt2)
                    frorgp(fyear,ifn,ofe) = forgp(mgt2)
                ENDIF

            ! Pesticide application
            CASE (4)
                npst = npst + 1
                ipst(fyear,npst,ofe) = getJulianDate(day,mon)
                pst_kg(fyear,npst,ofe) = mgt3
                ipest(fyear,npst,ofe) = mgt6

                ! Set active pesticide flags and
                DO ip = 1, mxnp
                    IF (mgt6 == ip) THEN
                        pstflg(ip) = 1
                    ENDIF
                ENDDO

            ! Auto fertilizer operation
            CASE (11)
                nafer = nafer + 1
                iafer(fyear,nafer,ofe) = getJulianDate(day,mon)
                afrt_ly1(ofe) = mgt8
                IF (afrt_ly1(ofe) <= 1.0e-6) afrt_ly1(ofe) = 0.2
                auto_nstr(ofe) = mgt1
                auto_nmxs(ofe) = mgt3
                IF (auto_nmxs(ofe) <= 0.0) auto_nmxs(ofe) = 200.0
                auto_nmxa(ofe) = mgt4
                IF (auto_nmxa(ofe) <= 0.0) auto_nmxa(ofe) = 300.0
                auto_eff(ofe) = mgt7
                IF (auto_eff(ofe) <= 0.0) auto_eff(ofe) = 1.3

                IF (mgt2 <= 0) THEN !! if no fert # given assume 28-10-10
                    afminn(ofe) = 0.280
                    afminp(ofe) = 0.044
                    aforgn(ofe) = 0.0
                    aforgp(ofe) = 0.0
                    afnh4n(ofe) = 0.0
                ELSE
                    afminn(ofe) = fminn(mgt2)
                    afminp(ofe) = fminp(mgt2)
                    aforgn(ofe) = forgn(mgt2)
                    aforgp(ofe) = forgp(mgt2)
                    afnh4n(ofe) = fnh4n(mgt2)
                ENDIF
            END SELECT

        !! Exit upon reading the last operation
        IF (iop == nops(ofe)) EXIT

    ENDDO

    CLOSE (199)
    RETURN
END
