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
!!    This subroutine automatically applies Nitrogen and Phosphorus when Nitrogen stress exceeds a user input threshold.
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          |units              |definition         |options
!!    -----------------------------------------------------------------------
!!    auto_nmxa(:)  |kg NO3-N/ha        |maximum NO3-N content allowed to be applied in one year by auto-fertilization
!!    auto_nmxs(:)  |kg NO3-N/ha        |maximum NO3-N content allowed in one fertilizer application
!!    auto_nstr(:)  |none               |nitrogen stress factor which triggers auto fertilization
!!    auton         |kg N/ha            |amount of nitrogen applied in auto-fert application
!!    autop         |kg P/ha            |amount of phosphorus applied in auto-fert application
!!    auton         |kg N/ha            |amount of nitrogen applied in auto-fert application
!!    autop         |kg P/ha            |amount of phosphorus applied in auto-fert application
!!    dwfert        |kg fert/ha         |amount of fertilizer to be applied to meet nitrogen requirement
!!    nstress       |none               |code for approach used to determine amount of nitrogen to OFE
!!                                                          |0 nitrogen target approach
!!                                                          |1 annual max approach
!!    rtoaf         |none               |weighting factor used to partition the organic N & P content of the fertilizer between the fresh organic and the active organic pools
!!    targn         |kg N/ha            |target mineral N application
!!    tfp           |kg minP/kg frt     |fraction of mineral P to be applied
!!    tpno3         |
!!    tsno3         |
!!    xx            |none               |fraction of total amount of fertilizer to be applied to layer
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE automatedFertilizerApplication

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER, PARAMETER :: Nmethod = 0
    REAL :: rtoaf, tsno3, tpno3, dwfert, xx, targn, tfp

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Simulating Automated Fertilizer Application"

    !! Define local variables
    rtoaf = 0.50

    !! Determine amount of mineral N to be applied
    IF (Nstress(iofe) < auto_nstr(iofe)) THEN
        targn = 0.

        !! COMMENT NEEDED
        IF (Nmethod == 0) THEN                                      ! n target approach
            tsno3 = 0.
            tpno3 = 0.

            !! COMMENT NEEDED
            DO sl = 1,numsl
                tsno3 = tsno3 + sol_no3(sl,iofe) + sol_nh4(sl,iofe)
            ENDDO

            !! COMMENT NEEDED
            tpno3 = cnb(iofe) * bio_ms(iofe)
            !targn = tnylda(nro(iofe),icr(iofe),iofe) - tsno3 - tpno3
            targn = 0.0 - tsno3 - tpno3 !! CODE DEVELOPMENT NEEDED: RPM: Remove this line and uncomment the previous line when tnylda is calculated (make sure to declare and define tnylda!)
            IF (targn > auto_nmxs(iofe)) targn = auto_nmxs(iofe)
            IF (targn < 0.0) targn = 0.0
            anano3(iofe) = anano3(iofe) + targn

            !! COMMENT NEEDED
            IF (anano3(iofe) >= auto_nmxa(iofe)) THEN
                targn = auto_nmxa(iofe) - (anano3(iofe) - targn)
                IF (targn < 0.) targn = 0.
                anano3(iofe) = auto_nmxa(iofe)
            ENDIF

        !! COMMENT NEEDED
        ELSE                                                        ! annual max approach
            targn = auto_nmxs(iofe) * (1. - phuacc(iofe))
            IF (targn > auto_nmxs(iofe)) targn = auto_nmxs(iofe)
            anano3(iofe) = anano3(iofe) + targn

            !! COMMENT NEEDED
            IF (anano3(iofe) >= auto_nmxa(iofe)) THEN
                targn = auto_nmxa(iofe) - (anano3(iofe) - targn)
                anano3(iofe) = auto_nmxa(iofe)
            ENDIF
        ENDIF

        !! COMMENT NEEDED
        IF (targn <= 1.e-6) RETURN

        !! Add nutrients to soil based on nitrogen need
        dwfert = 0.
        IF (afminn(iofe) > 0.0001) THEN
            dwfert = targn / afminn(iofe)
        ELSE
            dwfert = 0.
        ENDIF

        !! COMMENT NEEDED
        DO sl = 1,2
            xx = 0.

            !! COMMENT NEEDED
            IF (sl == 1) THEN
                xx = afrt_ly1(iofe)
            ELSE
                xx = 1. - afrt_ly1(iofe)
            ENDIF

            !! COMMENT NEEDED
            sol_no3(sl,iofe) = sol_no3(sl,iofe) + xx * dwfert * afminn(iofe) * (1. - afnh4n(iofe))
            sol_nh4(sl,iofe) = sol_nh4(sl,iofe) + xx * dwfert * afminn(iofe) * afnh4n(iofe)
            sol_fon(sl,iofe) = sol_fon(sl,iofe) + rtoaf * xx * dwfert * aforgn(iofe)
            sol_aorgn(sl,iofe) = sol_aorgn(sl,iofe) + (1.0 - rtoaf) * xx * dwfert * aforgn(iofe)
            sol_fop(sl,iofe) = sol_fop(sl,iofe) + rtoaf * xx * dwfert * aforgp(iofe)
            soilActOrgP(sl,iofe) = soilActOrgP(sl,iofe) + (1.0 - rtoaf) * xx * dwfert * aforgp(iofe)

            !! Check for P stress
            tfp = 0.
            IF (Pstress(iofe) <= 0.75) THEN
                tfp = afminn(iofe) / 7.
            ELSE
                tfp = afminp(iofe)
            ENDIF
            sol_labp(sl,iofe) = sol_labp(sl,iofe) + xx * dwfert * tfp
        ENDDO

        !! Perform summary calculations
        auton = auton + dwfert * (afminn(iofe) + aforgn(iofe))
        autop = autop + dwfert * (tfp + aforgp(iofe))
    ENDIF
    RETURN
END
