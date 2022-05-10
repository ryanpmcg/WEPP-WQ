!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2021-11        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + uses free-source-format (132 character maximum per line)
!!    + this code is intended to be run once per OFE
!!    + conventions follow SWAT 2009 documentation
!!    + There is no stable organic phosphorus pool in WEPP-WQ as there is in SWAT.
!!
!!    IMPORTANT NOTES:
!!    + Do not confuse ofe and iofe and iterOFE!
!!    + iofe is the OFE iterator controlled by WEPP and ofe/iterOFE operates independent of WEPP!
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine initializes soil chemical properties
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE initializeSoilChemistry

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: conversionFactor, depthFactor, depthDifference, soilPesticide

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Initializing Soil Chemistry"


    !!! -------------------- INITIALIZE SOIL CARBON -------------------- !!!

    !! Initialize soil carbon for subsurface layers if specified (< 0)
    DO sl = 2,numsl
        IF (soilCarbon(sl,ofe) < 0.0) THEN
            depthDifference = bottomDepth(sl,ofe) - bottomDepth(1,ofe)
            soilCarbon(sl,ofe) = soilCarbon(sl-1,ofe) * Exp(-0.001 * depthDifference)
        ENDIF
    ENDDO


    !!! -------------------- INITIALIZE SOIL PESTICIDES -------------------- !!!

    !! Compute pesticide sorption coefficients, convert pesticide units, and initialize lower layer concentrations if specified (< 0)
    DO ip = 1,mxnp
        IF (pstflg(ip) > 0) THEN
            DO sl = 1,numsl
                soilPesticide = sol_pst(sl,ofe,ip)
                IF (soilPesticide < 0.0) soilPesticide = sol_pst(1,ofe,ip)
                conversionFactor = dryBulkDensity(sl,ofe) * soilThickness(sl,ofe) / 100.0               !! mg/kg (ppm) => kg/ha
                sol_kp(sl,ofe,ip) = skoc(ip) * soilCarbon(sl,ofe) / 100.0
                sol_pst(sl,ofe,ip) = soilPesticide * conversionFactor
            ENDDO
        ENDIF
    ENDDO


    !!! -------------------- INITIALIZE SOIL NUTRIENTS -------------------- !!!

    !! Calculate initial nutrient contents of soil layers and profile
    DO sl = 1,numsl
        conversionFactor = dryBulkDensity(sl,ofe) * soilThickness(sl,ofe) / 100.0                       !! mg/kg (ppm) => kg/ha
        conv_wt(sl,ofe) = 1.0e6 * conversionFactor                                                      !! kg/kg => kg/ha

        !! Initialize and convert units for soil nitrate; EQN 3.1.1.1 from SWAT Manual
        IF (sol_no3(sl,ofe) < 0.0) THEN
            depthFactor = Exp(-bottomDepth(sl,ofe) / 1000.0)
            sol_no3(sl,ofe) = 7.0*depthFactor
        ENDIF
        sol_no3(sl,ofe) = sol_no3(sl,ofe) * conversionFactor

        !! Initialize and convert units for soil ammonium
        IF (sol_nh4(sl,ofe) < 0.0) sol_nh4(sl,ofe) = 0.0
        sol_nh4(sl,ofe) = sol_nh4(sl,ofe) * conversionFactor

        !! Initialize and convert units for soil organic nitrogen; Assume C:N ratio of 14.0
        IF (sol_orgn(sl,ofe) >= 0.0) sol_orgn(sl,ofe) = sol_orgn(sl,ofe) * conversionFactor
        IF (sol_orgn(sl,ofe) < 0.0) sol_orgn(sl,ofe) = 1.0e6 * ((soilCarbon(sl,ofe)/100.0) / 14.0) * conversionFactor
        sol_aorgn(sl,ofe) = sol_orgn(sl,ofe) * rtn(ofe)
        sol_orgn(sl,ofe) = sol_orgn(sl,ofe) * (1.0 - rtn(ofe))

        !! Initialize and convert units for soil organic phosphorus; Assume N:P ratio of 8.0
        IF (soilTotOrgP(sl,ofe) >= 0.0) soilTotOrgP(sl,ofe) = soilTotOrgP(sl,ofe) * conversionFactor
        IF (soilTotOrgP(sl,ofe) < 0.0) soilTotOrgP(sl,ofe) = sol_orgn(sl,ofe) / 8.0
        soilActOrgP(sl,ofe) = soilTotOrgP(sl,ofe) * rtn(ofe)
        soilStaOrgP(sl,ofe) = soilTotOrgP(sl,ofe) - soilActOrgP(sl,ofe)

        !! Initialize and convert units for soil labile phosphorus; Assume 5 mg/kg (ppm)
        IF (sol_labp(sl,ofe) >= 0.0) sol_labp(sl,ofe) = sol_labp(sl,ofe) * conversionFactor
        IF (sol_labp(sl,ofe) < 0.0) sol_labp(sl,ofe) = 25.0 * conversionFactor
        sol_actp(sl,ofe) = sol_labp(sl,ofe) * (1.0 - pai(ofe)) / pai(ofe)
        sol_stap(sl,ofe) = 4.0 * sol_actp(sl,ofe)

    ENDDO

    !! Initialize and convert units for soil fresh nutrients
    sol_fon(1,ofe) = soilResidue(1,ofe) * 0.0015
    sol_fop(1,ofe) = soilResidue(1,ofe) * 0.0003

    RETURN

END SUBROUTINE
