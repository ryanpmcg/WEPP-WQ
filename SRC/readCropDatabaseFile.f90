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
!!    This subroutine reads input parameters from the landuse/landcover database (crop.dat)
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~  !! COMMENT NEEDED: Some table values are missing!
!!    -----------------------------------------------------------------------
!!    name          |units              |definition         |option
!!    -----------------------------------------------------------------------
!!    b1            |none               |variable to hold calculation results
!!    b2            |none               |variable to hold calculation results
!!    b3            |none               |variable to hold calculation results
!!    bioehi        |(kg/ha)/(MJ/m**2)  |biomass-energy ratio when plant is in an environment with CO2 level equal to the value of CO2HI. This biomass-energy ratio is used to set the 2nd point on the radiation use efficiency curve variable to hold calculation results
!!    co2hi         |uL CO2/L air       |CO2 concentration higher than the ambient corresponding to the 2nd point on radiation use efficiency curve
!!    eof           |none               |end of file flag (=-1 of eof, else =0)
!!    frgrw1        |none               |fraction of the growing season corresponding to the 1st point on optimal leaf area development curve
!!    frgrw2        |none               |fraction of the growing season corresponding to the 2nd point on optimal leaf area development curve
!!    ic            |none               |landuse/landcover array storage number when a land cover is assigned in the .mgt file, the variables for the land cover are accessed by the array number. Landuse/landcover numbers (ICNUM) in crop.dat need to be assigned consecutively to ensure that the crop number used by the user is the same as the array storage number
!!    icnum         |none               |crop/landcover number. Reference number only.
!!    laimx1        |none               |fraction of maximum leaf area index corresponding to the 1st point on optimal leaf area development curve
!!    laimx2        |none               |fraction of maximum leaf area index corresponding to the 2nd point on optimal leaf area development curve
!!    mcrdb         |none               |maximum number of crops/landcover in database file (crop.dat)
!!    temp
!!    usle_c        |none               |minimum value of the USLE C factor for water erosion
!!    xx            |none               |dummy variable to hold IDC expressed as a real number
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE readCropDatabaseFile

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: ic, icnum, eof
    REAL :: xx, usle_c, frgrw2, laimx2, co2hi, bioehi, b1, b2, b3, c1, temp, frgrw1, laimx1

    !! Reset EOF status flag and open database
    eof = 0
    OPEN (155,FILE=cropDatabase)

    !! Skip header lines
    DO iter = 1,10
        READ (155,*)
    END DO

    !! Loop through each crop in the database
    DO ic = 1,mcrdb

        !! Initialize local variables in loop
        bioehi = 0.0
        co2hi = 0.0
        icnum = 0
        frgrw1 = 0.0
        frgrw2 = 0.0
        laimx1 = 0.0
        laimx2 = 0.0
        usle_c = 0.0
        xx = 0.0

        !! Read parameters from file
        READ (155,*,IOSTAT=eof) icnum, cpnm(ic), idc(ic), bio_e(ic), harvestIndex(ic), blai(ic), frgrw1, laimx1, frgrw2, laimx2,  &
                                dlai(ic), chtmx(ic), rdmx(ic), t_opt(ic), t_base(ic), cnyld(ic), cpyld(ic), bn(1,ic), bn(2,ic),   &
                                bn(3,ic), bp(1,ic), bp(2,ic), bp(3,ic), wsyf(ic), usle_c, gsi(ic), vpth(ic), wavp(ic), vpd2(ic),  &
                                co2hi, bioehi, rsdco_pl(ic)

        !! Set default value
        IF (bio_e(ic) > 0.0) THEN

            !! Determine shape parameters for the leaf area development equation
            CALL getScurve(laimx1, laimx2, frgrw1, frgrw2, leaf1(ic), leaf2(ic))

            !! The other point used to determine shape parameters for radiation use efficiency is the ambient CO2 level (330 ul/l) and the biomass-energy ratio (bio_e) given for the crop/land cover.
            c1 = 330.0                                       ! ambient CO2 !! CODE DEVELOPMENT NEEDED: Link this to atmospheric input file
            IF (co2hi == 330.0) co2hi = 660.0                ! RPM: this makes no sense.
            b1 = bio_e(ic) * 0.01                            ! "ambient" bio-e ratio/100
            b2 = bioehi * 0.01                               ! "elevated" bio-e ratio/100

            !! Determine shape parameters for the radiation use efficiency equation
            CALL getScurve(b1, b2, c1, co2hi, wac21(ic), wac22(ic))
            IF (usle_c < 1.0e-4) usle_c = 0.001
            cvm(ic) = Log(usle_c)

            !! Set Nitrogen uptake parameters
            IF (bn(2,ic) - bn(3,ic) < 0.0001) bn(3,ic) = 0.75 * bn(3,ic) ! fix bad input for bn(3,ic)
            b1 = bn(1,ic) - bn(3,ic)                        ! Normalize N fractions
            b2 = 1.0 - (bn(2,ic) - bn(3,ic)) / b1
            b3 = 1.0 - 0.00001 / b1

            !! Determine shape parameters for plant nitrogen uptake equation
            CALL getScurve(b2, b3, 0.5, 1.0, bio_n1(ic), bio_n2(ic))

            !! Set phosphorus uptake parameters
            IF (bp(2,ic) - bp(3,ic) < 0.0001) bp(3,ic) = 0.75 * bp(3,ic) ! fix bad input for bp(3,ic)
            b1 = bp(1,ic) - bp(3,ic)                        !Normalize P fractions
            b2 = 1.0 - (bp(2,ic) - bp(3,ic)) / b1
            b3 = 1.0 - 0.00001 / b1

            !! Determine shape parameters for plant phosphorus uptake equation
            CALL getScurve(b2, b3, 0.5, 1.0, bio_p1(ic), bio_p2(ic))
            temp = 0.0
            temp = vpd2(ic) - Int(vpd2(ic))
            IF (vpd2(ic) < 1.0e-6) THEN
                vpd2(ic) = 0.0
            ELSE
                vpd2(ic) = (1.0-vpd2(ic)) / (temp-vpth(ic))
                vpd2(ic) = -1.0 / vpd2(ic)
            ENDIF
        ENDIF
    ENDDO

    CLOSE (155)
    RETURN
END
