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
!!    This subroutine estimates daily nitrification (NH4 to NO3) and volatilization (NH4 to NH3)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeNitrogenVolatilization

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: fc25wp75, top, bottom, topDepth, middleDepth, depthFactor, soilWaterFactor, temperaturefactor, nitrificationFactor,   &
            volatilizationFactor, ammoniumLoss, nitrification, volatilization, cecFactor

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Nitrification and Volatilization"

    !! Define local variables
    cecFactor = 0.15

    !! Compute for each soil layer
    DO sl = 1,numsl

        !! Compute soil temperature factor
        IF (soilTemperature(sl,iofe) > 5.0) temperaturefactor = 0.41*(soilTemperature(sl,iofe) - 5.0)/10.0
        IF (soilTemperature(sl,iofe) <= 5.0) temperaturefactor = 0.0

        !! Compute soil water factor
        fc25wp75 = 0.25*soilWaterFC(sl,iofe) - 0.75*soilWaterWP(sl,iofe)
        IF (soilWater(sl,iofe) < fc25wp75) THEN
            top = soilWater(sl,iofe) - soilWaterWP(sl,iofe)
            bottom = 0.25*(soilWaterFC(sl,iofe) - soilWaterWP(sl,iofe))
            soilWaterFactor = top / bottom
        ELSE
            soilWaterFactor = 1.0
        END IF

        !! Compute depth factor
        topDepth = bottomDepth(sl,iofe) - soilThickness(sl,iofe)
        middleDepth = (bottomDepth(sl,iofe) + topDepth) / 2.0
        depthFactor = 1.0 - middleDepth/(middleDepth + Exp(4.706 - 0.0305*middleDepth))

        !! Compute nitrification and volatilization factors
        nitrificationFactor = temperaturefactor * soilWaterFactor
        volatilizationFactor = temperaturefactor * depthFactor * cecFactor

        !! Only compute losses if they are possible and present
        IF (sol_nh4(sl,iofe) > 0.0 .AND. (nitrificationFactor > 0.0 .OR. volatilizationFactor > 0.0)) THEN

            ! Compute total loss and loss partitioning factors
            ammoniumLoss = Min(sol_nh4(sl,iofe), sol_nh4(sl,iofe)*(1.0 - Exp(-nitrificationFactor - volatilizationFactor)))
            nitrification = 1.0 - Exp(-nitrificationFactor)
            volatilization = 1.0 - Exp(-volatilizationFactor)

            ! Calculate ammonium volatilization (NH4 => NH3)
            volatilization = ammoniumLoss * volatilization/(volatilization + nitrification)
            volatilization = Max(volatilization, 0.0)
            volatilization = Min(volatilization, sol_nh4(sl,iofe))
            sol_nh4(sl,iofe) = sol_nh4(sl,iofe) - volatilization

            ! Calculate nitrification (NH4 => NO3)
            nitrification = ammoniumLoss - volatilization
            nitrification = Max(nitrification, 0.0)
            nitrification = Min(nitrification, sol_nh4(sl,iofe))
            sol_nh4(sl,iofe) = sol_nh4(sl,iofe) - nitrification
            sol_no3(sl,iofe) = sol_no3(sl,iofe) + nitrification

        ENDIF

    ENDDO

    RETURN

END SUBROUTINE
