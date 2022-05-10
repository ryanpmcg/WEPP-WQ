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
!!    + changed file unit due to conflict with interface graphics file
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine estimates daily average temperature at the bottom of each soil layer
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                  |units         |definition
!!    -----------------------------------------------------------------------
!!    a                     |none          |variable to hold intermediate calculation
!!    b                     |none          |variable to hold intermediate calculation
!!    bareTemp              |deg C         |temperature of bare soil surface
!!    c                     |none          |variable to hold intermediate calculation
!!    centerDepth           |mm            |soil layer center depth
!!    coverLag              |none          |lagging factor for cover
!!    coverTemp             |deg C         |temperature of soil surface corrected for cover
!!    dampingDepth          |mm            |damping depth for day
!!    densityFactor         |none          |variable to hold intermediate calculation
!!    depthFactor           |none          |depth factor
!!    depthRatio            |none          |ratio of depth at center of layer to damping depth
!!    lag                   |none          |lag coefficient for soil temperature
!!    LHS                   |none          |variable to hold intermediate calculation
!!    maxDampingDepth       |mm            |maximum damping depth
!!    RHS                   |none          |variable to hold intermediate calculation
!!    solarBalance          |MJ/m^2        |radiation hitting soil surface on day
!!    profileDensityFactor  |none          |variable to hold intermediate calculation
!!    waterScalingFactor    |none          |scaling factor for soil water impact on daily damping depth
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeSoilTemperature

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: a, b, bareTemp, c, centerDepth, coverLag, coverTemp, dampingDepth, densityFactor, depthFactor, depthRatio, lag,       &
            LHS, maxDampingDepth, newTemperature, profileDensityFactor, RHS, solarBalance, waterScalingFactor

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Soil Temperature"

    !! Define local variables
    lag = 0.8

    !! Calculate maximum damping depth
    densityFactor = avgBulkDensity(iofe) / (avgBulkDensity(iofe) + 686.0 * Exp(-5.63 * avgBulkDensity(iofe)))                       ! SWAT manual equation 2.3.6
    maxDampingDepth = 1000.0 + 2500.0 * densityFactor

    !! Calculate scaling factor for soil water                                                                                      ! SWAT manual equation 2.3.7
    profileDensityFactor = 0.356 - 0.144 * avgBulkDensity(iofe)
    waterScalingFactor = profileWater(iofe) / (profileDensityFactor * bottomDepth(numsl,iofe))

    !! Calculate daily value for damping depth                                                                                      ! SWAT manual equation 2.3.8
    a = Log(500.0 / maxDampingDepth)
    b = Exp(a * ((1.0 - waterScalingFactor) / (1.0 + waterScalingFactor))**2)
    dampingDepth = b * maxDampingDepth

    !! Calculate lagging factor for soil cover impact on soil surface temp                                                          ! SWAT manual equation 2.3.11
    coverLag = surfaceResidue(iofe) / (surfaceResidue(iofe) + Exp(7.563 - 1.297e-4 * surfaceResidue(iofe)))
    coverLag = Max(1.0, coverLag)

    !! Calculate temperature at soil surface
    solarBalance = (solarRadiation(iofe) * (1.0 - soilAlbedo(iofe)) - 14.0) / 20.0                                                  ! SWAT manual equation 2.3.10
    bareTemp = avgAirTemp(iofe) + 0.5 * (maxAirTemp(iofe) - minAirTemp(iofe)) * solarBalance                                        ! SWAT manual equation 2.3.9
    coverTemp = coverLag * soilTemperature(2,iofe) + (1.0 - coverLag) * bareTemp                                                    ! SWAT manual equation 2.3.12

    !! Adjust surface temperature for cover ! RPM is not sure why this isn't 0.00 rather than 0.01
    IF (surfaceResidue(iofe) >= 0.01) surfaceTemp(iofe) = Min(bareTemp, coverTemp)
    IF (surfaceResidue(iofe) < 0.01) surfaceTemp(iofe) = bareTemp

    !! Calculate temperature for each layer on current day
    c = 0.0
    DO sl = 1,numsl
        centerDepth = (c + bottomDepth(sl,iofe)) / 2.0                                                                              ! calculate depth at center of layer
        depthRatio = centerDepth / dampingDepth                                                                                     ! SWAT manual equation 2.3.5
        depthFactor = depthRatio / (depthRatio + Exp(-0.8669 - 2.0775 * depthRatio))                                                ! SWAT manual equation 2.3.4
        LHS = lag*soilTemperature(sl,iofe)
        RHS = (1.0 - lag)*(depthFactor*(avgAnnualAirTemp-surfaceTemp(iofe)) + surfaceTemp(iofe))
        newTemperature = LHS + RHS                                                                                                  ! SWAT manual equation 2.3.3
        IF (ISNAN(newTemperature)) newTemperature = soilTemperature(sl,iofe)                                                        ! Added for cases where little to no change results in NaN
        soilTemperature(sl,iofe) = newTemperature
        c = bottomDepth(sl,iofe)
    ENDDO
    RETURN
END
