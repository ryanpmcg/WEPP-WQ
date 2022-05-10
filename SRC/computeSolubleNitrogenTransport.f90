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
!!    + generic refactoring; converted to free-source format
!!    + added lateral flow calculations; refactored all other 'loss' calculations
!!    + changed local variable names to be descriptive; left existing non-local variables 'as-is'
!!    + soil nitrogen stores are in units of kg/ha (can be deduced from local variable notes also)
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine simulates the loss of nitrate via surface runoff, lateral flow, and percolation out of the profile
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name          |units              |definition         |option
!!    -----------------------------------------------------------------------
!!    NO3Concentration                  |kg/ha-mm           |same as name
!!    mobileWater                       |mm                 |same as name
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeSolubleNitrogenTransport

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: mobileWater, NO3Concentration

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Soluble Nitrogen Transport"

    !! Compute for each soil layer
    DO sl = 1,numsl

        !! Determine concentration of nitrate in mobile water
        IF (sl == 1) mobileWater = verticalFlow(sl,iofe) + lateralFlow(sl,iofe) + surfaceFlow(iofe) + 1.0e-10
        IF (sl /= 1) mobileWater = verticalFlow(sl,iofe) + lateralFlow(sl,iofe) + 1.0e-10
        mobileNO3(sl,iofe) = sol_no3(sl,iofe) * (1.0 - Exp(-mobileWater / ((1.0 - anion_excl(iofe)) * soilWaterST(sl,iofe))))
        mobileNO3(sl,iofe) = Max(mobileNO3(sl,iofe), 0.0)
        mobileNO3(sl,iofe) = Min(mobileNO3(sl,iofe), sol_no3(sl,iofe))
        NO3Concentration = Max(mobileNO3(sl,iofe)/mobileWater, 0.0)


        !!! -------------------- SURFACE RUNOFF CALCULATIONS -------------------- !!!

        !! Calculate nitrate in surface runoff
        IF (sl == 1) THEN
            runoffNO3(iofe) = NO3Concentration * nperco(iofe) * surfaceFlow(iofe)
            runoffNO3(iofe) = Max(runoffNO3(iofe), 0.0)
            runoffNO3(iofe) = Min(runoffNO3(iofe), sol_no3(sl,iofe))
            sol_no3(sl,iofe) = sol_no3(sl,iofe) - runoffNO3(iofe)
        ENDIF


        !!! -------------------- VERTICAL FLOW CALCULATIONS -------------------- !!!

        !! Calculate nitrate in vertical flow
        verticalNO3(sl,iofe) = NO3Concentration * verticalFlow(sl,iofe)
        verticalNO3(sl,iofe) = Max(verticalNO3(sl,iofe), 0.0)
        verticalNO3(sl,iofe) = Min(verticalNO3(sl,iofe), sol_no3(sl,iofe))
        sol_no3(sl,iofe) = sol_no3(sl,iofe) - verticalNO3(sl,iofe)

        !! Handle vertical pesticide movement within the soil profile
        IF (sl < numsl) sol_no3(sl+1,iofe) = sol_no3(sl+1,iofe) + verticalNO3(sl,iofe)


        !!! -------------------- LATERAL FLOW CALCULATIONS -------------------- !!!

        !! Calculate nitrate in lateral flow
        IF (sl == 1) lateralNO3(sl,iofe) = NO3Concentration * nperco(iofe) * lateralFlow(sl,iofe)
        IF (sl /= 1) lateralNO3(sl,iofe) = NO3Concentration * lateralFlow(sl,iofe)
        lateralNO3(sl,iofe) = Max(lateralNO3(sl,iofe), 0.0)
        lateralNO3(sl,iofe) = Min(lateralNO3(sl,iofe), sol_no3(sl,iofe))
        sol_no3(sl,iofe) = sol_no3(sl,iofe) - lateralNO3(sl,iofe)

        ! Handle lateral nitrate movement within the hillslope
        IF (iofe < numOFE) sol_no3(sl,iofe+1) = sol_no3(sl,iofe+1) + lateralNO3(sl,iofe)


        !!! -------------------- TILE DRAINAGE CALCULATIONS -------------------- !!!

        !! Calculate nitrate in tile drainage
        IF (sl == numsl) THEN   !! CODE DEVELOPMENT NEEDED: !RPM: Changed from 9th layer to bottom layer (an improvement), but maybe this should be based on the tile depth instead?
            tileDrainNO3(iofe) = NO3Concentration * tileDrainFlow(iofe)
            tileDrainNO3(iofe) = Max(tileDrainNO3(iofe), 0.0)
            tileDrainNO3(iofe) = Min(tileDrainNO3(iofe), sol_no3(sl,iofe))
            sol_no3(sl,iofe) = sol_no3(sl,iofe) - tileDrainNO3(iofe)
        ENDIF


        !!! -------------------- END NITRATE LOSS CALCULATIONS -------------------- !!!

    ENDDO
    RETURN
END
