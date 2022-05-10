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
!!    + added lateral flow and tile flow calculations
!!    + added vertical flow calculations beyond the first soil layer
!!    + changed local variable names to be descriptive; left existing non-local variables 'as-is'
!!    + soil phosphorus stores are in units of kg/ha (which can be deduced from local variable descriptions also)
!!
!!    ~ ~ ~ ASSUMPTIONS ~ ~ ~
!!    + 50% of labile P is available to be moved in the top 10mm of soil
!!    + 20% of labile P is available to be moved in other soil depths
!!    + these values were obtained from the 2012 SWAT code base (solp.f) but were not mentioned in the 2009 documentation
!!    + apparently, runoff is not affected the same way in the 2012 code base as described in the 2009 documentation
!!      - i.e., the fractionAvailableLabileP is not enforced in runoff labile P calculations though it is in all other loss functions
!!      - the same convention has been implemented here
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates the amount of soluble phosphorus lost from the soil profile in runoff, vertical, lateral, and tile drain flow.
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                          |units              |definition
!!    -----------------------------------------------------------------------
!!    fractionAvailableLabileP      |none               |same as name
!!    mmAdsorbedP                   |mm                 |same as name
!!    percolationFactor             |mm                 |unit conversion factor for pperco
!!    top10mmPortion                |none               |portion of soil layer in the top 10mm of the profile
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeSolublePhosphorusTransport

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: fractionAvailableLabileP, mmAdsorbedP, percolationFactor, thickness, top10mmPortion

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Soluble Phosphorus Transport"

    !! Compute for each soil layer
    DO sl = 1,numsl

        !! Compute layer portion in the top 10mm
        IF (bottomDepth(sl,iofe) <= 10.0) top10mmPortion = 1.0                                                                          ! Soil layer completely above 10mm threshold
        IF (bottomDepth(sl,iofe) > 10.0 .AND. bottomDepth(sl,iofe) - soilThickness(sl,iofe) < 10.0)                               &
            top10mmPortion = 1.0 - (bottomDepth(sl,iofe) - 10.0)/soilThickness(sl,iofe)                                                 ! Soil layer crosses 10mm threshold
        IF (bottomDepth(sl,iofe) - soilThickness(sl,iofe) >= 10.0) top10mmPortion = 0.0                                                 ! Soil layer completely below 10mm threshold
        fractionAvailableLabileP = 0.5*top10mmPortion + 0.2*(1.0 - top10mmPortion)

        !! Compute labile P percolation factor
        percolationFactor = (10.0 * dryBulkDensity(sl,iofe) * bottomDepth(sl,iofe) * pperco(iofe))


        !!! -------------------- SURFACE RUNOFF CALCULATIONS -------------------- !!!

        !! Calculate phosphorus in surface runoff
        IF (sl == 1) THEN
            mmAdsorbedP = dryBulkDensity(sl,iofe) * bottomDepth(sl,iofe) * phoskd(iofe)
            runoffLabileP(iofe) = sol_labp(sl,iofe) * surfaceFlow(iofe) / mmAdsorbedP                                                   ! If it is to be enforced, the fractionAvailableLabileP should be multiplied by soilLabileP on this line
            runoffLabileP(iofe) = Min(runoffLabileP(iofe), sol_labp(sl,iofe))
            runoffLabileP(iofe) = Max(runoffLabileP(iofe), 0.0)
            sol_labp(sl,iofe) = sol_labp(sl,iofe) - runoffLabileP(iofe)
        ENDIF


        !!! -------------------- VERTICAL FLOW CALCULATIONS -------------------- !!!

        !! Calculate phosphorus in vertical flow
        verticalLabileP(sl,iofe) = sol_labp(sl,iofe) * verticalFlow(sl,iofe)/percolationFactor
        verticalLabileP(sl,iofe) = Min(verticalLabileP(sl,iofe), fractionAvailableLabileP*sol_labp(sl,iofe))
        verticalLabileP(sl,iofe) = Max(verticalLabileP(sl,iofe), 0.0)
        sol_labp(sl,iofe) = sol_labp(sl,iofe) - verticalLabileP(sl,iofe)

        !! Handle vertical phosphorus movement within the soil profile
        IF (sl < numsl) sol_labp(sl+1,iofe) = sol_labp(sl+1,iofe) + verticalLabileP(sl,iofe)


        !!! -------------------- LATERAL FLOW CALCULATIONS -------------------- !!!

        !! Calculate phosphorus in lateral flow
        lateralLabileP(sl,iofe) = sol_labp(sl,iofe) * lateralFlow(sl,iofe)/percolationFactor
        lateralLabileP(sl,iofe) = Min(lateralLabileP(sl,iofe), fractionAvailableLabileP*sol_labp(sl,iofe))
        lateralLabileP(sl,iofe) = Max(lateralLabileP(sl,iofe), 0.0)
        sol_labp(sl,iofe) = sol_labp(sl,iofe) - lateralLabileP(sl,iofe)

        ! Handle lateral phosphorus movement within the hillslope
        IF (iofe < numOFE) sol_labp(sl,iofe+1) = sol_labp(sl,iofe+1) + lateralLabileP(sl,iofe)


        !!! -------------------- TILE DRAINAGE CALCULATIONS -------------------- !!!

        !! Calculate phosphorus in tile drainage
        IF (sl == numsl) THEN   !! CODE DEVELOPMENT NEEDED: !RPM: Changed from 9th layer to bottom layer (an improvement), but maybe this should be based on the tile depth instead?
            tileDrainLabileP(iofe) = sol_labp(sl,iofe) * tileDrainFlow(iofe)/percolationFactor
            tileDrainLabileP(iofe) = Min(tileDrainLabileP(iofe), fractionAvailableLabileP*sol_labp(sl,iofe))
            tileDrainLabileP(iofe) = Max(tileDrainLabileP(iofe), 0.0)
            sol_labp(sl,iofe) = sol_labp(sl,iofe) - tileDrainLabileP(iofe)
        ENDIF


        !!! -------------------- END PHOSPHORUS LOSS CALCULATIONS -------------------- !!!

    ENDDO
    RETURN

END
