!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |????-??        |Reza Savabi
!!    Modification      |2021-10        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + generic refactoring; converted to free-source format
!!    + added lateral flow calculations; refactored all other 'loss' calculations
!!    + changed local variable names to be descriptive; left existing non-local variables 'as-is'
!!    + soil pesticide stores are in units of kg/ha (can be deduced from local variable notes also)
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates  pesticides leached through each layer, pesticide transported with lateral subsurface flow, and pesticide transported with surface runoff
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    ------------------------------------------------------------------------------
!!    name                              |units              |definition         |options
!!    ------------------------------------------------------------------------------
!!    bottom                            |mm                 |division term from net pesticide equation
!!    mobileWater                       |mm                 |water used to compute pesticide mobility
!!    percopWater                       |mm                 |water used to compute pesticide concentration
!!    pesticideConcentration            |kg/ha-mm           |same as name
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeSolublePesticideTransport

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: ip
    REAL :: bottom, mobileWater, percopWater, pesticideConcentration

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Soluble Pesticide Transport"

	!! Compute for each soil layer
    DO sl = 1,numsl

		!! Compute for each pesticide
		DO iter = 1,mxnp
			ip = iter

			!! Execute only if the pesticide is active in the simulation
			IF (pstflg(iter) > 0) THEN

				!! Compute bottom term of SWAT mobile pesticide equation
				bottom = soilWaterST(sl,iofe) + sol_kp(sl,iofe,ip)*dryBulkDensity(sl,iofe)*soilThickness(sl,iofe)

				!! Compute mobile water
				IF (sl == 1) mobileWater = verticalFlow(sl,iofe) + lateralFlow(sl,iofe) + surfaceFlow(iofe)
				IF (sl /= 1) mobileWater = verticalFlow(sl,iofe) + lateralFlow(sl,iofe)

				!! Compute water for concentration calculations; this is identical to SWAT documentation and model code, but it is different from nitrate calculations and RPM is wondering if that could be an error in SWAT?
				IF (sl == 1) percopWater = verticalFlow(sl,iofe) + percop(iofe)*(lateralFlow(sl,iofe) + surfaceFlow(iofe))
				IF (sl /= 1) percopWater = mobileWater

				!! Compute pesticide concentration
                mobilePesticide(sl,iofe,ip) = sol_pst(sl,iofe,ip) * (1.0 - Exp(-mobileWater/bottom))
                pesticideConcentration = mobilePesticide(sl,iofe,ip)/percopWater
                pesticideConcentration = Max(pesticideConcentration, 0.0)
                pesticideConcentration = Min(wsol(ip)/100.0, pesticideConcentration)


                !!! -------------------- SURFACE RUNOFF CALCULATIONS -------------------- !!!

                !! Calculate pesticide in surface runoff
                IF (sl == 1) THEN
                    runoffPesticide(iofe,ip) = pesticideConcentration * percop(iofe) * surfaceFlow(iofe)
                    runoffPesticide(iofe,ip) = Max(runoffPesticide(iofe,ip), 0.0)
                    runoffPesticide(iofe,ip) = Min(runoffPesticide(iofe,ip), sol_pst(sl,iofe,ip))
                    sol_pst(sl,iofe,ip) = sol_pst(sl,iofe,ip) - runoffPesticide(iofe,ip)
                ENDIF


                !!! -------------------- VERTICAL FLOW CALCULATIONS -------------------- !!!

                !! Calculate pesticide in vertical flow
                verticalPesticide(sl,iofe,ip) = pesticideConcentration * verticalFlow(sl,iofe)
                verticalPesticide(sl,iofe,ip) = Max(verticalPesticide(sl,iofe,ip), 0.0)
                verticalPesticide(sl,iofe,ip) = Min(verticalPesticide(sl,iofe,ip), sol_pst(sl,iofe,ip))
                sol_pst(sl,iofe,ip) = sol_pst(sl,iofe,ip) - verticalPesticide(sl,iofe,ip)

                !! Handle vertical pesticide movement within the soil profile
                IF (sl < numsl) sol_pst(sl+1,iofe,ip) = sol_pst(sl+1,iofe,ip) + verticalPesticide(sl,iofe,ip)


                !!! -------------------- LATERAL FLOW CALCULATIONS -------------------- !!!

                !! Calculate pesticide in lateral flow
                IF (sl == 1) lateralPesticide(sl,iofe,ip) = pesticideConcentration * percop(iofe) * lateralFlow(sl,iofe)
                IF (sl /= 1) lateralPesticide(sl,iofe,ip) = pesticideConcentration * lateralFlow(sl,iofe)
                lateralPesticide(sl,iofe,ip) = Max(lateralPesticide(sl,iofe,ip), 0.0)
                lateralPesticide(sl,iofe,ip) = Min(lateralPesticide(sl,iofe,ip), sol_pst(sl,iofe,ip))
                sol_pst(sl,iofe,ip) = sol_pst(sl,iofe,ip) - lateralPesticide(sl,iofe,ip)

                !! Handle lateral pesticide movement within the hillslope
                IF (iofe < numOFE) sol_pst(sl,iofe+1,ip) = sol_pst(sl,iofe+1,ip) + lateralPesticide(sl,iofe,ip)


                !!! -------------------- TILE DRAINAGE CALCULATIONS -------------------- !!!

                !! Calculate pesticide in tile drainage
                IF (sl == numsl) THEN   !! CODE DEVELOPMENT NEEDED: !RPM: Changed from 9th layer to bottom layer (an improvement), but maybe this should be based on the tile depth instead?
                    tileDrainPesticide(iofe,ip) = pesticideConcentration * tileDrainFlow(iofe)
                    tileDrainPesticide(iofe,ip) = Max(tileDrainPesticide(iofe,ip), 0.0)
                    tileDrainPesticide(iofe,ip) = Min(tileDrainPesticide(iofe,ip), sol_pst(sl,iofe,ip))
                    sol_pst(sl,iofe,ip) = sol_pst(sl,iofe,ip) - tileDrainPesticide(iofe,ip)
                ENDIF


                !!! -------------------- END PESTICIDE LOSS CALCULATIONS -------------------- !!!

			ENDIF
		ENDDO
	ENDDO
    RETURN
END
