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
!!    + uses free-source format (132 character maximum per line)
!!    + some constants and limits are different from that of various SWAT versions
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine estimates daily nitrogen and phosphorus mineralization and immobilization considering fresh organic material (plant residue) and active and stable humus material
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                      |units              |definition
!!    -----------------------------------------------------------------------
!!    actOrgMinRate             |none               |rate factor for humus mineralization on active organic N
!!    bottom                    |none               |used for storing the bottom of a long equation
!!    carbonFraction            |none               |fraction of carbon in residue
!!    combinedFactor            |none               |product of soil water and temperature factors for mineralization
!!    CNRfactor                 |none               |carbon to nitrogen ratio factor
!!    CPRfactor                 |none               |carbon to phosphorus ratio factor
!!    CtoNratio                 |ratio              |carbon to nitrogen ratio of residue
!!    CtoPratio                 |ratio              |carbon to phosphorus ratio of residue
!!    denitrificationRate       |none               |denitrification rate constant
!!    decayRate                 |none               |residue decomposition/decay rate determined from nutrient composition and soil water and temperature factors
!!    freshDecompN              |kg/ha              |decomposed nitrogen from fresh residue
!!    freshDecompP              |kg/ha              |decomposed phosphorus from fresh residue
!!    residueCompFactor         |none               |residue composition factor
!!    rootCombinedFactor        |none               |square root of combined soil water and temperature factors
!!    soilResidueCarbon         |kg/ha              |amount of residue which is organic carbon
!!    soilTemperatureFactor     |none               |factor for determining soil temperature limitations on mineralization
!!    soilWaterDenitThresh      |none               |denitrification threshold below which none occurs (a fraction of field capacity)
!!    soilWaterFactor           |none               |factor for determining soil water limitations on mineralization
!!    top                       |none               |used for storing the top of a long equation
!!    totalOrganicN             |kg/ha              |amount of organic nitrogen in the active and stable organic pools
!!    transOrgMinRate           |none               |mineralization rate constant
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeNutrientMineralization

    !! Load parameter module
    USE parm

    !! Declare local variables
    REAL :: actOrgMinRate, bottom, carbonFraction, combinedFactor, CNRfactor, CPRfactor, CtoNratio, CtoPratio,                    &
            denitrificationRate, decayRate, freshDecompN, freshDecompP, residueCompFactor, rootCombinedFactor,                    &
            soilResidueCarbon, soilTemperatureFactor, soilWaterDenitThresh, soilWaterFactor, top, totalOrganicN, transOrgMinRate

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Nutrient Mineralization"

    !! Define local variables
    carbonFraction = 0.58                                                           ! fraction (0-1) of residue that is carbon
    actOrgMinRate = cmn(iofe)                                                       ! dimensionless constant from SWAT 2009 EQN 3.1.2.4
    transOrgMinRate = 0.00001                                                       ! dimensionless constant from SWAT 2009 EQN 3.1.2.3
    denitrificationRate = 0.8                                                       ! dimensionless constant from SWAT 2009 EQN 3.1.4.1
    soilWaterDenitThresh = 0.6                                                      ! 60% of field capacity in the soil layer

    !! Compute mineralization by soil layer
    DO sl = 1,numsl

        !! Mineralization can occur only if temp above 0 deg
        IF (soilTemperature(sl,iofe) > 0.0) THEN

            !! Compute soil water factor
            soilWaterFactor = 0.1 + 0.9 * Sqrt(soilWater(sl,iofe)/soilWaterFC(sl,iofe))
            soilWaterFactor = Max(0.05, soilWaterFactor)

            !! Compute soil temperature factor
            top = 0.9 * soilTemperature(sl,iofe)
            bottom = soilTemperature(sl,iofe) + Exp(9.93 - 0.312 * soilTemperature(sl,iofe))
            soilTemperatureFactor =  top/bottom + 0.1
            soilTemperatureFactor = Max(0.1, soilTemperatureFactor)

            !! Compute combined factor
            combinedFactor = soilTemperatureFactor * soilWaterFactor
            IF (combinedFactor < 0.0) combinedFactor = 0.0
            IF (combinedFactor > 1.0e6) combinedFactor = 1.0e6
            rootCombinedFactor = Sqrt(combinedFactor)

            !! Compute conversion between active and stable organic nitrogen pools
            nStaOrgMin(sl,iofe) = transOrgMinRate * (sol_aorgn(sl,iofe) * (1.0 / rtn(iofe) - 1.0) - sol_orgn(sl,iofe))

            IF (nStaOrgMin(sl,iofe) > 0.0) nStaOrgMin(sl,iofe) = Min(nStaOrgMin(sl,iofe), sol_aorgn(sl,iofe))
            IF (nStaOrgMin(sl,iofe) < 0.0) nStaOrgMin(sl,iofe) = -Min(-nStaOrgMin(sl,iofe), sol_orgn(sl,iofe))
            sol_orgn(sl,iofe) = Max(0.0, sol_orgn(sl,iofe) + nStaOrgMin(sl,iofe))
            sol_aorgn(sl,iofe) = Max(0.0, sol_aorgn(sl,iofe) - nStaOrgMin(sl,iofe))

            !! Compute humus mineralization on active organic n
            nActOrgMin(sl,iofe) = actOrgMinRate * rootCombinedFactor * sol_aorgn(sl,iofe)
            nActOrgMin(sl,iofe) = Max(nActOrgMin(sl,iofe), 0.0)
            nActOrgMin(sl,iofe) = Min(nActOrgMin(sl,iofe), sol_aorgn(sl,iofe))

            !! Compute humus mineralization from active organic p
            totalOrganicN = sol_orgn(sl,iofe) + sol_aorgn(sl,iofe)
            pActOrgMin(sl,iofe) = 1.4 * actOrgMinRate / totalOrganicN * soilActOrgP(sl,iofe)
            pActOrgMin(sl,iofe) = Min(pActOrgMin(sl,iofe), soilActOrgP(sl,iofe))

            !! Move mineralized nutrients between pools after the organic P calculations
            sol_aorgn(sl,iofe) = Max(1.0e-6, sol_aorgn(sl,iofe) - nActOrgMin(sl,iofe))
            sol_no3(sl,iofe) = sol_no3(sl,iofe) + nActOrgMin(sl,iofe)
            soilActOrgP(sl,iofe) = soilActOrgP(sl,iofe) - pActOrgMin(sl,iofe)
            sol_labp(sl,iofe) = sol_labp(sl,iofe) + pActOrgMin(sl,iofe)

            !! Compute residue decomposition and mineralization of fresh organic n and p (upper two layers only)
            IF (sl <= 2) THEN

                ! Compute residue carbon
                soilResidueCarbon = carbonFraction * soilResidue(sl,iofe)

                ! Compute residue carbon:nitrogen ratio and factor
                CtoNratio = soilResidueCarbon / (sol_fon(sl,iofe) + sol_no3(sl,iofe))
                CNRfactor = Exp(-0.693 * (CtoNratio - 25.0) / 25.0)
                CNRfactor = Max(CNRfactor, 0.0)

                ! Compute residue carbon:phosphorus ratio and factor
                CtoPratio = soilResidueCarbon / (sol_fop(sl,iofe) + sol_labp(sl,iofe))
                CPRfactor = Exp(-0.693 * (CtoPratio - 200.0) / 200.0)
                CPRfactor = Max(CPRfactor, 0.0)

                ! Compute residue composition factor
                residueCompFactor = Min(CNRfactor, CPRfactor, 1.0)

                ! Compute decay rate constant
                IF (icrop > 0) decayRate =  rsdco_pl(icrop) * residueCompFactor * rootCombinedFactor
                IF (icrop <= 0) decayRate = 0.05
                decayRate = Max(0.0001, decayRate)
                decayRate = Min(decayRate, 1.0)

                ! Compute residue decomposition
                residueDecomp = decayRate * soilResidue(sl,iofe)
                residueDecomp = Max(residueDecomp, 0.0)
                residueDecomp = Min(residueDecomp,  soilResidue(sl,iofe))
                soilResidue(sl,iofe) = soilResidue(sl,iofe) - residueDecomp

                ! Compute nitrogen transformations
                freshDecompN = decayRate * sol_fon(sl,iofe)
                freshDecompN = Max(freshDecompN, 0.0)
                freshDecompN = Min(freshDecompN, sol_fon(sl,iofe))
                sol_fon(sl,iofe) = sol_fon(sl,iofe) - freshDecompN
                sol_no3(sl,iofe) = sol_no3(sl,iofe) + 0.8 * freshDecompN
                sol_aorgn(sl,iofe) = sol_aorgn(sl,iofe) + 0.2 * freshDecompN

                ! Compute phosphorus transformations
                freshDecompP = decayRate * sol_fop(sl,iofe)
                freshDecompP = Max(freshDecompP, 0.0)
                freshDecompP = Min(freshDecompP, sol_fop(sl,iofe))
                sol_fop(sl,iofe) = sol_fop(sl,iofe) - freshDecompP
                sol_labp(sl,iofe) = sol_labp(sl,iofe) + 0.8 * freshDecompP
                soilActOrgP(sl,iofe) = soilActOrgP(sl,iofe) + 0.2 * freshDecompP

            ENDIF

            !! Compute denitrification
            IF (soilWaterFactor >= soilWaterDenitThresh) THEN
                denitrificationFactor = 1.0 - exp(-denitrificationRate * soilTemperatureFactor * soilCarbon(sl,iofe))
                denitrification(sl,iofe) = sol_no3(sl,iofe) * denitrificationFactor
                denitrification(sl,iofe) = Max(denitrification(sl,iofe), 0.0)
                denitrification(sl,iofe) = Min(denitrification(sl,iofe), sol_no3(sl,iofe))
                sol_no3(sl,iofe) = sol_no3(sl,iofe) - denitrification(sl,iofe)
            ELSE
                denitrification(sl,iofe) = 0.0
            ENDIF

        ENDIF

    ENDDO

    RETURN

END SUBROUTINE
