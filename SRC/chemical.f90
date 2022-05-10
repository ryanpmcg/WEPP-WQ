!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
!!    + this code is intended to be executed once per day per OFE
!!    + this code executes in an order that is questionable to me but is consistent with SWAT 2009
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine computes chemical processes on a daily basis
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE chemical

    !! Load parameter module
    USE parm

    !! Declare local variables
    IMPLICIT NONE
    INTEGER :: chemicalInitializationFlag, getJulianDate

    !!! -------------------- HANDLE INVALID VALUES -------------------- !!!

    CALL checkChemicalSimulationHealth
    IF (stopCondition .EQ. 0) STOP 'ERROR: An invalid value was encountered. More detail is provided in the error file.'
    IF (stopCondition .EQ. 2) CALL repairChemicalSimulation


    !!! -------------------- RESET APPROPRIATE VARIABLES -------------------- !!!

    CALL zeroChemicalVariables


    !!! -------------------- HANDLE CROP MANAGEMENT IMPACTS -------------------- !!!

    CALL getCropNumber

    !! Simulate harvest/kill operations on those days
    IF (julianDay .EQ. julianDayHarvest) CALL harvestKillOperation(icrop)

    !! Set biomass and rootmass to zero after harvest
    IF (julianDayHarvest .GT. julianDayPlant .AND. julianDay .GT. julianDayHarvest ) THEN
        bio_ms(iofe) = 0.0
        dryBioMass(iofe) = 0.0
        rootMass(iofe) = 0.0
    ENDIF

    !! Set biomass and rootmass to zero before planting
    IF (julianDayHarvest .GT. julianDayPlant .AND. julianDay .LE. julianDayPlant ) THEN
        bio_ms(iofe) = 0.0
        dryBioMass(iofe) = 0.0
        rootMass(iofe) = 0.0
    ENDIF

    !! Add biomass and rootmass between harvest and planting
    IF (julianDayHarvest .LE. julianDayPlant .AND. julianDay .GT. julianDayPlant) THEN
        bio_ms(iofe) = rootMass(iofe) + dryBioMass(iofe)
    ELSEIF (julianDay .GT. julianDayHarvest) THEN
        bio_ms(iofe) = 0.0
        dryBioMass = 0.0
        rootMass = 0.0
    ENDIF


    !!! -------------------- SIMULATE TEMPERATURE AND GROWTH -------------------- !!!

    CALL computeSoilTemperature
    CALL computeNitrogenUptake
    CALL computePhosphorusUptake

    !! Set cumulative plant nutrient variables to zero on harvest day
    IF (julianDay .EQ. julianDayHarvest) THEN
        plantN(iofe) = 0.0
        plantP(iofe) = 0.0
    ENDIF


    !!! -------------------- SIMULATE FERTILIZER APPLICATION -------------------- !!!

    IF (julianDay == ifert(simulationYear,nfert(iofe),iofe)) CALL fertilizerApplication

    SELECT CASE (idc(icrop))
        CASE (4, 5, 6, 7)
            IF (auto_nstr(iofe) > 0.0) CALL automatedFertilizerApplication
    END SELECT

    !! Set nitrogen and phosphorus stress to 1 (no stress) after harvest !RPM is not sure this should occur?
    IF (julianDay .GT. julianDayHarvest) THEN
        Nstress = 1.0
        Pstress = 1.0
    ENDIF


    !!! -------------------- SIMULATE PESTICIDE APPLICATION -------------------- !!!
    IF (mapest > 0) THEN
        DO WHILE (julianDay == ipst(simulationYear,npest(iofe),iofe))
            CALL pesticideApplication
            IF (npest(iofe) .GT. mapest) THEN
                npest(iofe) = 1
                EXIT
            ENDIF
        ENDDO
    ENDIF


    !!! -------------------- SIMULATE CHEMICAL TRANSFORMATIONS -------------------- !!!

    CALL computeNutrientMineralization
    CALL computeNitrogenVolatilization
    CALL computePhosphorusMineralization
    CALL computeAtmosphericDeposition


    !!! -------------------- SIMULATE PESTICIDE DYNAMICS -------------------- !!!

    IF (precipitation >= 2.54) CALL computePesticideWashoff
    CALL computePesticideDecay


    !!! -------------------- SIMULATE SEDIMENT CHEMICAL TRANSPORT -------------------- !!!

    IF (surfaceFlow(iofe) > 0.0) THEN
        IF (sedimentYieldOFE(iofe) > 0.0) THEN
            CALL computeEnrichmentRatio
            CALL computeSedimentPesticideTransport
            CALL computeSedimentPhosphorusTransport
            CALL computeSedimentNitrogenTransport
        ENDIF
    ENDIF


    !!! -------------------- SIMULATE SOLUBLE CHEMICAL TRANSPORT -------------------- !!!

    CALL computeSolubleNitrogenTransport
    CALL computeSolublePhosphorusTransport
    CALL computeSolublePesticideTransport


    !!! -------------------- SUMMARIZE AND OUTPUT RESULTS -------------------- !!!

    iterOFE = iofe
    CALL summarize
    CALL writeChemicalOutput


    !!! -------------------- END CHEMICAL SIMULATION -------------------- !!!

    RETURN
END
