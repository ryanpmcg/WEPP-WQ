!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    --------------------------------------------------------------------------
!!    description       |date           |names
!!    --------------------------------------------------------------------------
!!    Original Code     |2021-08        |Ryan McGehee (original code)
!!    --------------------------------------------------------------------------
!!
!!    RPM Modification:
!!    + converted to free-source format
!!    + added new deposition declarations
!!    + sometimes there are multiple similar items (e.g., ofe, iofe, and iterOFE)
!!        which exist because a subroutine is reused in different contexts within
!!        two loop constructs; this is obviously not ideal and should be gradually
!!        developed to a different approach.
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This module declares variables for WEPP-WQ including:
!!      + real, integer, and character scalars
!!      + real, integer, and character arrays
!!
!!    ~ ~ ~ PARAMETERS ~ ~ ~
!!    --------------------------------------------------------------------------
!!    Parameter         |Initial Value          |Description
!!    --------------------------------------------------------------------------
!!    mxnp              |                       |maximum number of pesticides (read from pesticide database)
!!    numOFE            |                       |maximum number of OFEs (read from WEPP)
!!    iofe              |                       |current OFE (read from WEPP)
!!    mautof            |                       |maximum autofertilizer operations
!!    --------------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE PARM


    !!! -------------------- SCALAR DECLARATIONS -------------------- !!!

    !! Declare all real scalar variables
    REAL :: Ninitial, soilN, Nloss, Ngain, Nbalance, Pinitial, soilP, Ploss, Pgain, Pbalance, PsTinitial, soilPsT, PsTloss,       &
            PsTgain, PsTbalance, sedorgp, precipitation, wpq20, wlpq20, wps20, wlps20, uobw, ubw, LabToActMinP, ActMinToStaMinP,  &
            autop, auton, rmn2tl, avgAnnualAirTemp, sedimentYieldHillslope, pcNO3, pcNO3i, pcNO3f, pcNH4, pcNH4i, pcNH4f,         &
            precipConcNO3, precipConcNH4, dryDepositionNO3, dryDepositionNH4, wetDepositionNO3, wetDepositionNH4,                 &
            wetDepositionMethod, hillslopeLength, hillslopeWidth, hillslopeArea, runoffConversionFactor

    !! Declare all integer scalar variables
    INTEGER :: mxnp, mcrdb, nfdb, mhru, mautof, mafert, mapest, mno, nhru, nbyr, irtpest, iprp, ofe, iofe, iterOFE, icrop,        &
               numOFE, numPest, numYears, method, leapyr, ipd, lubtot, itotr, itotb, itots, n4, iprn, iter, sl, pstnum,           &
               numsl, numCrop, iwave, simulationDay, simulationJulianDay, simulationMonth, simulationYear, calendarYear,          &
               calendarMonth, calendarDay, startYear, startMonth, startDay, julianDay, julianDayHarvest, julianDayPlant,          &
               stopCondition, stopBehavior

    !! Declare all character scalar variables
    CHARACTER (LEN=100) :: atmosphereInput, cropDatabase, fertilizerDatabase, pesticideDatabase
    CHARACTER (LEN=51) :: cropName

    !! Declare all logical scalar variables
    LOGICAL :: verbose


    !!! -------------------- ARRAY DECLARATIONS -------------------- !!!

    !! Declare all real 1-D arrays
    REAL, DIMENSION (:), ALLOCATABLE :: skoc, wsol, wof, ap_ef, hlife_f, hlife_s, henry, decay_f, decay_s, wac21, wac22,          &
                                        cnyld, rsdco_pl, wsyf, vpth, leaf1, leaf2, t_base, t_opt, dryBioMass, rootMass, Nstress,  &
                                        Pstress, Tstress, Wstress, harvestIndex, bio_e, vpd2, gsi, cmn, ubn, ubp, uobn, uobp,     &
                                        chtmx, wavp, cvm, blai, dlai, rdmx, cpyld, anion_excl, pstsol, pab, pdb, ssb, bio_n1,     &
                                        bio_n2, bio_p1, bio_p2, forgn, forgp, fminn, bactpdb, fminp, fnh4n, bactlpdb, bactkddb,   &
                                        cnb, cpt, phuacc, leafAreaIndex, bio_ms, pnd_sed, pnd_nsed, afrt_ly1, auto_nstr,          &
                                        hru_fr, auto_nmxs, afminn, auto_nmxa, afminp, aforgn, aforgp, afnh4n, auto_eff,           &
                                        solarRadiation, avgMonthlyAirTemp, minAirTemp, maxAirTemp, avgAirTemp, anano3,            &
                                        pcNO3arr, pcNH4arr, avgBulkDensity, enrichmentRatio, profileWater, profileWaterFC,        &
                                        runoffNO3, runoffLabileP, sedimentConcentration, sedimentYieldOFE, sedimentMinP,          &
										sedimentOrgN, sedimentOrgP, soilAlbedo, surfaceFlow, surfaceResidue, surfaceTemp,         &
										tileDrainFlow, tileDrainNO3, tileDrainLabileP, rtn, rtof, nperco, pperco, percop, phoskd, &
										pai, nFixation, plantN, plantP, yield, yieldN, yieldP, newResidue, areaOFE, lengthOFE,    &
										widthOFE, cumAreaOFE, cumLengthOFE, appliedN, appliedP


    !! Declare all real 2-D arrays
    REAL, DIMENSION (:,:), ALLOCATABLE :: sol_labp, sol_nh4, sol_orgn, sol_no3, dkg, dkf, pst_enr, plt_pst, ssub,                 &
                                          conv_wt, sol_fon, sol_fop, bn, bp, sol_aorgn, sol_actp, sol_stap,                       &
                                          bottomDepth, dryBulkDensity, soilCarbon, denitrification, nActOrgMin, nStaOrgMin,       &
                                          lateralFlow, lateralNO3, lateralLabileP, mobileNO3, pActOrgMin, pStaOrgMin,             &
                                          activeOrgNApplied, activeOrgPApplied, freshOrgNApplied, freshOrgPApplied,               &
                                          stableOrgNApplied, stableOrgPApplied, runoffPesticide, sedimentPesticide, soilPorosity, &
                                          soilResidue, soilTemperature, soilThickness, uptakeNO3, uptakeLabileP, soilWater,       &
                                          soilWaterFC, soilWaterMA, soilWaterPA, soilWaterST, soilWaterWP, soilActOrgP,           &
                                          soilStaOrgP, soilTotOrgP, tileDrainPesticide, verticalFlow, verticalNO3,                &
                                          verticalLabileP

    !! Declare all real 3-D arrays
    REAL, DIMENSION (:,:,:), ALLOCATABLE :: sol_pst, sol_kp, frt_kg, fcompost, frt_ly1, pst_kg, sda, frorgn, frorgp, frnh4n,      &
                                            frminn, frminp, lateralPesticide, mobilePesticide, verticalPesticide


    !! Declare all integer 1-D arrays
    INTEGER, DIMENSION (:), ALLOCATABLE :: ndays, idc, nro, nops, nfert, npest, nafert, icr, pstflg

    !! Declare all integer 3-D arrays
    INTEGER, DIMENSION (:,:,:), ALLOCATABLE :: ifert, ipst, iafer, ncr, ipest, ikill, ihvo

    !! Declare all character arrays
    CHARACTER (LEN=100), DIMENSION (:), ALLOCATABLE :: chmmgtdat, solchmdat
    CHARACTER (LEN=4), DIMENSION (:), ALLOCATABLE :: cpnm
    CHARACTER (LEN=20), DIMENSION (:), ALLOCATABLE :: fertnm !name of fertilizer/nutrient
    CHARACTER (LEN=16), DIMENSION (300) :: pname !name of pesticide/toxin
    CHARACTER (LEN=4) :: title(60)

END MODULE PARM
