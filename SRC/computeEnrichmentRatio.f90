!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2021-12        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + this code has been updated identical to SWAT 2009 documentation
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates the enrichment ratio for nutrient and pesticide transport with runoff
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE computeEnrichmentRatio

    !! Load parameter module
    USE parm

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Computing Enrichment Ratios"

    !! Compute Enrichment Ratio
    sedimentConcentration(iofe) = sedimentYieldOFE(iofe) / (10 * areaOFE(iofe) * surfaceFlow(iofe) + 1.0e-6)
    enrichmentRatio(iofe) = 0.78 * sedimentConcentration(iofe) ** (-0.2468)

    RETURN
END
