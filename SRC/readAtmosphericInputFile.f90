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
!!    + see 'computeAtmosphericDeposition' for uses of these parameters/inputs and additional documentation
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads data from the atmosphere input file pertaining to wet and dry deposition of nitrate and ammonium.
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                  |units              |definition         |options
!!    -----------------------------------------------------------------------
!!    dryDepositionNO3      |kg/ha              |dry deposited nitrate for the day for the simulation
!!    dryDepositionNH4      |kg/ha              |dry deposited ammonium for the day for the simulation
!!    eof                   |none               |end of file flag
!!    pcNO3                 |mg/L (ppm)         |concentration of nitrate in precipitation
!!    pcNO3i                |mg/L (ppm)         |initial concentration of nitrate in precipitation
!!    pcNO3f                |mg/L (ppm)         |final concentration of nitrate in precipitation
!!    pcNO3arr              |mg/L (ppm)         |annual concentration of nitrate in precipitation
!!    pcNH4                 |mg/L (ppm)         |concentration of ammonium in precipitation
!!    pcNH4i                |mg/L (ppm)         |initial concentration of ammonium in precipitation
!!    pcNH4f                |mg/L (ppm)         |final concentration of ammonium in precipitation
!!    pcNH4arr              |mg/L (ppm)         |annual concentration of ammonium in precipitation
!!    wetDepostionMethod    |integer            |determines wet deposition simulation behavior
!!                                                                  |1-constant concentration of pcNO3|pcNH4 every year
!!                                                                  |2-linear concentration change pcNO3i|pcNH4i to pcNO3f|pcNH4f every year
!!                                                                  |3-explicit concentration of pcNO3arr(iyear)|pcNH4arr(iyear) every year
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE readAtmosphericInputFile

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: iyear, eof

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "          Reading Atmospheric Inputs"


    !!! -------------------- OPEN FILE AND READ WET DEPOSITION METHOD -------------------- !!!

    OPEN (122,FILE=atmosphereInput)
    eof = 0
    DO iter = 1,9
        READ(122,*)
    END DO
    READ (122,*, IOSTAT=eof) wetDepositionMethod


    !!! -------------------- READ NITRATE PARAMETERS -------------------- !!!

    DO iter = 1,3
        READ(122,*)
    END DO
    READ (122,*, IOSTAT=eof) dryDepositionNO3
    READ (122,*, IOSTAT=eof) pcNO3
    READ (122,*, IOSTAT=eof) pcNO3i
    READ (122,*, IOSTAT=eof) pcNO3f
    READ (122,*)
    READ (122,*, IOSTAT=eof) (pcNO3arr(iyear), iyear=1,numYears)


    !!! -------------------- READ AMMONIUM PARAMETERS -------------------- !!!

    DO iter = 1,3
        READ(122,*)
    END DO
    READ (122,*, IOSTAT=eof) dryDepositionNH4
    READ (122,*, IOSTAT=eof) pcNH4
    READ (122,*, IOSTAT=eof) pcNH4i
    READ (122,*, IOSTAT=eof) pcNH4f
    READ (122,*)
    READ (122,*, IOSTAT=eof) (pcNH4arr(iyear), iyear=1,numYears)


    !!! -------------------- END READING AND CLOSE FILE -------------------- !!!

    CLOSE(122)
    RETURN
END SUBROUTINE

