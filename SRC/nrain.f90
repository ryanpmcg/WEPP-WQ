!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2011-??        |Reza Savabi (from SWAT)
!!    Modification      |2021-08        |Ryan McGehee (original code)
!!    -----------------------------------------------------------------------
!!
!!    Original Code Notes:
!!    + none
!!
!!    RPM Modification Notes:
!!    + converted to free source format
!!    + added new deposition options (linear and explicit)
!!    + added ammonium wet deposition
!!    + refactored code, updated documentation, added comments
!!    + ammonium deposition commented out (not implemented elsewhere)
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds nitrate from rainfall to the soil profile
!!
!!    ~ ~ ~ ASSUMPTIONS ~ ~ ~
!!    + rainfall nitrogen concentration is spatially uniform
!!    + rainfall nitrogen concentration changes temporally with annual granularity
!!    + unspecified method results in no atmospheric deposition
!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name              |units          |definition
!!    -----------------------------------------------------------------------
!!    curyr             |none           |current year of simulation
!!    totyr             |none           |total years in simulation
!!    hru_fr(:)         |none           |fraction of watershed in HRU
!!    ihru              |none           |HRU number
!!    nyskip            |none           |number of years to skip output summarization and printing
!!    method            |none           |method for rcn determination (1, 2, or 3; constant, linear, or explicit)
!!    precip            |mm H2O         |precipitation for the day in HRU
!!    rcno3             |mg/L           |rainfall nitrate concentration (scalar)
!!    rcno3i            |mg/L           |initial rainfall nitrate concentration (scalar)
!!    rcno3f            |mg/L           |final rainfall nitrate concentration (scalar)
!!    rcno3arr          |mg/L           |annual rainfall nitrate concentration (1D array)
!!    rcnh4             |mg/L           |rainfall ammonium concentration (scalar)
!!    rcnh4i            |mg/L           |initial rainfall ammonium concentration (scalar)
!!    rcnh4f            |mg/L           |final rainfall ammonium concentration (scalar)
!!    rcnh4arr          |mg/L           |annual rainfall ammonium concentration (1D array)
!!    sol_no3(:,:)      |kg N/ha        |amount of nitrogen stored in the nitrate pool in soil layer
!!    wshd_raino3       |kg N/ha        |average annual amount of NO3 added to soil by rainfall in watershed
!!    -----------------------------------------------------------------------
!!
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name              |units          |definition
!!    -----------------------------------------------------------------------
!!    no3pcp            |kg N/ha        |nitrate added to the soil in rainfall
!!    sol_no3(:,:)      |kg N/ha        |amount of nitrogen stored in the nitrate pool in soil layer
!!    wshd_raino3       |kg N/ha        |average annual amount of NO3 added to soil by rainfall in watershed
!!    -----------------------------------------------------------------------
!!
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name              |units          |definition
!!    -----------------------------------------------------------------------
!!    j                 |none           |HRU number
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE nrain

!! Load parameter module
USE parm

!! Declare local variables
INTEGER :: j
j = 0
j = ihru

!! Input error handling
!! INSERT ERROR HANDLING CODE HERE !!

!! Determine rcn based on user-specified input (method)
IF (method.eq.1) THEN
    rcno3 = rcno3
    !rcnh4 = rcnh4
ELSE IF (method.eq.2) THEN
    rcno3 = rcno3i + (rcno3f - rcno3i) * (curyr/totyr)
    !rcnh4 = rcnh4i + (rcnh4f - rcnh4i) * (curyr/totyr)
ELSE IF (method.eq.3) THEN
    rcno3 = rcno3arr(curyr)
    !rcnh4 = rcnh4arr(curyr)
ELSE
    rcno3 = 0
    !rcnh4 = 0
END IF

!! Calculate nitrogen in precipitation and add to soil surface layer
no3pcp = .01 * rcno3 * precip
!nh4pcp = .01 * rcnh4 * precip
sol_no3(1,j) = sol_no3(1,j) + no3pcp
!sol_nh4(1,j) = sol_nh4(1,j) + nh4pcp

!! Perform summary calculations
IF (curyr > nyskip) THEN
    wshd_raino3 = wshd_raino3 + no3pcp * hru_fr(j)
    !wshd_rainh4 = wshd_rainh4 + nh4pcp * hru_fr(j)
END IF

RETURN
END
