!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |yyyy-mm        |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2021-09        |Ryan McGehee
!!    -----------------------------------------------------------------------
!!
!!    RPM Code Notes:
!!    + uses free-source-format (132 character maximum per line)
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes chemical processes on a daily basis
!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name              |units          |definition
!!    -----------------------------------------------------------------------
!!    ihru              |none           |??
!!    nro(:)            |none           |sequence number of year in rotation
!!    ipst(:,:,:)       |julian date    |date of pesticide application
!!    phupst(:,:,:)     |none           |fraction of plant heat units at which pesticide application occurs
!!    igro(:)           |none           |land cover status code. This code informs the model whether or not a land cover is growing at the beginning of the simulation (0 no land cover growing; 1 land cover growing)
!!    gplt(:)           |heat units     |base zero total heat units (used when no land cover is growing) within the year
!!    phuacc(:)         |none           |fraction of plant heat units accumulated
!!    peakr             |m^3/s          |peak runoff rate
!!
!!    -----------------------------------------------------------------------
!!
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                          |units          |definition
!!    -----------------------------------------------------------------------
!!
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE chemical(bddry, st, flux, ul, thetdr, dg, fc, solthk, mxnsl, mxplan, sdate, FPHU, areaOFE,       &
           runoff, rmagt, smrm, obmaxt, obmint, tmin, tmax, tave, radmj, salb, month, day, sedyldMT, iplane,&
           rain, soilw, jyear, ncrop, CropName, orgmat, LAI, jdharv, jdplt, drainq)


!! Load parameter module (modparm.f)
USE parm


!! Declare local variables
IMPLICIT NONE
CHARACTER*51 :: CropName, NamCrop, NULL
INTEGER :: iwave, jyear, month, day, iplane, ncrop, tdplt, mxnsl, mxplan, sdate, jdharv, iro, jdplt, icrop, &
           crop_number, ICFLAG
REAL :: bddry(mxnsl,mxplan), st(mxnsl,mxplan), flux(mxnsl,mxplan), ul(mxnsl), thetdr(mxnsl,mxplan),         &
        fc(mxnsl,mxplan), solthk(mxnsl,mxplan), dg(mxnsl,mxplan), rmagt(mxplan), smrm(3,mxplan), FPHU,      &
        areaOFE, runoff, obmaxt(12), obmint(12), tmin, tmax, tave, radmj, salb, sedyldMT, rain, drainq,     &
        rootMass, soilw(mxnsl,mxplan), orgmat(mxnsl,mxplan), flat, strsn, strsp, strsw, strst, dryBioMass,  &
        HINDX, LAI


!! Load common blocks
COMMON /nutrient_stress/ strsn, strsp, strsw, strst, dryBioMass, HINDX, rootMass      !!DSHI
COMMON /chem_initialize/ ICFLAG


!! Define local variables
mcr = ncrop
ihru = iplane
iwave = 0
da_ha = areaOFE/10000.
curyr = jyear
flat = drainq


!! COMMENT NEEDED
IF (ICFLAG .LT. 1) THEN
    NamCrop = '                                                   '
    NULL =    '                                                   '
    FPHU = 0.0
ENDIF


!! Initialize simulation
CALL chemical_initialize(bddry, st, flux, ul, thetdr, dg, fc, solthk, rmagt, smrm, mxnsl, mxplan, obmaxt,   &
    obmint, tmin, tmax, tave, radmj, salb, month, soilw, orgmat, LAI, drainq)
CALL initializeVariables


!! COMMENT NEEDED
nn = sol_nly(ihru)
bio_ms(ihru) = dryBioMass + rootMass
phuacc(ihru) = FPHU
sedyld = sedyldMT
surfq = runoff*1000.
precip = rain*1000.


!! COMMENT NEEDED
IF (CropName .eq. NULL) NamCrop = CropName
icrop = crop_number(CropName)
NamCrop = CropName
ida = sdate


!! COMMENT NEEDED
IF (sdate .eq. jdharv) THEN
    CALL harvkillop(icrop)
!    bio_ms(ihru) = 0.
!    dryBioMass = 0.
!    snup(ihru) = 0.
!    spup(ihru) = 0.
!    alai(ihru) = 0.1
!    rootMass = 0.
!    phuacc(ihru) = 0.
!    strsn = 1.0
!    strsp = 1.0
ENDIF


!! Set biomass and rootmass to zero after harvest !RPM is not sure this should occur?
IF (jdharv .gt. jdplt .and. sdate.gt.jdharv ) THEN
    bio_ms(ihru) = 0
    dryBioMass = 0
    rootMass = 0
ENDIF


!! Set biomass and rootmass to zero before planting !RPM is not sure this should occur?
IF (jdharv .gt. jdplt .and. sdate.le.jdplt ) THEN
    bio_ms(ihru) = 0
    dryBioMass = 0
    rootMass = 0
ENDIF


!! Add biomass and rootmass between harvest and planting !RPM is not sure this should occur?
IF (jdharv .le. jdplt .and. sdate .gt. jdplt) THEN
    bio_ms(ihru) = rootMass + dryBioMass
ELSEIF ( sdate .gt. jdharv) THEN
    bio_ms(ihru) = 0
    dryBioMass = 0
    rootMass = 0
ENDIF


!! Uncomment the following line to check biomass calculations
!write(*,*)  jyear,sdate,jdharv,jdplt,dryBioMass,rootMass,bio_ms(ihru)


!! COMMENT NEEDED
nro(ihru) = curyr


!! Uncomment the following lines to check fertilizer applications
!IF (mafert > 0) THEN
!    write(*,*) ifert(nro(ihru),nfert(ihru),ihru),ida,mafert
!ENDIF


!! Apply fertilizer-check day and heat units
IF (ida == ifert(nro(ihru),nfert(ihru),ihru)) THEN
!    write(*,*) 'Fertilizer Needed: Fertilizer Subroutine Called'
    CALL fert
!    IF (nfert(ihru) .gt. mafert) THEN    !!DSHI
!        nfert(ihru) = 1
!        EXIT
!    ENDIF
ENDIF


!! DEPRECATED: Heat-based fertilizer application
!IF (igro(j) == 0) THEN
!IF (gplt(j) > phun(nro(j),nfert(j),j)) CALL fert
!IF (phuacc(ihru) > phun(nro(ihru),nfert(ihru),ihru)) CALL fert
!ENDIF


!! Compute soil temperature for soil layers
CALL solt
CALL nup(icrop,strsn)
!IF (strsn <= 0.0) strsn = 0.5
CALL npup(icrop,strsp)
IF (strsp <= 0.0) strsp = strsn


!! Auto fertilization-nitrogen demand (non-legumes only)
!! CODE DEVELOPMENT NEEDED: CASE DEFAULT...
SELECT CASE (idc(icrop))
    CASE (4, 5, 6, 7)
        IF (auto_nstr(ihru) > 0.) CALL anfert(strsn, strsp)
END SELECT


!! Set nitrogen and phosphorus stress to 1 (no stress) after harvest !RPM is not sure this should occur?
IF (sdate .gt. jdharv) THEN
    strsn = 1.0
    strsp = 1.0
ENDIF


!! COMMENT NEEDED
CALL nminrl(icrop)
CALL nitvol
CALL pminrl
CALL enrsb(iwave)
CALL psed (iwave)


!! COMMENT NEEDED
IF (mapest > 0) THEN
    DO WHILE (ida == ipst(nro(ihru),npest(ihru),ihru))
        CALL apply
        IF (npest(ihru) .gt. mapest) THEN
            npest(ihru) = 1
            EXIT
        ENDIF
    ENDDO
ENDIF


!! DEPRECATED: Heat-based pesticide application
!if (igro(ihru) == 0) THEN
!if (gplt(ihru) > phupst(nro(ihru),nfert(ihru),ihru)) CALL apply
!else
!if (phuacc(ihru)> phupst(nro(ihru),nfert(ihru),ihru)) CALL apply
!ENDIF


!! COMMENT NEEDED
IF (precip >= 2.54) CALL washp  ! Compute pesticide washoff
CALL decay                      ! Compute pesticide degradation
CALL pestlch                    ! Compute pesticide movement in soil


!! COMMENT NEEDED
!IF (surfq > 0. .and. peakr > 1.e-6) THEN
IF (surfq > 0.) THEN
    IF (precip > 0.) THEN
        CALL enrsb(0)
        IF (sedyld > 0.) CALL pesty(0) ! pesticide transported with suspended sediment
        CALL orgn(0)
!        CALL psed(0)
    ENDIF
ENDIF


!! COMMENT NEEDED
CALL nrain                      ! Compute atmospheric wet deposition of nitrate and ammonium
CALL nlch                       ! Compute nitrate movement leaching
CALL solpL                      ! Compute phosphorus movement
!CALL subwq                     ! Compute chl-a, CBOD and dissolved oxygen loadings
CALL sumv                       ! Perform output summarization


!! Set plant uptake to be zero on harvest date
IF (sdate .eq. jdharv) THEN
    ssub(40,ihru) = 0 !nitrogen
    ssub(42,ihru) = 0 !phosphorus
ENDIF


!! Write daily output for chemicals -- DSHI inserted module
CALL writeDailyChemicalOutput(jyear,month,day,strsn,strsp,mxnsl)
CALL writeDailySurfaceChemicalOutput(jyear,month,day,strsn,strsp)
CALL writeDailyChemicalMassBalanceOutput(jyear,month,day,strsn,strsp,strsw,strst)


RETURN
END
