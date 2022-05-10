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
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine applies N and P specified by date and amount in the management file (.mgt)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE fertilizerApplication

    !! Load parameter module
    USE parm

    !! Declare local variables
    INTEGER :: a, b, c
    REAL :: amount, vvv, layerFraction, zzz

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "        Simulating Fertilizer Operation"

    !! Define local variables
    a = simulationYear  !sequence number of year in rotation
    b = nfert(iofe)     !sequence number of fertilizer application within the year
    c = (iofe)          !OFE

    !! Apply fertilizers to the top two soil layers
    !  This can be easily modified by increasing the 'sl' iterator limit and adding another 'layerFraction' condition below
    DO sl = 1,2

        !! Compute fraction of fertilizer applied to each soil layer
        IF (sl == 1) layerFraction = frt_ly1(a,b,c)
        IF (sl == 2) layerFraction = 1.0 - frt_ly1(a,b,c)

        !!! -------------------- MINERAL NITROGEN ADDITIONS -------------------- !!!

        sol_no3(sl,iofe) = sol_no3(sl,iofe) + layerFraction * frt_kg(a,b,c) * (1.0 - frnh4n(a,b,c)) * frminn(a,b,c)
        sol_nh4(sl,iofe) = sol_nh4(sl,iofe) + layerFraction * frt_kg(a,b,c) * frnh4n(a,b,c) * frminn(a,b,c)


        !!! -------------------- ORGANIC NITROGEN ADDITIONS WITH COMPOST -------------------- !!!

        IF (fcompost(a,b,c) > 0) THEN

            ! Compute amounts applied to each pool
            amount = layerFraction*frt_kg(a,b,c)*frorgn(a,b,c)
            freshOrgNApplied(sl,iofe) = rtof(iofe)*amount*rtn(iofe)
            activeOrgNApplied(sl,iofe) = (1.0 - rtof(iofe))*amount*rtn(iofe)
            stableOrgNApplied(sl,iofe) = amount*(1.0 - rtn(iofe))

            ! Update pools
            sol_fon(sl,iofe) = sol_fon(sl,iofe) + freshOrgNApplied(sl,iofe)
            sol_aorgn(sl,iofe) = sol_aorgn(sl,iofe) + activeOrgNApplied(sl,iofe)
            sol_orgn(sl,iofe) = sol_orgn(sl,iofe) + stableOrgNApplied(sl,iofe)


        !!! -------------------- ORGANIC NITROGEN ADDITIONS WITHOUT COMPOST -------------------- !!!

        ELSE

            ! Compute amounts applied to each pool
            freshOrgNApplied(sl,iofe) = rtof(iofe) * layerFraction * frt_kg(a,b,c) * frorgn(a,b,c)
            activeOrgNApplied(sl,iofe) = (1.0 - rtof(iofe)) * layerFraction * frt_kg(a,b,c) * frorgn(a,b,c)
            stableOrgNApplied(sl,iofe) = 0.0

            ! Update pools
            sol_fon(sl,iofe) = sol_fon(sl,iofe) + freshOrgNApplied(sl,iofe)
            sol_aorgn(sl,iofe) = sol_aorgn(sl,iofe) + activeOrgNApplied(sl,iofe)

        ENDIF


        !!! -------------------- ORGANIC AND MINERAL PHOSPHORUS ADDITIONS WITH COMPOST -------------------- !!!

        IF (fcompost(a,b,c) > 0) THEN

            !! COMMENT NEEDED: 'Update nitrogen-phosphorus rate coefficients' may work?
            amount = 0.125*frorgn(a,b,c)*frt_kg(a,b,c)
            vvv = (1.0 - pai(iofe)) / pai(iofe)
            zzz = 1.0 + vvv + 4.0 * vvv**2.0

            !! Compost phosphorus calculations
            IF (amount > (frminp(a,b,c)*frt_kg(a,b,c))) amount = 0.85 * amount
            sol_fop(sl,iofe) = sol_fop(sl,iofe) + rtof(iofe) * layerFraction * amount
            fertp = fertp + rtof(iofe) * layerFraction * amount
            soilActOrgP(sl,iofe) = soilActOrgP(sl,iofe) + (1.0 - rtof(iofe)) * layerFraction * amount
            fertp = fertp + (1.0 - rtof(iofe)) * layerFraction * amount
            sol_labp(sl,iofe) = sol_labp(sl,iofe) + layerFraction*(frminp(a,b,c) * frt_kg(a,b,c) - amount) / zzz
            fertp = fertp + layerFraction*(frminp(a,b,c) * frt_kg(a,b,c) - amount) / zzz
            sol_actp(sl,iofe) = sol_actp(sl,iofe) +(layerFraction*(frminp(a,b,c) * frt_kg(a,b,c) - amount) / zzz) * vvv
            fertp = fertp +(layerFraction*(frminp(a,b,c) * frt_kg(a,b,c) - amount) / zzz) * vvv
            sol_stap(sl,iofe) = sol_stap(sl,iofe)+4*(layerFraction*(frminp(a,b,c) * frt_kg(a,b,c) - amount) / zzz) * vvv
            fertp = fertp +4*(layerFraction*(frminp(a,b,c) * frt_kg(a,b,c) - amount) / zzz) * vvv


        !!! -------------------- ORGANIC AND MINERAL PHOSPHORUS ADDITIONS WITHOUT COMPOST -------------------- !!!

        ELSE

            sol_labp(sl,iofe) = sol_labp(sl,iofe) + layerFraction*frt_kg(a,b,c) * frminp(a,b,c)
            fertp = fertp + layerFraction * frt_kg(a,b,c) * frminp(a,b,c)
            sol_fop(sl,iofe) = sol_fop(sl,iofe) + rtof(iofe) * layerFraction * frt_kg(a,b,c) * frorgp(a,b,c)
            fertp = fertp + rtof(iofe) * layerFraction * frt_kg(a,b,c) * frorgp(a,b,c)
            soilActOrgP(sl,iofe) = soilActOrgP(sl,iofe) + (1.0 - rtof(iofe)) * layerFraction * frt_kg(a,b,c) * frorgp(a,b,c)
            fertp = fertp + (1.0 - rtof(iofe)) * layerFraction * frt_kg(a,b,c) * frorgp(a,b,c)

        ENDIF
    ENDDO

    !! Perform summary calculations
    appliedN(iofe) = appliedN(iofe) + frt_kg(a,b,c) * (frminn(a,b,c) + frorgn(a,b,c))
    appliedP(iofe) = appliedP(iofe) + frt_kg(a,b,c) * (frminp(a,b,c) + frorgp(a,b,c))

    !! Increase fertilizer sequence number by one
    nfert(iofe) = nfert(iofe) + 1
    RETURN
END
