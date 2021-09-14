!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    ~ ~ ~ CODE DOCUMENTATION ~ ~ ~
!!    -----------------------------------------------------------------------
!!    description       |date           |names
!!    -----------------------------------------------------------------------
!!    Original Code     |2011-??        |Reza Savabi (from SWAT)
!!    Modification      |2021-08        |Ryan McGehee (original code)
!!    -----------------------------------------------------------------------
!!
!!    RPM Modification:
!!    + converted to free-source format
!!    + added new deposition declarations
!!    + refactored code, updated documentation, added comments
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this module declares variables for WEPP-WQ including:
!!      + real, integer, and character scalars
!!      + real, integer, and character arrays
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE PARM

!! List of Related Parameters
!! mrg = maximum number of rainfall/temperature gages
!! mch = maximum number of channels
!! mbb = maximum number of subbasins
!! ml = maximum number of soil layers
!! mr = maximum number of reservoirs
!! mpdb = maximum number of pesticides in the database
!! mcrdb = maximum number of crops in database
!! mfdb = maximum number of fertilizer in database
!! ma = maximum number of applications
!! mnr = maximum number years of rotation
!! mtil = maximum number tillages in database
!! mhyd = maximum number of hydrograph nodes
!! mhru = maximum number of hydrologic response units


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                            !!
!!                        SCALAR VARIABLE DECLARATIONS                        !!
!!                                                                            !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! Declare all real scalar variables
REAL :: Ninitial, soilN, Nloss, Ngain, Nbalance, Pinitial, soilP, Ploss, Pgain, Pbalance, PsTinitial, soilPsT, PsTloss, PsTgain,  &
        PsTbalance, percop, wshd_fminp, wshd_ftotn, wshd_fnh3, wshd_fno3, wshd_forgn, wshd_forgp, wshd_ftotp, wshd_yldn,          &
        wshd_yldp, wshd_fixn, wshd_pup, wshd_hmn, wshd_rwn, wshd_hmp, wshd_rmn, wshd_dnit, wshd_rmp, wshd_voln, wshd_nitn,        &
        wshd_pas, wshd_pal, alb, wshd_plch, wshd_raino3, basno3f, basorgnf, basno3i, basorgni, basminpi, basorgpi, surfq, fixn,   &
        sedyld, percn, nplnt, pplnt, percp, sedorgn, sedorgp, surqno3, surqsolp, fertn, fertp, precip, nperco, pperco, rsdco,     &
        phoskd, uno3d, rcno3, rcno3i, rcno3f, bactkdq, thbact, wpq20, wlpq20, wps20, wlps20, enratio, da_ha, wdpq, wgpq, wdlpq,   &
        wdps, wgps, wdlps, wglps, psp, no3pcp, sedminp, uobw, ubw, ubn, uobn, ubp, uobp, rmptl, wdntl, rmp1tl, roctl, autop,      &
        auton, hmntl, rwntl, hmptl, rmn2tl, rtn, cmn, wglpq, rcnh4, rcnh4i, rcnh4f

!! Declare all integer scalar variables
INTEGER :: mcr, mbb, mpdb, mcrdb, mfdb, mhru, ma,mafert, mapest,mnr, mp, ml, msubo, mrcho, nn, nhru, nbyr, ihru, npmx, irtpest,   &
           curyr, totyr, nyskip, method, ida, idaf, idal, leapyr, mbsbo, mstdo, i, ipd, lubtot, itotr, itotb, itots, n4, iprn, iprp

!! Declare all character scalar variables
CHARACTER(LEN=80) :: prog


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                            !!
!!                             ARRAY DECLARATIONS                             !!
!!                                                                            !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! Declare all real 1-D arrays
REAL, DIMENSION (:), ALLOCATABLE :: skoc, wsol, wof, ap_ef, hlife_f, hlife_s, henry, decay_f, decay_s, rcno3arr, wac21, wac22,    &
                                    cnyld, rsdco_pl, wsyf, vpth, leaf1, leaf2, t_base, t_opt, hvsti, bio_e, vpd2, gsi, chtmx,     &
                                    wavp, cvm, blai, dlai, rdmx, cpyld, anion_excl, sol_avbd, pstsol, pab, pdb, ssb, bio_n1,      &
                                    bio_n2, bio_p1, bio_p2, forgn, forgp, fminn, bactpdb, fminp, fnh3n, bactlpdb, bactkddb, cnb,  &
                                    cpt, erorgn, erorgp, phuacc, sol_cov, alai, bio_ms, pnd_sed, pnd_nsed, afrt_ly1, auto_nstr,   &
                                    hru_fr, auto_nmxs, afminn, auto_nmxa, afminp, aforgn, aforgp, afnh3n, bactpq, auto_eff,       &
                                    sol_sw, bactps, tmpav, hru_ra, tmx, tmn, tmp_an, anano3, sol_sumfc, snup, spup, rcnh4arr

!! Declare all real 2-D arrays
REAL, DIMENSION (:,:), ALLOCATABLE :: sol_labp, sol_ul, sol_fc, sol_z, sol_bd, sol_st, sol_nh3, sol_orgn, sol_por, sol_orgp,      &
                                      sol_wpmm, sol_cbn, sol_no3, dkg, dkf, pst_enr, zdb, pst_surq, plt_pst, pst_sed, sda, ssub,  &
                                      sol_fon, sol_rsd, sol_fop, bn, bp, sol_aorgn, sol_tmp, sol_awc, sol_prk, sol_actp, sol_stap,&
                                      conv_wt

!! Declare all real 3-D arrays
REAL, DIMENSION (:,:,:), ALLOCATABLE :: sol_pst, sol_kp, frt_kg, fcompost, frt_ly1, pst_kg, frorgn, frorgp, frnh3n, frminn,       &
                                        frminp, tnyld, tnylda

!! Declare all integer 1-D arrays
INTEGER, DIMENSION (:), ALLOCATABLE :: values, ndays, hrutot, nope, pstflg, idc, hrupest, nro, nrot, nfert, npest, icr, sol_nly,  &
                                       nafert, hru_sub, npno

!! Declare all integer 3-D arrays
INTEGER, DIMENSION (:,:,:), ALLOCATABLE :: ifert, ipst, iafer, ncr, ipest, ikill, ihvo, ncrops

!! Declare all character arrays
CHARACTER(LEN=4), DIMENSION (:), ALLOCATABLE :: cpnm
CHARACTER(LEN=8), DIMENSION (100) :: fertnm
CHARACTER(LEN=16), DIMENSION (300) :: pname
CHARACTER(LEN=4) :: title(60)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                            !!
!!                                CODE ARCHIVE                                !!
!!                                                                            !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! IMPORTANT NOTE: SOME OF THE FOLLOWING DECLARATIONS MAY NEED TO BE SPLIT INTO TWO COMMANDS AS THEY MAY EXCEED LINE CONTINUATION LIMITS !!

!REAL :: bactrolp, bactsedlp, sno3up, sdiegropq, sdiegrolpq, sdiegrops, sdiegrolps, sbactrop, sbactrolp, sbactsedp, sbactsedlp,    &
!        sbactlchp, sbactlchlp, psp, basminpf, basorgpf, sftmp, smtmp, smfmx, smfmn, timp, wshd_wstrs, wshd_nstrs, wshd_pstrs,     &
!        wshd_tstrs, tloss, inflpcp, snomlt, snofall, strstmp, percn, qtile, crk, latq, latlyr, sepbtm, nplnt, pplnt, sedorgn,     &
!        sedorgp, latno3, surqno3, surqsolp, fertn, thbact, wpq20, wlpq20, wps20, wlps20, bactrop, bactsedp, bactlchp, bactlchlp,  &
!        enratio, bsprev, bssprev, crco, spadyo, spadyev, spadysp, spadyrfv, strsp, strsn, mumax, lambda0, lambda1, lambda2, k_l,  &
!        k_n, k_p, p_n, rmptl, wdntl, rtn, cmn, rmp1tl, roctl, gwseep, revapday, sorpesto, volat, spcon, spexp, solpesti, sorpesti,&
!        cin, pest_sol,pest_henry, rdd, swtemp, chla_ini, dq, hsedyld

!INTEGER :: mtil, ign, mvaro, mrecd, idist, mudb, mrecm, ifirstp, idt, nstep

!CHARACTER(LEN=8) :: date
!CHARACTER(LEN=10) :: time
!CHARACTER(LEN=5) :: zone
!CHARACTER(LEN=13) :: slrfile, wndfile, rhfile, petfile

!REAL, DIMENSION (:), ALLOCATABLE :: flwin, flwout, bankst, flowfr, cpmax4, cpmax21, cpmax60, cpmax90, pmass, flow, drift,         &
!                                    ch_revap, alpha_bnk, alpha_bnke, sub_minp, sub_sumfc, sub_gwno3, sub_gwsolp, sub_km, sub_tc,  &
!                                    sub_sedp, wlat, wsml, wqd, wyd, wtloss, sub_sw, wyno3, wssfn, wysp, wsubp, welev, sub_orgn,   &
!                                    sub_orgp, wchl, wcbod, wdisox, wsolp, wsorp, wyon, wyph, wbactp, wbactlp, sub_wtmp, br1,      &
!                                    res_k, lkpst_conc, res_evol, res_pvol, res_vol, res_psa, lkpst_rea, lkpst_vol, br2, res_rr,   &
!                                    res_sed, lkpst_koc, lkpst_stl, lkpst_rsp, lkpst_mix, lkspst_conc, lkspst_rea, lkspst_bry,     &
!                                    lkspst_act, wurtnf, res_nsed, resdata, chlar, res_orgn, res_orgp, res_no3, res_solp, res_chla,&
!                                    res_seci, res_esa, seccir, res_no2, res_nh3, bio_n1, bio_n2, bio_p1, bio_p2, flocnst, sedcnst,&
!                                    orgncnst, orgpcnst, no3cnst, minpcnst, nh3cnst, no2cnst, bactpcnst, cmtl1cnst, cmtl2cnst,     &
!                                    bactlpcnst, cmtl3cnst, effmix, deptil, rnum1s, hyd_dakm, pot_fr, pot_tile, pot_vol, potsa,    &
!                                    pot_volx, potflwi, potsedi, wfsh, pot_nsed, pot_no3l, skpot, newrti, fsred, filterw, esco,    &
!                                    epco, cnb, cpt, slsubbsn, flowmin, divmax, canmx, usle_p, lat_sed, rch_dakm, pnd_no3s, cn2,   &
!                                    lat_ttime, ch_cov, ch_di, ch_erod, phuacc, slope, gwno3, slsoil, sed_stl, gwminp, yldanu,     &
!                                    dmanu, pnd_solp, pnd_no3, driftco, pnd_orgp, pnd_orgn, ch_si, bio_ms, sol_alb, strsw, pnd_fr, &
!                                    pnd_psa, pnd_pvol, pnd_k, pnd_esa, pnd_evol, pnd_vol, chla, wet_fr, wet_nsa, wet_nvol, wet_k, &
!                                    wet_mxsa, wet_mxvol, wet_vol, wet_sed, wet_nsed, auto_nstr, smx, sci, bp1, bp2, auto_nmxs,    &
!                                    bw1, bw2, afminn, auto_nmxa, secci, afminp, aforgn, aforgp, afnh3n, cht, co2, u10, rhd, elev, &
!                                    tmp_lo, rwt, olai, usle_k, tconc, hru_rmx, anano3, aird, t_ov, sol_sumfc, sol_avpor,          &
!                                    usle_mult, wet_orgp, rock, silt, sub_pet, aairr, snup, sdtsav, shallirr, deepirr, canstor,    &
!                                    ovrlnd, ch_l1, ch_l2, chpst_conc, chpst_rea, chpst_vol, chpst_koc, chpst_stl, chpst_rsp,      &
!                                    chpst_mix, sedpst_conc, wet_no3, sedpst_rea, sedpst_bry, wet_solp, sedpst_act, wet_no3s,      &
!                                    wet_chla, spup, wet_seci, pnd_no3g, chside, gwht, delay, gw_q, pnd_solpg, alpha_bf, alpha_bfe,&
!                                    gw_spyld, gw_delaye, gw_revap, rchrg_dp, rchrg, yldaa, dmaa, xlaia, xlai, lat, ffc, latcos,   &
!                                    latsin, wet_sedp, cklsp, deepst, shallst, wet_solpg, swh, swp, rs1, rs2, rs3, rs4, rs5, rs6,  &
!                                    rs7, rk1, rk2, rk3, rk4, rk5, rk6, bc1, bc2, bc3, bc4, ammonian, disolvp, bod, abc, disox,    &
!                                    algae, nitraten, nitriten, bio_min, ssfp, ddrain, gdrain, sol_crk, dayl, tlaps, plaps, snotmp,&
!                                    brt, twash, rnd1, rnd2, rnd3, rnd4, rnd5, rnd6, rnd7, rnd8, rnd9, qmres, sm, smm, smy, ssb,   &
!                                    aabv, hrtwtr, hhstor, hdepth, hsdti, rk7, ff1, ff2, ff0, ffe

!REAL, DIMENSION (:,:), ALLOCATABLE :: prisk, qrisk, prisk21, qrisk21, prisk60, qrisk60, prisk90, qrisk90, srch, syrch, strch,     &
!                                      rchdy, sbsub, sybsub, stbsub, sub_pst, flat, sol_nh3, sres, syres, stres, wuresn, starg,    &
!                                      oflowmx, oflowmn, psetlr, nsetlr, floyr, sedyr, orgnyr, orgpyr, no3yr, minpyr, nh3yr, no2yr,&
!                                      bactpyr, bactlpyr, cmtl1yr, cmtl2yr, cmtl3yr, varoute, shyd, wvl, crdep, wgncur, wgnold,    &
!                                      rfinc, tmpinc, radinc, wrt, dewpt, ch_w, ch_s, ch_n, pcpband, sda, smo, syr, wupnd, tavband,&
!                                      stot, ssub, sysub, stsub, phi, wurch, wushal, wudeep, tmnband, amp_r, solarav, tmpstdmx,    &
!                                      tmpstdmn, pcf, surf_bs, nsetlw, bss, elevb_fr, tmxband, wtraa, amo

!REAL, DIMENSION (:,:,:), ALLOCATABLE :: resout, bactpf, bactlpf, bactkdf, phupst, hitar, alainit, yldkg, dm2, phu, phup,          &
!                                        auto_wstr, bio_init, auto_rnf, phuho, phuk, yldn, dm2n, phug, bmeat, wmanure, bmtrmp,     &
!                                        flomon, deptill, sedmon, orgnmon, orgpmon, no3mon, minpmon, nh3mon, no2mon, bactpmon,     &
!                                        bactlpmon, cmtl1mon, cmtl2mon, cmtl3mon, hiovr, bio_tar, harveff, pr_w, pcp_stat

!INTEGER, DIMENSION (:), ALLOCATABLE :: ifirstr, idg, icolb, icolr, icolrsv, icols, ifirst, ifirstt, elevp, elevt, idmx4, idmx21,  &
!                                       idmx60, idmx90, iymx4, iymx21, iymx60, iymx90, ndflow, ipdvar, ipdvab, ipdvas, ires1,      &
!                                       ires2, iresco, mores, iyres, res_sub, iflod1r, iflod2r, ndtargr, nop, igro, nair, ipnd1,   &
!                                       ipnd2, nirr, iflod1, iflod2, ndtarg, nmgt, icr, ncut, nsweep, idorm, urblu, ldrain,        &
!                                       iurban, ireg, ismro, ihmro, icodes, ihouts, inum1s, inum2s, inum3s, inum4s

!INTEGER, DIMENSION (:,:), ALLOCATABLE :: rndseed

!INTEGER, DIMENSION (:,:,:), ALLOCATABLE :: iairr, iafer, isweep, irel_imp, irelease, igraz, igftyp, ndgraz

!CHARACTER(LEN=4), DIMENSION (38) :: cpnm
!CHARACTER(LEN=4) :: title(60), cpnm(105)
!CHARACTER(LEN=8), DIMENSION (150) :: tillnm
!CHARACTER(LEN=13), DIMENSION (18) :: rfile,tfile
!CHARACTER(LEN=13), DIMENSION (1000) :: resvo,lwqdat
!CHARACTER(LEN=1), DIMENSION (6300) :: hydgrp
!CHARACTER(LEN=30), DIMENSION (50) :: urbname
!CHARACTER(LEN=16), DIMENSION (6300) :: snam
!CHARACTER(LEN=13) :: heds(65),hedb(15),hedr(42),hedrsv(34)
!CHARACTER(LEN=13) :: hedwtr(40)

END MODULE PARM
