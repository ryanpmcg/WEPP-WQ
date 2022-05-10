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
!!    + refactored, converted to free-source format
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine initializes variables for the daily simulation of the land phase of the hydrologic cycle
!!
!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    -----------------------------------------------------------------------
!!    name                          |units          |definition
!!    -----------------------------------------------------------------------
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert application
!!    no3gw       |kg N/ha       |nitrate loading to reach in groundwater
!!    no3pcp      |kg N/ha       |nitrate added to the soil in rainfall
!!    numsl       |none          |number of soil layers in simulation (fixed based on mxnsl from WEPP)
!!    pst_sol     |              |
!!    -----------------------------------------------------------------------
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE initializeObjects

    !! Load parameter module
    USE parm

    !! Print status update
    IF (verbose .EQV. .TRUE.) WRITE (*,*) "            Initializing Objects"

!!  Included variables documented as follows
!!  Variable                             |Unit              |Description
!!  -----------------------------------------------------------------------

    afminn = 0.0                        !|                  |
    afminp = 0.0                        !|                  |
    afnh4n = 0.0                        !|                  |
    aforgn = 0.0                        !|                  |
    aforgp = 0.0                        !|                  |
    afrt_ly1 = 0.0                      !|                  |
    anion_excl = 0.0                    !|                  |
    ap_ef = 0.0                         !|                  |
    auto_eff = 0.0                      !|                  |
    auto_nmxa = 0.0                     !|                  |
    auto_nmxs = 0.0                     !|                  |
    auton = 0.0                         !|                  |
    autop = 0.0                         !|                  |
    bio_e = 0.0                         !|                  |
    bio_ms = 0.0                        !|                  |
    bio_n1 = 0.0                        !|                  |
    bio_n2 = 0.0                        !|                  |
    bio_p1 = 0.0                        !|                  |
    bio_p2 = 0.0                        !|                  |
    bn = 0.0                            !|                  |
    bp = 0.0                            !|                  |
    cnb = 0.0                           !|                  |
    cpt = 0.0                           !|                  |
    decay_f = 0.0                       !|                  |
    decay_s = 0.0                       !|                  |
    dkf = 0.0                           !|                  |
    dkg = 0.0                           !|                  |
    fcompost = 0.0                      !|                  |
    fminn = 0.0                         !|                  |
    fminp = 0.0                         !|                  |
    fnh4n = 0.0                         !|                  |
    forgn = 0.0                         !|                  |
    forgp = 0.0                         !|                  |
    frminn = 0.0                        !|                  |
    frminp = 0.0                        !|                  |
    frnh4n = 0.0                        !|                  |
    frorgn = 0.0                        !|                  |
    frorgp = 0.0                        !|                  |
    frt_kg = 0.0                        !|                  |
    frt_ly1 = 0.0                       !|                  |
    henry = 0.0                         !|                  |
    hlife_f = 0.0                       !|                  |
    hlife_s = 0.0                       !|                  |
    iafer = 0                           !|                  |
    idc = 0                             !|                  |
    ifert = 0                           !|                  |
    ipest = 0                           !|                  |
    ncr = 0                             !|                  |
    nops = 0                            !|                  |
    no3pcp = 0.0                        !|                  |
    pab = 0.0                           !|                  |
    pdb = 0.0                           !|                  |
    percp = 0.0                         !|                  |
    phuacc = 0.0                        !|                  |
    plt_pst = 0.0                       !|                  |
    pst_enr = 0.0                       !|                  |
    pst_kg = 0.0                        !|                  |
    skoc = 0.0                          !|                  |
    sol_actp = 0.0                      !|                  |
    sol_aorgn = 0.0                     !|                  |
    sol_fon = 0.0                       !|                  |
    sol_fop = 0.0                       !|                  |
    sol_kp = 0.0                        !|                  |
    sol_labp = 0.0                      !|                  |
    sol_no3 = 0.0                       !|                  |
    sol_orgn = 0.0                      !|                  |
    sol_orgp = 0.0                      !|                  |
    sol_pst = 0.0                       !|                  |
    sol_stap = 0.0                      !|                  |
    ssb = 0.0                           !|                  |
    ssub = 0.0                          !|                  |
    wof = 0.0                           !|                  |
    wsol = 0.0                          !|                  |

    !! Non-zero variable initializations
    auto_nstr = -1.0                    !|                  |
    hru_fr = 1.0                        !|                  |
    icr = 1                             !|                  |
    ipst = 400                          !|                  |
    pname = ""                          !|                  |
    title = ""                          !|                  |
    rsdco_pl = 0.05                     !|                  |

    RETURN

END
