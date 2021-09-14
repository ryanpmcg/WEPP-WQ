      subroutine initial

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine zeros values for single array variables

      use parm

cd      basminpf = 0.
      basminpi = 0.
cd      basno3f = 0.
      basno3i = 0.
cd      basorgnf = 0.
      basorgni = 0.
cd      basorgpf = 0.
      basorgpi = 0.
cd      crco = 0.1
cd      dtot = 0.0
cd      icrk = 0
cd      ifirsth = 1
cd      ifirstp = 1
cd      ifirsts = 1
cd      ifirstw = 1
cd      immo = 0
cd      iwatable = 0
cd      mo_chk = 0
cd      n4 = 1
cd      n21 = 1
cd      n60 = 1
cd      n90 = 1
      nhru = 0
      npmx = 0    !! DSHI changed from 1 to 0
cd      pest_henry = 0.
cd      pest_sol = 0.
cd      petfile = ""
cd      sbactlchlp = 0.
cd      sbactlchp = 0.
cd      sbactrolp = 0.
cd      sbactrop = 0.
cd      sbactsedlp = 0.
cd      sbactsedp = 0.
cd      sdiegrolpq = 0.
cd      sdiegrolps = 0.
cd      sdiegropq = 0.
cd      sdiegrops = 0.
cd      sno3up = 0.
cd      spadyev = 0.
cd      spadyo = 0.
cd      spadyrfv = 0.
cd      spadysp = 0.
cd      volcrmin = 0.
      wshd_dnit = 0.
      wshd_fixn = 0.
      wshd_fminp = 0.
      wshd_fnh3 = 0.
      wshd_fno3 = 0.
      wshd_forgn = 0.
      wshd_forgp = 0.
      wshd_ftotn = 0.
      wshd_ftotp = 0.
      wshd_hmn = 0.
      wshd_hmp = 0.
      wshd_nitn = 0.
cd      wshd_nstrs = 0.
      wshd_pal = 0.
      wshd_pas = 0.
cd      wshd_plch = 0.
cd      wshd_pndfr = 0.
cd      wshd_pndha = 0.
cd      wshd_pndsed = 0.
cd      wshd_pndv = 0.
cd      wshd_pstrs = 0.
cd      wshd_pup = 0.
cd      wshd_raino3 = 0.
cd      wshd_resfr = 0.
cd      wshd_resha = 0.
cd      wshd_ressed = 0.
cd      wshd_resv = 0.
      wshd_rmn = 0.
      wshd_rmp = 0.
      wshd_rwn = 0.
cd      wshd_snob = 0.
cd      wshd_sw = 0.
cd      wshd_tstrs = 0.
      wshd_voln = 0.
cd      wshd_wetfr = 0.
cd      wshd_wstrs = 0.
cd      wshd_yldn = 0.
      wshd_yldp = 0.

cd      do j = 1, mhru
cd        rnd2(j) = Aunif(rndseed(idg(2),j))
cd        rnd3(j) = Aunif(rndseed(idg(3),j))
cd        rnd6(j) = Aunif(rndseed(idg(6),j))
cd        rnd8(j) = Aunif(rndseed(idg(8),j))
cd        rnd9(j) = Aunif(rndseed(idg(9),j))
cd      end do

      return
      end
