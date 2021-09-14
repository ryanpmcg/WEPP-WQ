      subroutine nminrl(icrop)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily nitrogen and phosphorus
!!    mineralization and immobilization considering fresh organic
!!    material (plant residue) and active and stable humus material

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cmn           |none          |rate factor for humus mineralization on
!!                                 |active organic N
!!    curyr         |none          |current year of simulation
!!    hru_fr(:)     |km**2/km**2   |fraction of watershed area in HRU
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    ihru          |none          |HRU number
!!    ncr(:,:,:)    |none          |land cover code from crop.dat
!!    nn            |none          |number of layers in soil profile
!!    nro(:)        |none          |sequence number of year in rotation
!!    nyskip        |none          |number of years to skip output
!!                                 |summarization and printing
!!    rsdco_pl(:)   |none          |plant residue decomposition coefficient. The
!!                                 |fraction of residue which will decompose in
!!                                 |a day assuming optimal moisture,
!!                                 |temperature, C:N ratio, and C:P ratio
!!    rtn           |none          |nitrogen active pool fraction. The fraction
!!                                 |of organic nitrogen in the active pool.
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_cbn(:,:)  |%             |percent organic carbon in soil layer
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_labp(:,:) |kg P/ha       |amount of phosohorus stored in the labile P
!!                                 |pool
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool in soil layer
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool in soil layer
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil
!!                                 |classified as residue
!!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer on
!!                                 |current day
!!    sol_tmp(:,:)  |deg C         |daily average temperature of soil layer
!!    sol_ul(:,:)   |mm H2O        |amount of water held in the soil layer at
!!                                 |saturation
!!    wshd_dnit     |kg N/ha       |average annual amount of nitrogen lost from
!!                                 |nitrate pool due to denitrification in
!!                                 |watershed
!!    wshd_hmn      |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from active organic to nitrate pool in
!!                                 |watershed
!!    wshd_hmp      |kg P/ha       |average annual amount of phosphorus moving
!!                                 |from organic to labile pool in watershed
!!    wshd_rmn      |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from fresh organic (residue) to nitrate
!!                                 |and active organic pools in watershed
!!    wshd_rmp      |kg P/ha       |average annual amount of phosphorus moving
!!                                 |from fresh organic (residue) to labile
!!                                 |and organic pools in watershed
!!    wshd_rwn      |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from active organic to stable organic pool
!!                                 |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hmntl         |kg N/ha       |amount of nitrogen moving from active
!!                                 |organic to nitrate pool in soil profile
!!                                 |on current day in HRU
!!    hmptl         |kg P/ha       |amount of phosphorus moving from the
!!                                 |organic to labile pool in soil profile
!!                                 |on current day in HRU
!!    rmn2tl        |kg N/ha       |amount of nitrogen moving from the fresh
!!                                 |organic (residue) to the nitrate(80%) and
!!                                 |active organic(20%) pools in soil profile
!!                                 |on current day in HRU
!!    rmptl         |kg P/ha       |amount of phosphorus moving from the
!!                                 |fresh organic (residue) to the labile(80%)
!!                                 |and organic(20%) pools in soil profile
!!                                 |on current day in HRU
!!    rwntl         |kg N/ha       |amount of nitrogen moving from active
!!                                 |organic to stable organic pool in soil
!!                                 |profile on current day in HRU
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_labp(:,:) |kg P/ha       |amount of phosohorus stored in the labile P
!!                                 |pool
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool in soil layer
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool in soil layer
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil
!!                                 |classified as residue
!!    wdntl         |kg N/ha       |amount of nitrogen lost from nitrate pool
!!                                 |by denitrification in soil profile on
!!                                 |current day in HRU
!!    wshd_dnit     |kg N/ha       |average annual amount of nitrogen lost from
!!                                 |nitrate pool due to denitrification in
!!                                 |watershed
!!    wshd_hmn      |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from active organic to nitrate pool in
!!                                 |watershed
!!    wshd_hmp      |kg P/ha       |average annual amount of phosphorus moving
!!                                 |from organic to labile pool in watershed
!!    wshd_rmn      |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from fresh organic (residue) to nitrate
!!                                 |and active organic pools in watershed
!!    wshd_rmp      |kg P/ha       |average annual amount of phosphorus moving
!!                                 |from fresh organic (residue) to labile
!!                                 |and organic pools in watershed
!!    wshd_rwn      |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from active organic to stable organic pool
!!                                 |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ca          |
!!    cdg         |none          |soil temperature factor
!!    cdn         |none          | denitrification coefficient
!!    cnr         |
!!    cnrf        |
!!    cpr         |
!!    cprf        |
!!    csf         |none          |combined temperature/soil water factor
!!    decr        |
!!    hmn         |kg N/ha       |amount of nitrogen moving from active organic
!!                               |nitrogen pool to nitrate pool in layer
!!    hmp         |kg P/ha       |amount of phosphorus moving from the organic
!!                               |pool to the labile pool in layer
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    kk          |none          |soil layer used to compute soil water and
!!                               |soil temperature factors
!!    r4          |
!!    rdc         |
!!    rmn1        |kg N/ha       |amount of nitrogen moving from fresh organic
!!                               |to nitrate(80%) and active organic(20%)
!!                               |pools in layer
!!    rmp         |kg P/ha       |amount of phosphorus moving from fresh organic
!!                               |to labile(80%) and organic(20%) pools in layer
!!    rwn         |kg N/ha       |amount of nitrogen moving from active organic
!!                               |to stable organic pool in layer
!!    sdnco       |none          |denitrification threshold: fraction of field
!!                               | capacity
!!    sut         |none          |soil water factor
!!    wdn         |kg N/ha       |amount of nitrogen lost from nitrate pool in
!!                               |layer due to denitrification
!!    xx          |varies        |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Exp, Sqrt, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

cd      real, parameter :: cdn = -1.4, sdnco = 1.1
      real :: cdn, sdnco
      integer :: j, k, kk, icrop
      real :: rmn1, rmp, xx, csf, rwn, hmn, hmp, r4, cnr, cnrf, cpr
      real :: cprf, ca, decr, rdc, wdn, cdg, sut

      j = 0
      j = ihru
      cdn = 0.8    ! this is denitrification coefficient readin from the file.
      sdnco = 0.8   !  SWAT document 2012 is 0.6 , code 2012 its default value is 1.3.
c       write(*,*) sol_no3(1,1),'soil no3'
      do k = 1, nn
         kk = 0 
        if (k == 1) then
          kk = 2
        else
          kk = k
        end if
c        write(*,*) wdn,'wdn'
        !! mineralization can occur only if temp above 0 deg
        if (sol_tmp(kk,j) > 0.) then
          !! compute soil water factor
          sut = 0.
          sut = .1 + .9 * Sqrt(sol_st(kk,j)/sol_fc(kk,j)) !SWAT code 2012 
c          sut =  sol_st(kk,j) / (sol_ul(kk,j) + 1.e-10)   !!SWAT document2009
          sut = Max(.05, sut)
c          write(*,*) sut,'sut--water factor'         
       
        !!compute soil temperature factor
          xx = 0.
          cdg = 0.
          xx = sol_tmp(kk,j)
c         write(*,*)  xx,xx,'soil temp. factor'
          cdg = 0.9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1 !! SWAT code 2012
          cdg = Max(.1, cdg)
c         write(*,*)  cdg, cdg,'soil temp. factor'
 
        !! compute combined factor
          xx = 0.
          csf = 0.
          xx = cdg * sut
          if (xx < 0.) xx = 0.
          if (xx > 1.e6) xx = 1.e6
          csf = Sqrt(xx)
c          write(*,*)  csf, csf,'combined factor'
c          write(*,*) sol_orgp(k,j),'sol_orgp'
          !! compute flow between active and stable pools
          !! rwn is postive ( from active to stable)
          !! rwn is negative( flow from stable to active)
          rwn = 0.
c          rwn = .1e-4 * sol_aorgn(k,j) * (1. / rtn - 1.) -              &
c     &                                                    sol_orgn(k,j)  !doc2009.
         rwn = .1e-4 * (sol_aorgn(k,j) * (1. / rtn - 1.) -              &
     &                                                    sol_orgn(k,j))  ! SWAT code 2012.

          !! rwn: the equation in code is different from document and SWAT code2012   

          if (rwn > 0.) then                                 !! update to SWAT version 2012
            rwn = Min(rwn, sol_aorgn(k,j))
c           write(*,*) rwn,'from active to stable'
          else
            rwn = -(Min(Abs(rwn), sol_orgn(k,j)))
c           write(*,*) rwn,'from stable to active'
          endif                                              !! update to SWAT version 2012

         sol_orgn(k,j) = Max(1.e-6, sol_orgn(k,j) + rwn)
         sol_aorgn(k,j) = Max(1.e-6, sol_aorgn(k,j) - rwn)
         
 
          !! compute humus mineralization on active organic n
          hmn = 0.
          hmn = cmn * csf * sol_aorgn(k,j) !! cmn is read from soil chemical file, assigned by users.     
          hmn = Min(hmn, sol_aorgn(k,j))
         
c         write(*,*) sol_aorgn(k,j),rtn,'active_org_N,rtn' 

          !! compute humus mineralization on active organic p
          xx = 0.
          hmp = 0.
          xx = sol_orgn(k,j) + sol_aorgn(k,j)
          if (xx > 1.e-6) then
            hmp = 1.4 * hmn * sol_orgp(k,j) / xx
          else
            hmp = 0.
          end if
          hmp = Min(hmp, sol_orgp(k,j))
! move mineralized nutrients between pools         
          sol_aorgn(k,j) = Max(1.e-6, sol_aorgn(k,j) - hmn)
          sol_no3(k,j) = sol_no3(k,j) + hmn
          sol_orgp(k,j) = sol_orgp(k,j) - hmp
          sol_labp(k,j) = sol_labp(k,j) + hmp
      
            
          !! compute residue decomp and mineralization of 
          !! fresh organic n and p (upper two layers only)
          rmn1 = 0.
          rmp = 0.
          if (k <= 2) then
            r4 = 0.
            r4 = .58 * sol_rsd(k,j)

            if (sol_fon(k,j) + sol_no3(k,j) > 1.e-4) then
              cnr = 0.
              cnr = r4 / (sol_fon(k,j) + sol_no3(k,j))
              if (cnr > 500.) cnr = 500.
              cnrf = 0.
              cnrf = Exp(-.693 * (cnr - 25.) / 25.)
            else
              cnrf = 1.
            end if

            if (sol_fop(k,j) + sol_labp(k,j) > 1.e-4) then
              cpr = 0.
              cpr = r4 / (sol_fop(k,j) + sol_labp(k,j))
              if (cpr > 5000.) cpr = 5000.
              cprf = 0.
              cprf = Exp(-.693 * (cpr - 200.) / 200.)
            else
              cprf = 1.
            end if
            ca = 0.
            decr = 0.
            rdc = 0.
            ca = Min(cnrf, cprf, 1.)
c           decr = rsdco_pl(ncr(nro(j),icr(j),j)) * ca * csf
 
c            decr = rsdco_pl(icrop) * ca * csf   
 
          !!DSHI change to above line, TESTING only
          !! above line needs to be improved when OFEs.
c           write(*,*) rsdco_pl(icrop),rsdco_pl(icrop),'rsdco_pl'
           if (icrop > 0) then
              decr =  rsdco_pl(icrop) * ca * csf
           else
                decr = 0.05
           end if 
        
            decr = Max(.0001, decr)
            decr = Min(decr, 1.)
            sol_rsd(k,j) = amax1( 1.e-6,sol_rsd(k,j))
            rdc = decr * sol_rsd(k,j)
            sol_rsd(k,j) = sol_rsd(k,j) - rdc
    
            if (sol_rsd(k,j) < 0.) sol_rsd(k,j) = 0.
            rmn1 = decr * sol_fon(k,j)
            sol_fop(k,j) = amax1(1.e-6,sol_fop(k,j) - rmp)
            rmp = decr * sol_fop(k,j)
            sol_fop(k,j) = sol_fop(k,j) - rmp
            sol_fon(k,j) = amax1( 1.e-6,sol_fon(k,j))
            sol_fon(k,j) = sol_fon(k,j) - rmn1
            sol_no3(k,j) = sol_no3(k,j) + .8 * rmn1
            sol_aorgn(k,j) = sol_aorgn(k,j) + .2 * rmn1
            sol_labp(k,j) = sol_labp(k,j) + .8 * rmp
            sol_orgp(k,j) = sol_orgp(k,j) + .2 * rmp         
           end if              

          !! compute denitrification 
          wdn = 0.           

          if (sut >= sdnco) then
            ll =  cdn * cdg * sol_cbn(k,j)
            
            hh = 1. - 1/exp(cdn*cdg*sol_cbn(k,j))
            wdn = sol_no3(k,j) * hh
c            write(*,*) sol_cbn(k,j),hh,ll,wdn,'NO3_DEN'                     
          else
            wdn = 0.
          end if
c         write(*,*) sol_no3(k,j),'NO3' 
         sol_no3(k,j) = sol_no3(k,j) - wdn  

c         if (wdn > 0 ) then
c          write(*,*) wdn,'DEN'        
c         end if
        !! summary calculations
          if (curyr > nyskip) then
            wshd_hmn = wshd_hmn + hmn * hru_fr(j)
            wshd_rwn = wshd_rwn + rwn * hru_fr(j)
            wshd_hmp = wshd_hmp + hmp * hru_fr(j)
            wshd_rmn = wshd_rmn + rmn1 * hru_fr(j)
            wshd_rmp = wshd_rmp + rmp * hru_fr(j)
            wshd_dnit = wshd_dnit + wdn * hru_fr(j)
            hmntl = hmntl + hmn
            rwntl = rwntl + rwn
            hmptl = hmptl + hmp
            rmn2tl = rmn2tl + rmn1
            rmptl = rmptl + rmp
            wdntl = wdntl + wdn
       
          end if
 
        end if
      end do

   
      return
      end
