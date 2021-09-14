      subroutine fert
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies N and P specified by date and
!!    amount in the management file (.mgt)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition                  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkdf(:,:,:)|none          |fraction of bacteria in solution (the
!!                                 |remaining fraction is sorbed to soil
!!                                 |particles)A
!!    bactlpf(:,:,:)|# bact/kg frt |concentration of less persistent bacteria
!!                                 |in fertilizer
!!    bactpf(:,:,:) |# bact/kg frt |concentration of persistent bacteria in
!!                                 |fertilizer
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil 
!!                                 |particles
!!    curyr         |none          |current year of simulation
!!    fertn         |kg N/ha       |total amount of nitrogen applied to soil
!!                                 |in HRU on day
!!    fertp         |kg P/ha       |total amount of phosphorus applied to soil
!!                                 |in HRU on day
!!    frminn(:,:,:) |kg minN/kg frt|fraction of fertilizer that is mineral N
!!                                 |(NO3 + NH4)
!!    frminp(:,:,:) |kg minP/kg frt|fraction of fertilizer that is mineral P
!!    frnh3n(:,:,:) |kgNH3-N/kgminN|fraction of mineral N in fertilizer that
!!                                 |is NH3-N
!!    frorgn(:,:,:) |kg orgN/kg frt|fraction of fertilizer that is organic N
!!    frorgp(:,:,:) |kg orgP/kg frt|fraction of fertilizer that is organic P
!!    frt_kg(:,:,:) |kg/ha         |amount of fertilizer applied to HRU
!!    frt_ly1(:,:,:)|none          |fraction of fertilizer which is applied to
!!                                 |the top 10 mm of soil (the remaining
!!                                 |fraction is applied to first soil layer)
!!    hru_fr(:)     |km2/km2       |fraction of watershed area in HRU
!!    ihru          |none          |HRU number
!!    nfert(:)      |none          |sequence number of fertilizer application
!!                                 |within the year
!!    nro(:)        |none          |sequence number of year in rotation
!!    nyskip        |none          |number of years to not print/summarize output
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_labp(:,:) |kg P/ha       |amount of phosohorus stored in the labile P
!!                                 |pool
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                                 |in soil layer
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool
!!    wshd_fminp    |kg P/ha       |average annual amount of mineral P applied
!!                                 |in watershed
!!    wshd_fnh3     |kg N/ha       |average annual amount of NH3-N applied in
!!                                 |watershed
!!    wshd_fno3     |kg N/ha       |average annual amount of NO3-N applied in
!!                                 |watershed
!!    wshd_orgn     |kg N/ha       |average annual amount of organic N applied
!!                                 |in watershed
!!    wshd_orgp     |kg P/ha       |average annual amount of organic P applied
!!                                 |in watershed
!!    wshd_ftotn    |kg N/ha       |average annual amount of N (mineral & 
!!                                 |organic) applied in watershed
!!    wshd_ftotp    |kg P/ha       |average annual amount of P (mineral &
!!                                 |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha|less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha|less persistent bacteria attached to soil
!!                                |particles
!!    bactpq(:)     |# colonies/ha|persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha|persistent bacteria attached to soil 
!!                                |particles
!!    fertn         |kg N/ha      |total amount of nitrogen applied to soil
!!                                |in HRU on day
!!    fertp         |kg P/ha      |total amount of phosphorus applied to soil
!!                                |in HRU on day
!!    nfert(:)      |none         |sequence number of fertilizer application
!!                                |within the year
!!    sol_aorgn(:,:)|kg N/ha      |amount of nitrogen stored in the active
!!                                |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha      |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha      |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_labp(:,:) |kg P/ha      |amount of phosohorus stored in the labile P
!!                                |pool
!!    sol_nh3(:,:)  |kg N/ha      |amount of nitrogen stored in the ammonium
!!                                |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha      |amount of nitrogen stored in the nitrate pool
!!                                |in soil layer
!!    sol_orgp(:,:) |kg P/ha      |amount of phosphorus stored in the organic
!!                                |P pool
!!    wshd_fminp    |kg P/ha      |average annual amount of mineral P applied
!!                                |in watershed
!!    wshd_fnh3     |kg N/ha      |average annual amount of NH3-N applied in
!!                                |watershed
!!    wshd_fno3     |kg N/ha      |average annual amount of NO3-N applied in
!!                                |watershed
!!    wshd_orgn     |kg N/ha      |average annual amount of organic N applied
!!                                |in watershed
!!    wshd_orgp     |kg P/ha      |average annual amount of organic P applied
!!                                |in watershed
!!    wshd_ftotn    |kg N/ha      |average annual amount of N (mineral & 
!!                                |organic) applied in watershed
!!    wshd_ftotp    |kg P/ha      |average annual amount of P (mineral &
!!                                |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units        |definition                  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j            |none         |HRU number
!!    l            |none         |counter (soil layer #)
!!    rtof         |none         |weighting factor used to partition the 
!!                               |organic N & P content of the fertilizer
!!                               |between the fresh organic and the active 
!!                               |organic pools
!!    xx           |none         |fraction of fertilizer applied to layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

cd      real, parameter :: rtof=0.5
      real :: rtof
      integer :: j, l
      real :: vv, xx, yy, zz,hh

      j = 0
      j = ihru
      rtof=0.5

      do l = 1, 2
        vv = 0.
        xx = 0.
        yy = 0.
        zz = 0.
        if (l == 1) then
          xx = frt_ly1(nro(j),nfert(j),j)
        else
          xx = 1. - frt_ly1(nro(j),nfert(j),j)
        endif
           
c        xx = 0.2
        sol_no3(l,j) = sol_no3(l,j) + xx * frt_kg(nro(j),nfert(j),j) *  &
     &      (1. - frnh3n(nro(j),nfert(j),j)) * frminn(nro(j),nfert(j),j)


         write(*,*) xx,sol_no3(1,1),'1,sol_no3',sol_no3(2,1),'2,sol_no3'
         write(*,*)frminn(nro(j),nfert(j),j),frnh3n(nro(j),nfert(j),j)

        if (fcompost(nro(j),nfert(j),j) > 0) then                       !! DSHI modified for implementing compost application
          yy = xx*frt_kg(nro(j),nfert(j),j) * frorgn(nro(j),nfert(j),j)
          sol_fon(l,j) = sol_fon(l,j) + rtof * yy * rtn
          sol_aorgn(l,j) = sol_aorgn(l,j) + (1. - rtof) * yy * rtn
          sol_orgn(l,j) = sol_orgn(l,j) + yy * (1. - rtn)
        else                                                            !! DSHI no compost application
          sol_fon(l,j) = sol_fon(l,j) + rtof * xx *                     &
     &        frt_kg(nro(j),nfert(j),j) * frorgn(nro(j),nfert(j),j)
          sol_aorgn(l,j) = sol_aorgn(l,j) + (1. - rtof) * xx *          &
     &      frt_kg(nro(j),nfert(j),j) * frorgn(nro(j),nfert(j),j)
        endif

        sol_nh3(l,j) = sol_nh3(l,j) + xx * frt_kg(nro(j),nfert(j),j) *  &
     &      frnh3n(nro(j),nfert(j),j) * frminn(nro(j),nfert(j),j)

        if (fcompost(nro(j),nfert(j),j) > 0) then                       !! DSHI modified for implementing compost application
          yy = 0.125*frorgn(nro(j),nfert(j),j)*frt_kg(nro(j),nfert(j),j)
          if (yy>(frminp(nro(j),nfert(j),j)*frt_kg(nro(j),nfert(j),j)))
     &      yy = 0.85 * yy

          sol_fop(l,j) = sol_fop(l,j) + rtof * xx * yy
          fertp = fertp + rtof * xx * yy

          sol_orgp(l,j) = sol_orgp(l,j) + (1. - rtof) * xx * yy
          fertp = fertp + (1. - rtof) * xx * yy

	    vv = (1.0 - psp) / psp
	    zz = 1.0 + vv + 4.0 * vv**2.0
          
          sol_labp(l,j) = sol_labp(l,j) + xx*(frminp(nro(j),nfert(j),j) &
     &      * frt_kg(nro(j),nfert(j),j) - yy) / zz

          fertp = fertp + xx*(frminp(nro(j),nfert(j),j)                 &
     &      * frt_kg(nro(j),nfert(j),j) - yy) / zz

          sol_actp(l,j) = sol_actp(l,j) +(xx*(frminp(nro(j),nfert(j),j) &
     &      * frt_kg(nro(j),nfert(j),j) - yy) / zz) * vv
          fertp = fertp +(xx*(frminp(nro(j),nfert(j),j)                 &
     &      * frt_kg(nro(j),nfert(j),j) - yy) / zz) * vv

          sol_stap(l,j) = sol_stap(l,j)+4*(xx*(frminp(nro(j),nfert(j),j)
     &      * frt_kg(nro(j),nfert(j),j) - yy) / zz) * vv
          fertp = fertp +4*(xx*(frminp(nro(j),nfert(j),j)               &
     &      * frt_kg(nro(j),nfert(j),j) - yy) / zz) * vv
        else                                                            !! DSHI no compost application
          sol_labp(l,j) = sol_labp(l,j) + xx*frt_kg(nro(j),nfert(j),j)* &
     &      frminp(nro(j),nfert(j),j)
          fertp = fertp + xx * frt_kg(nro(j),nfert(j),j) *              &
     &      frminp(nro(j),nfert(j),j)
c           write(*,*) sol_labp(l,j),l,j,'sol p, l, j'
          sol_fop(l,j) = sol_fop(l,j) + rtof * xx *                     &
     &      frt_kg(nro(j),nfert(j),j) * frorgp(nro(j),nfert(j),j)
          fertp = fertp + rtof * xx *                                   &
     &      frt_kg(nro(j),nfert(j),j) * frorgp(nro(j),nfert(j),j)

          sol_orgp(l,j) = sol_orgp(l,j) + (1. - rtof) * xx *            &
     &      frt_kg(nro(j),nfert(j),j) * frorgp(nro(j),nfert(j),j)
          fertp = fertp + (1. - rtof) * xx *                            &
     &      frt_kg(nro(j),nfert(j),j) * frorgp(nro(j),nfert(j),j)
        endif

      end do

!! summary calculations
      fertn = fertn + frt_kg(nro(j),nfert(j),j) *                       &
     &   (frminn(nro(j),nfert(j),j) + frorgn(nro(j),nfert(j),j))

cd      fertp = fertp + frt_kg(nro(j),nfert(j),j) *                       &
cd     &   (frminp(nro(j),nfert(j),j) + frorgp(nro(j),nfert(j),j))

      if (curyr > nyskip) then
      wshd_ftotn = wshd_ftotn + frt_kg(nro(j),nfert(j),j) * hru_fr(j) * &
     &   (frminn(nro(j),nfert(j),j) + frorgn(nro(j),nfert(j),j))

      wshd_forgn = wshd_forgn + frt_kg(nro(j),nfert(j),j) * hru_fr(j) * &
     &   frorgn(nro(j),nfert(j),j)

      wshd_fno3 = wshd_fno3 + frt_kg(nro(j),nfert(j),j) * hru_fr(j) *   &
     &   frminn(nro(j),nfert(j),j) * (1. - frnh3n(nro(j),nfert(j),j))

      wshd_fnh3 = wshd_fnh3 + frt_kg(nro(j),nfert(j),j) * hru_fr(j) *   &
     &   frminn(nro(j),nfert(j),j) * frnh3n(nro(j),nfert(j),j)

      wshd_ftotp = wshd_ftotp + frt_kg(nro(j),nfert(j),j) * hru_fr(j) * &
     &   (frminp(nro(j),nfert(j),j) + frorgp(nro(j),nfert(j),j))

      wshd_fminp = wshd_fminp + frt_kg(nro(j),nfert(j),j) * hru_fr(j) * &
     &   frminp(nro(j),nfert(j),j)

      wshd_forgp = wshd_forgp + frt_kg(nro(j),nfert(j),j) * hru_fr(j) * &
     &   frorgp(nro(j),nfert(j),j)
      end if
   
 
!! increase fertilizer sequence number by one
      nfert(j) = nfert(j) + 1
        
      return
      end
