      subroutine pminrl
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes p flux between the labile, active mineral
!!    and stable mineral p pools.     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr        |none          |current year of simulation
!!    hru_fr(:)    |km**2/km**2   |fraction of watershed area in HRU
!!    ihru         |none          |HRU number
!!    nn           |none          |number of layers in soil profile
!!    nyskip       |none          |number of years to skip output summarization
!!                                |and printing
!!    psp          |none          |Phosphorus sorption coefficient. The fraction
!!                                |of fertilizer P remaining in labile pool
!!                                |after initial rapid phase of P sorption
!!    sol_actp(:,:)|kg P/ha       |amount of phosphorus stored in the
!!                                |active mineral phosphorus pool
!!    sol_labp(:,:)|kg P/ha       |amount of phosohorus stored in the labile P
!!                                |pool
!!    sol_stap(:,:)|kg P/ha       |amount of phosphorus in the soil layer
!!                                |stored in the stable mineral phosphorus pool
!!    wshd_pal     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from labile mineral to active mineral pool
!!                                |in watershed
!!    wshd_pas     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from active mineral to stable mineral pool
!!                                |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rmp1tl       |kg P/ha       |amount of phosphorus moving from the labile
!!                                |mineral pool to the active mineral pool in
!!                                |the soil profile on the current day in the
!!                                |HRU
!!    roctl        |kg P/ha       |amount of phosphorus moving from the active
!!                                |mineral pool to the stable mineral pool
!!                                |in the soil profile on the current day in
!!                                |the HRU
!!    sol_actp(:,:)|kg P/ha       |amount of phosphorus stored in the
!!                                |active mineral phosphorus pool
!!    sol_labp(:,:)|kg P/ha       |amount of phosohorus stored in the labile P
!!                                |pool
!!    sol_stap(:,:)|kg P/ha       |amount of phosphorus in the soil layer
!!                                |stored in the stable mineral phosphorus pool
!!    wshd_pal     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from labile mineral to active mineral pool
!!                                |in watershed
!!    wshd_pas     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from active mineral to stable mineral pool
!!                                |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bk          |
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layer)
!!    rmn1        |kg P/ha       |amount of phosphorus moving from the labile
!!                               |mineral to the active mineral pool in the
!!                               |soil layer
!!    roc         |kg P/ha       |amount of phosphorus moving from the active
!!                               |mineral to the stable mineral pool in the 
!!                               |soil layer
!!    rto         |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

cd      real, parameter :: bk = .0006
      real :: bk 
      integer :: j, l
      real :: rto, rmn1, roc
   
      j = 0
      j = ihru
      bk = .0006

      rto = 0.
      rto = psp / (1.-psp)
c     write(*,*) psp,psp,'psp'
      do l = 1, nn
        rmn1 = 0.
        rmn1 = (sol_labp(l,j) - sol_actp(l,j) * rto)
 
        if (rmn1 > 0.) rmn1 = rmn1 * 0.1
        if (rmn1 < 0.) rmn1 = rmn1 * .6   

        rmn1 = Min(rmn1, sol_labp(l,j))
        roc = 0.
        roc = bk * (4. * sol_actp(l,j) - sol_stap(l,j))   !! DSHI replaced bk-->0.0006
 
        if (roc < 0.) roc = roc * .1
        roc = Min(roc, sol_actp(l,j))       
        sol_stap(l,j) = sol_stap(l,j) + roc
 
        if (sol_stap(l,j) < 0.) sol_stap(l,j) = 0.
        sol_actp(l,j) = sol_actp(l,j) - roc + rmn1

        if (sol_actp(l,j) < 0.) sol_actp(l,j) = 0.
        sol_labp(l,j) = sol_labp(l,j) - rmn1
 
        if (sol_labp(l,j) < 0.) sol_labp(l,j) = 0.
  
        if (curyr > nyskip) then
          wshd_pas = wshd_pas + roc * hru_fr(j)
          wshd_pal = wshd_pal + rmn1 * hru_fr(j)
          roctl = roctl + roc
          rmp1tl = rmp1tl + rmn1
       
       end if        
      end do

      return
      end
