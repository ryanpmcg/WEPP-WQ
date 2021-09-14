      subroutine nlch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates the loss of nitrate via surface runoff, 
!!    lateral flow, and percolation out of the profile

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    anion_excl(:)|none         |fraction of porosity from which anions
!!                               |are excluded
!!    flat(:,:)   |mm H2O        |lateral flow in soil layer on current day
!!    ihru        |none          |HRU number
!!    nn          |none          |number of layers in soil profile
!!    nperco      |none          |nitrogen percolation coefficient (0-1)
!!                               |0:concentration of soluble N in surface runoff
!!                               |  is zero
!!                               |1:percolate has same concentration of soluble
!!                               |  N as surface runoff
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in soil layer
!!    sol_prk(:,:)|mm H2O        |percolation from soil layer on current day
!!    sol_ul(:,:) |mm H2O        |amount of water held in the soil layer at
!!                               |saturation
!!    surfq       |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latno3      |kg N/ha       |amount of nitrate transported with lateral
!!                               |flow
!!    percn       |kg N/ha       |amount of nitrate percolating past bottom
!!                               |of soil profile
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in soil layer
!!    surqno3     |kg N/ha       |amount of nitrate transported with surface 
!!                               |runoff
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    co          |kg N/mm       |concentration of nitrate in solution
!!    cosurf      |kg N/mm       |concentration of nitrate in surface runoff
!!    j           |none          |HRU number
!!    jj          |none          |counter (soil layers)
!!    percnlyr    |kg N/ha       |nitrate leached to next lower layer with
!!                               |percolation
!!    sro         |mm H2O        |surface runoff
!!    ssfnlyr     |kg N/ha       |nitrate transported in lateral flow from layer
!!    vno3        |                ???
!!    vv          |mm H2O        |water mixing with nutrient in layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, jj
      real ::  ssfnlyr, percnlyr, vv, vno3, co    !!DSHI
      real :: sro
      real :: cosurf,flat,dtile
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxres.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pxstep.inc'
      include 'ccdrain.inc'

      j = 0
      j = ihru
      iplane = 1 
      percnlyr = 0.

      do jj = 1, nn

        !! add nitrate leached from layer above
        sol_no3(jj,j) = sol_no3(jj,j) + percnlyr
        open(1,file = 'percolation.txt')
        write(1,*) percnlyr,jj,'percolation NO3'

        !! determine concentration of nitrate in mobile water
        sro = 0.
        vv = 0.
        vno3 = 0.
        co = 0.
        if (jj == 1) then
          sro = surfq
        else
          sro = 0.
        end if
cd        vv = sol_prk(jj,j) + sro + flat(jj,j) + 1.e-10
        vv = sol_prk(jj,j) + sro + 1.e-10
        vno3 = sol_no3(jj,j) * (1. - Exp(-vv / ((1. - anion_excl(j)) *  &
     &                                                   sol_ul(jj,j))))
        co = Max(vno3 / vv, 0.)
c        write(*,*) jj,sol_ul(jj,j),vv,vno3,co,'1soil2vv3vno3'
        !! calculate nitrate in surface runoff
        cosurf = 0.
        cosurf = nperco * co
        if (jj == 1.) then
          surqno3 = surfq * cosurf
          surqno3 = Min(surqno3, sol_no3(jj,j))
          sol_no3(jj,j) = sol_no3(jj,j) - surqno3
c          write(*,*) surfq,cosurf,surqno3,'1 runoff 2cof4no3q'    
        endif
         
       
       !! calculate nitrate in tile drainage
        dtile = drainq(iplane)*1000
        ssfnlyr = 0.
        if (jj == 9) then
          ssfnlyr = co * dtile
          ssfnlyr = Min(ssfnlyr, sol_no3(jj,j))
          sol_no3(jj,j) = sol_no3(jj,j) - ssfnlyr

c         open(2,file = 'Tile_N_PKN.txt') 
c         write(2,*) dtile,ssfnlyr,percnlyr,co  

         end if

c       if (ssfnlyr.ne.0) then
c        write(*,*) ssfnlyr,'nitrate in tile drainage'
c       end if
        !! calculate nitrate in percolate
        percnlyr = 0.
        percnlyr = co * sol_prk(jj,j)
        percnlyr = Min(percnlyr, sol_no3(jj,j))
        sol_no3(jj,j) = sol_no3(jj,j) - percnlyr
      end do
c      write(*,*) percnlyr,ssfnlyr
      !! calculate nitrate leaching from soil profile
      percn = percnlyr      !  write(*,*) percn,'nitrogen leached'

      return
      end
