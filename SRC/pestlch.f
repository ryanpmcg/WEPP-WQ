      subroutine pestlch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates  pesticides leached through each layer,
!!    pesticide transported with lateral subsurface flow, and pesticide
!!    transported with surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flat(:,:)    |mm H2O        |lateral flow in soil layer on current day
!!    ihru         |none          |HRU number
!!    nn           |none          |number of layers in soil profile
!!    npno(:)      |none          |array of unique pesticides used in watershed
!!    nptot(:)     |none          |total number of pesticides used in HRU
!!    percop       |none          |pesticide percolation coefficient (0-1)
!!                                |0: concentration of pesticide in surface
!!                                |   runoff is zero
!!                                |1: percolate has same concentration of
!!                                |   pesticide as surface runoff
!!    sol_bd(:,:)  |Mg/m**3       |bulk density of the soil
!!    sol_kp(:,:,:)|(mg/kg)/(mg/L)|pesticide sorption coefficient, Kp; the
!!                                |ratio of the concentration in the solid
!!                                |phase to the concentration in solution
!!    sol_por(:,:) |none          |total porosity of soil layer expressed as
!!                                |a fraction of the total volume
!!    sol_prk(:,:) |mm H2O        |percolation from soil layer on current day
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer
!!    sol_wpmm(:,:)|mm H20        |water content of soil at -1.5 MPa (wilting
!!                                |point)
!!    sol_z(:,:)   |mm            |depth to bottom of soil layer
!!    surfq        |mm H2O        |surface runoff generated on day in HRU
!!    wsol(:)      |mg/L (ppm)    |solubility of chemical in water
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pst_surq(:,:)|kg/ha         |amount of pesticide type lost in surface
!!                                |runoff on current day in HRU
!!    pstsol(:)    |kg/ha         |amount of pesticide type leached from soil
!!                                |profile on current day
!!    ssfp(:)      |kg pst/ha     |amount of pesticide in lateral flow in HRU
!!                                |for the day
!!    zdb(:,:)     |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    co          |
!!    csurf       |
!!    dg          |mm            |depth of soil layer
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |none          |pesticide number from pest.dat
!!    ly          |none          |counter (soil layers)
!!    qsurf       |mm H2O        |surface runoff for layer
!!    vf          |
!!    xx          |
!!    yy          |
!!    zdb1        |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, ly, k, kk
      real :: dg, yy, qsurf, vf, zdb1, xx, co, csurf
 
      j = 0
      j = ihru
      pst_surq = 0.0    !!DSHI No filter strip reduction

      if (hrupest(j) /= 0) then

        do ly = 1, nn
          if (ly == 1) then
            yy = 0.
          else
            yy = 0.
            yy = sol_z(ly-1,j)
          end if
          dg = 0.
          dg = sol_z(ly,j) - yy

          do k = 1, npmx
            kk = 0
            kk = npno(k)

            if (kk > 0) then
                qsurf = 0.
              if (ly == 1) then
                qsurf = surfq
              else
                qsurf = 0.
              endif

              zdb1 = 0.
              zdb1 = (dg * sol_por(ly,j) + sol_wpmm(ly,j)) + 10. * 
     *              sol_kp(k,j,ly) * sol_bd(1,j)
              if (ly == 1) zdb(k,j) = zdb1

              vf = 0.
              vf = qsurf + sol_prk(ly,j)

              if (vf > 0.) then
                xx = 0.
                xx = sol_pst(k,j,ly) * (1. - Exp(-vf / (zdb1 + 1.e-6)))
                co = 0.
cd                co = Min(wsol(kk),xx / (sol_prk(ly,j) + percop * (qsurf &
cd     &                                           + flat(ly,j)) + 1.e-6))
                co = Min(wsol(kk),xx / (sol_prk(ly,j) + percop * (qsurf)&
     &                                           + 1.e-6))

                !! calculate pesticide leaching
                xx = 0.
                xx = co * sol_prk(ly,j)
                if (xx > sol_pst(k,j,ly)) xx = sol_pst(k,j,ly)
                sol_pst(k,j,ly) = sol_pst(k,j,ly) - xx

                if (ly < nn) then
                  sol_pst(k,j,ly+1) = sol_pst(k,j,ly+1) + xx
                else
                  pstsol(k) = pstsol(k) + xx
                end if

                !! calculate pesticide lost in lateral flow
                csurf = 0.
                yy = 0.
                csurf = percop * co
cd                yy = csurf * flat(ly,j)
cd                if (yy > sol_pst(k,j,ly)) yy = sol_pst(k,j,ly)
cd                sol_pst(k,j,ly) = sol_pst(k,j,ly) - yy
cd                ssfp(k) = ssfp(k) + yy * fsred(j)

                !! calculate pesticide lost in surface runoff
                if (ly == 1) then
                  yy = 0.
                  yy = csurf * surfq
                  if (yy > sol_pst(k,j,ly)) yy = sol_pst(k,j,ly)
                  sol_pst(k,j,ly) = sol_pst(k,j,ly) - yy
cd                  pst_surq(k,j) = yy * fsred(j)
                  pst_surq(k,j) = yy    !!DSHI No filter strip reduction
                endif
              end if

            end if
          end do
        end do
      end if

      return
      end
