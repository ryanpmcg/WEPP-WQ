      subroutine decay
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates degradation of pesticide in the soil and on 
!!    the plants

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    decay_f(:)    |none          |exponential of the rate constant for
!!                                 |degradation of the pesticide on foliage
!!    decay_s(:)    |none          |exponential of the rate constant for
!!                                 |degradation of the pesticide in soil
!!    dkf(:,:)      |kg/ha         |amount of pesticide lost through degradation
!!                                 |from plant foliage
!!    dkg(:,:)      |kg/ha         |amount of pesticide lost through degradation
!!                                 |from soil
!!    hru_fr(:)     |none          |fraction of watershed area in HRU
!!    ihru          |none          |HRU number
!!    nn            |none          |number of layers in soil profile
!!    npno(:)       |none          |array of unique pesticides used in watershed
!!    nptot(:)      |none          |total number of pesticides used in HRU
!!    pdb(:)        |kg pst/ha     |amount of pesticide lost through degradation
!!                                 |in watershed
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,:)|kg/ha         |pesticide in soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dkf(:,:)      |kg/ha         |amount of pesticide lost through degradation
!!                                 |from plant foliage
!!    dkg(:,:)      |kg/ha         |amount of pesticide lost through degradation
!!                                 |from soil
!!    pdb(:)        |kg pst/ha     |amount of pesticide lost through degradation
!!                                 |in watershed
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,:)|kg/ha         |pesticide in soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |none          |pesticide number from pest.dat
!!    l           |none          |counter (soil layers)
!!    x1          |kg/ha         |amount of pesticide present at beginning of 
!!                               |day
!!    xx          |kg/ha         |amount of pesticide present at end of day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, k, kk, l
      real :: x1, xx
   
      j = 0
      j = ihru

      if (hrupest(j) == 0) return

      do k = 1, npmx
        kk = 0
        kk = npno(k)
        if (kk > 0) then

          !! calculate degradation in soil
          do l = 1, nn
            x1 = 0.
            x1 = sol_pst(k,j,l)
            if (x1 >= 1.e-12) then
              xx = 0.
              xx = x1 * decay_s(kk)
              dkg(k,j) = dkg(k,j) + x1 - xx
              pdb(k) = pdb(k) + (x1 - xx) * hru_fr(j)
              sol_pst(k,j,l) = xx
            end if
          end do

          !! calculate degradation off plant foliage
          x1 = 0.
          x1 = plt_pst(k,j)
          if (x1 >= 1.e-12) then
            xx = 0.
            xx = x1 * decay_f(kk)
            dkf(k,j) = dkf(k,j) + x1 - xx
            pdb(k) = pdb(k) + (x1 - xx) * hru_fr(j)
            plt_pst(k,j) = xx
          end if
        end if
      end do

      return
      end

