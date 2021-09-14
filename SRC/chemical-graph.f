      subroutine chemical_graph( )
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this program writes the list of x-axis variables that need to be displayed under interface to see chemical module results

      use parm
      implicit none
cd      integer :: j,ICFLAG
cd      character (len=80) ::  titldum
cd      character (len=13) :: chemdat, mgtdat, pestidat, fertdat, cropdb    !! DSHI merged from openfile.f
      integer :: k,kk

      write (1113,*)'Nitrate N (kg/ha)'
      write (1113,*)'Fresh Organic N (kg/ha)'
      write (1113,*)'Active Organic N (kg/ha)'
      write (1113,*)'Ammonium N (kg/ha)'
      write (1113,*)'Stable Organic N (kg/ha)'
      write (1113,*)'N Leached (kg/ha)'
      write (1113,*)'N in Sediments (kg/ha)'
      write (1113,*)'N in Runoff (kg/ha)'
      write (1113,*)'N Uptake by Plants(kg/ha)'
      write (1113,*)'N Stress [-]'
      write (1113,*)'Denitrification (kg/ha)'
      write (1113,*)'N Fixation (kg/ha)'
      write (1113,*)'Labile P (kg/ha)'
      write (1113,*)'Active P (kg/ha)'
      write (1113,*)'Stable P (kg/ha)'
      write (1113,*)'Fresh Organic P (kg/ha)'
      write (1113,*)'Active Organic P (kg/ha)'
      write (1113,*)'P in Sediments (kg/ha)'
      write (1113,*)'P in Runoff (kg/ha)'
      write (1113,*)'P Uptake by Plants (kg/ha)'
      write (1113,*)'P Stress [-]'

      do k = 1, npmx
        kk = 0
        kk = npno(k)
        if (kk > 0) then
          write (1113,*)TRIM(pname(kk)),' in Soil (kg/ha)'
        endif
      enddo

      do k = 1, npmx
        kk = 0
        kk = npno(k)
        if (kk > 0) then
          write (1113,*)TRIM(pname(kk)),' Leached (kg/ha)'
        endif
      enddo

      do k = 1, npmx
        kk = 0
        kk = npno(k)
        if (kk > 0) then
          write (1113,*)TRIM(pname(kk)),' Degraded (kg/ha)'
        endif
      enddo

      do k = 1, npmx
        kk = 0
        kk = npno(k)
        if (kk > 0) then
          write (1113,*)TRIM(pname(kk)),' in Runoff (kg/ha)'
        endif
      enddo

      return
      end


