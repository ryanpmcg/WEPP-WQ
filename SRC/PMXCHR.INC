
c     begin include file pmxchr.inc

c     + + + PARAMETER DECLARATIONS + + +

      integer mxtchr
      real    dtlowl, dtupl1, dtupl2

      parameter (mxtchr=1440, dtlowl=600., dtupl1=3600., dtupl2=1800.)

c     + + + PARAMETER DEFINITIONS + + +

c     mxtchr - maximum number of time steps for channel routing
c     dtlowl - lower limit of time step [s]
c     dtupl1 - upper limit of time step for continuous modeling [s]
c     dtupl2 - upper limit of time step for event modeling [s]

c     end include file pmxchr.inc
