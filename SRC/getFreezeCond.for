      real function getFreezeCond(nowcrp,iplane)
c
c   +++PURPOSE+++
c
c   The following code determines the best frozen soil hydraulic coefficient to 
c   use based on the current land use. The three kfactors are read from the the
c   frost.txt file. kfactor(1) represents the frost permeability for forest conditions
c   (honeycomb frost). kfactor(2) for pasture/prennnial. kfactor(3) for annual crops
c   or fallow/bare (concrete frost).  3/30/2012 - jrf
c
c   Author: Jim Frankenberger
c   Date: 3/30/2012
c
c
      include 'pntype.inc'
      include 'pmxcrp.inc'
      include 'pmxpln.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxres.inc'
      include 'pmxnsl.inc'
      
      include 'ccrpvr5a.inc'
      include 'ccrpprm.inc'
      include 'cwint.inc'
      include 'cperen1.inc'
      include 'crinpt5.inc'
      include 'crinpt3a.inc'
      
c   +++ARGUMENT DECLARATIONS+++
c
      integer nowcrp,iplane
c
c   +++LOCAL Variables+++
c
      integer plant
      real kf

      kf = -1
      
      plant = itype(nowcrp,iplane)
      if (iplant(plant) == 1) then
c       use parameters for a cropland plant                 
        if (diam(plant) > 0.10) then
c          for stems > 10cm assume it is trees                    
           kf = kfactor(1)
         else
           if (rtmmax(plant) > 0.0) then
c            if perennial root mass is set assume a perennial                         
             kf = kfactor(2)
           else
c            last option, annual crop or fallow                       
             kf = kfactor(3)
           endif
         endif
      else
c        use parameters for a rangeland plant
         if ((ghgt(plant) > 0.).or.(shgt(plant) > 0.)) then
c           any grasses or shrubs assume plants                       
            kf = kfactor(2)
         else if (thgt(plant) > 0.) then
c          if any trees assume forest                    
           kf = kfactor(1)
         else                    
c           nothing, bare                      
            kf = kfactor(3)
         endif                 
      endif
c      write(*,*) iplane,'kf=', kf
      getFreezeCond = kf 
      
      end
   