      subroutine drain(soldep,nowcrp,watbl)
c
c     + + + PURPOSE + + +
c     This subroutine is called from WATBAL to calculate daily
c     drainage flux (m/d) if:
c       1) drainage system exists and,
c       2) water table  is above drainage tiles.
c     For more detail contact:
c            Reza Savabi, USDA-ARS, National Soil
c            Erosion Lab. (317-494-8673)
c
c     Called from WATBAL
c     Author(s): Savabi
c     Reference in User Guide:
c
c     Changes:
c             1) Deleted reference to include files: PNTYPE.INC,
c                PTILTY.INC, PMXTIM.INC, PMXTLS.INC, PMXTIL.INC,
c                PMXCRP.INC, & PMXCUT.INC.
c             2) Deleted reference to include files CFLAGS.INC
c                and CHYDROL.INC.
c             3) Changed dimensions on DRFC & DRAWAT from 10 to MXNSL.
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 01/12/93 - 1/15/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxnsl.inc'
      include 'pmxpnd.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
      include 'pntype.inc'
c     + + + ARGUMENT DECLARATIONS + + +
      real soldep, watbl
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     soldep - soil profile depth
c     ddrain - depth from surface to drainage tiles (m)
c     drainc - drainage coefficient (m/day)
c     drdiam - drain tile diameter (m)
c     unsdep - unsaturated depth from surface to water table (m)
c     sdrain - drain spacing (m)
c     drainq - drainage flux (m/day)
c     satdep - saturated depth from soil surface (m)
c     nowcrp - current crop
c     iplane - current overland flow element
c     drseq  - drainage sequence based on which crop and overland flow
c              element in use
c
c     + + + COMMON BLOCKS + + +
      include 'ccdrain.inc'
      include 'ccons.inc'
c        read: coca
c
      include 'cstruc.inc'
c        read: iplane
c
      include 'cparame.inc'
c        read: por
c
      include 'cwater.inc'
c        read: ssc, fc, thetfc, nsl, solthk, dg
c      modify: st
      include 'cwint.inc'
c
c     + + + LOCAL VARIABLES + + +
      real drfc(mxnsl), d,de,dranks,temp,totdg,totk,wattbl,watyld
      real vartmp,potdrain,alpha,r,L,cdep,fzdrfc,excess,dep2watbl
      integer cnt,limit,uzone
      
      
      integer jj, mn, tilel, i
c
c     + + + LOCAL DEFINITIONS + + +
c     drfc   - layer's field capacity, corrected for entrapped air
c     dranks - hydraulic conductivity for drainage calculation (m/d)
c
c     + + + SAVES + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c      Calculate average soil hydraulic conductivity in saturated zone
c      (below water table). Also determine soil layer where tile drain
c      is installed. 
c
      totk = 0.0
      totdg = 0.0
      cdep = 0.0
      tilel = 0
      excess = 0

c     
c     Figure out the soil layer where the tile is located.
c      
      do 5 mn = 1, nsl(iplane)
        cdep = cdep + dg(mn,iplane)
        if (cdep.le.ddrain(drseq(nowcrp,iplane))) then
           tilel = mn
        endif
    5 continue
    
      tilel = tilel + 1
    
c
c     For each of the soil layers above tile depth calculate the amount of water
c     that is excess and is available to be drained.
c    
      totk = 0.0
      totdg = 0.0
      dep2watbl = soldep - watbl
      do 7 jj = 1, nsl(iplane)
        if (jj.le.tilel) then
c          -------- layer's field capacity, corrected for entrapped air
          drfc(jj) = fc(jj) + ((1-coca(1,iplane))*dg(jj,iplane))
          fzdrfc = drfc(jj) - frzw(jj,iplane)
          if (fzdrfc .lt. 0.) fzdrfc = 0.

          vartmp = (dg(jj,iplane) - frozen(jj,iplane))/dg(jj,iplane)
          if (st(jj,iplane) .ge. fzdrfc) then
c           ---------- potential water which can be drained from this layer
c           drawat(jj) = st(jj,iplane) - drfc(jj)*vartmp
            drawat(jj) = st(jj,iplane) - fzdrfc
          else
            drawat(jj) = 0.0
          endif
        else
c         Layer is below tile depth, will not drain          
          drawat(jj) = 0.0
        endif
c
c       Used to compute Ke over the water table depth        
        if (solthk(jj,iplane).ge.dep2watbl) then
          totk = totk + (ssc(jj,iplane)*dg(jj,iplane))
          totdg = totdg + dg(jj,iplane)
        endif
    7 continue

c
c     Get average Ks of saturated layers and convert from m/s to cm/h.
c     Assume horizontal flow KZ = KY, where KY = vertical K.
c
      if (totdg.gt.0.0) then
         dranks = (totk/totdg) * 3600. * 100.0
      else
         dranks = 0.0
      endif
c
c
c     Calculate drainage flux (m/day) for tile drains
c
c     distance from restrictive layer or end of profile to tile (cm)
      d = (soldep - ddrain(drseq(nowcrp,iplane))) * 100.0
c     check if drain is placed below the end of the profile,
c     if it is then reset to just above bottom of profile      
      if (d.lt.0) d = 1.0
c     drain spacing (cm)     
      L = sdrain(drseq(nowcrp,iplane)) * 100.0
c     drain tile radius (cm)     
      r = (drdiam(drseq(nowcrp,iplane))/2.0) * 100.0
      temp = d / L
      alpha = 3.4
    
c
cc    Drain diameter is needed for DE calculations,
cc    0.1 m for Oregon; ie, rr=0.1
c
c     Calculate "equivalent depth (DE) using DRAINMOD equations
c     2-13 to 2/15.
c     (WEPP Equation 7b.2.6)
c
c     Also check for positive, otherwise d is negative which causes crash in
c     log call. 3/10/2010 - jrf
      if ((temp.le..3).and.(temp.gt.0.0)) then
        de = d / (1.0+temp*((8.0/3.14)*
     1      log(d/r)-alpha))
      else
        de = (L*3.14) / (8.0*(log(L/r)-1.15))
      end if
c
c     compute drainage flux in cm/day per unit width
      if (dep2watbl.le.ddrain(drseq(nowcrp,iplane))) then
c       highest water table height above the tile (cm) (would be midpoint between 2 tile lines)       
        wattbl = (ddrain(drseq(nowcrp,iplane)) - dep2watbl) * 100.0
c       sdrain=distance between tiles, m
c       ------ subsurface flow to drain tubes
c       (WEPP Equation 7b.2.5)
        drainq(iplane) = (8.0*dranks*de*wattbl)+(4.0*dranks*(wattbl**2))
        drainq(iplane) = drainq(iplane) / (L**2)
      else
        drainq(iplane) = 0.0
      end if
c
c     convert from cm/hr to m/day
      drainq(iplane) = (drainq(iplane) / 100.0) * 24.0                
c
c     ---- Limit drain flux to the hydraulic capacity of the drainage system.
      if (drainq(iplane).gt.drainc(drseq(nowcrp,iplane)))
     1    drainq(iplane) = drainc(drseq(nowcrp,iplane))
c
c     *** L0 IF ***
c     If there is drainage from the tiles (DRAINQ > 0), update the
c     available water content (ST) in each soil layer to reflect
c     the drainage from each layer, starting at the top.
c
c     drainq was being reduced as the water was removed from the soil,
c     the ending value represented the amount of water that was not removed. Use a local 
c     variable to step through removing water so that drainq represents the amount of water
c     removed by drainage for this day. 2-2-2010 jrf
      potdrain = drainq(iplane)
      
c     This is the soil layer one past where the tile is installed (it will be decreased).
c     Water will be drained starting at the layer and working towards the surface.      
      jj = tilel + 1
      
c     This controls how many layers of excess water is removed in a day. For now we
c     only remove any excess water in the layer containing the tile and all layers above. 
c     Since this is a daily timestep it is assumed that excess water above the tile will
c     make it to the tile in the same day.
c      
      limit = tilel
      if (limit.lt.1) limit = 1
      
      if (potdrain.gt.0.0) then
c       *** Begin L1 DO-LOOP ***
   20   continue
        jj = jj - 1
        
        if (potdrain.gt.0.0) then
c           ------------ If water excess in this layer exceeds or equals what has
c           run from drain...
            if (drawat(jj).ge.potdrain) then
c             -------------- subtract amount that has run out the drain fron this layer.
              st(jj,iplane) = st(jj,iplane) - potdrain
              if (st(jj,iplane).lt.1e-10) st(jj,iplane) = 1e-10
              drawat(jj) = drawat(jj) - potdrain
              potdrain = 0.0
              
c           ------------ If water excess in this layer is less than what has run
c           from drain...
            else
c             -------------- subtract from DRAINQ what this layer can contribute.
              potdrain = potdrain - drawat(jj)
c             -------------- adjust soil layer water content by same amount of water
              st(jj,iplane) = st(jj,iplane) - drawat(jj)
              drawat(jj) = 0
            end if
c                       
c
        end if
c
c       *** End L1 DO-LOOP ***

        if ((jj.gt.limit).and.(potdrain.gt.0.0)) go to 20
  
c
c
c     *** L0 ENDIF ***
      end if
      
c     if there was water that was not removed from the soil layers adjust drainq to that
c     it will balance with the actual water removed. jrf 2-2-2010      
      if (potdrain.gt.0.0) then
          drainq(iplane) = drainq(iplane) - potdrain
      endif
      
c     Convert drainage flux to soil water depth using average
c     drainable porosity (water yield).
c      watyld = por(1,iplane) - (thetfc(1,iplane)+(1.0-coca(1,iplane)))
c      unsdep(iplane) = unsdep(iplane) + (drainq(iplane)/watyld)
c      if (unsdep(iplane).lt.0.0) unsdep(iplane) = 0.0
c      satdep(iplane) = soldep - unsdep(iplane)
c
c     This is the calculation of satdep and unsdep from watbal, it takes into
c     account frozen soil. It finds the first saturated layer. Added 3-16-2010 jrf.
      satdep(iplane) = -1.0
      unsdep(iplane) = 0.0
      
      watbl = 0.0
      uzone = -1
      do 100 i = nsl(iplane), 1, -1
          drfc(i) = fc(i) + ((1-coca(i,iplane))*dg(i,iplane))
cd        Added by S. Dun, March 13, 2008
          fzdrfc = drfc(i) - frzw(i,iplane)
          if (fzdrfc .lt. 0.) fzdrfc = 0.
          if (st(i,iplane).ge.fzdrfc) then
            if (uzone.lt.0) then
               watbl = watbl + dg(i,iplane)
            endif
          else
c           hit unsaturated zone, stop
            uzone = i            
          endif
  100 continue
        
      satdep(iplane) = watbl

      unsdep(iplane) = soldep - satdep(iplane)

      return
      end
