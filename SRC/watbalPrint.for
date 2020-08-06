      subroutine watbalPrint(lunw)
c
c     + + + PURPOSE + + +
c
c     Prints out the watershed water output line. This could had to be moved from
c     its original location to handle the new channel routing code that includes 
c     surface storage.
c
c     January 25, 2012
c
c     Jim Frankenberger
c     NSERL
c
 
      integer lunw 
          
      real surfmm,watcon,rm,subrin,frozwt,baseflow
      integer i,ictop, icleft, icrght
          
      include 'pmxelm.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxprt.inc'
      include 'pmxres.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pmxhil.inc'
      include 'pmxcrp.inc'
      include 'pmxcut.inc'
      include 'pmxgrz.inc'
      include 'pxstep.inc'
      include 'pmxchr.inc'
          
      include 'cstruc.inc'
      include 'cchrt.inc'
      include 'cdist2.inc'
      include 'cslpopt.inc'
      include 'chydrol.inc'
      include 'cwater.inc'
      include 'ccdrain.inc'
      include 'cwint.inc'
      include 'cirriga.inc'
      include 'cirfur1.inc'
      include 'cstore.inc'
      include 'cupdate.inc'
      include 'cstruct.inc'
          
      watcon = 0.
      frozwt = 0.
      do 690 i = 1, nsl(iplane)
          watcon  = watcon + soilw(i,iplane)
          frozwt = frozwt + soilf(i,iplane)
 690  continue
 
      ictop = 0
      icleft = 0
      icrght = 0
      if(nctop(ielmt).gt.0) ictop = nctop(ielmt)-nhill
      if(ncleft(ielmt).gt.0) icleft= ncleft(ielmt)-nhill
      if(ncrght(ielmt).gt.0) icrght= ncrght(ielmt)-nhill
c    
       subrin =  (sbrunf(ictop)*fwidth(ictop)*slplen(ictop) 
     1              + sbrunf(icleft)*fwidth(icleft)*slplen(icleft) 
     2              + sbrunf(icrght)*fwidth(icrght)*slplen(icrght))
     3                /(fwidth(iplane)*slplen(iplane))
      
      rm=(rain(iplane)+wmelt(iplane)+irdept(iplane)+iraplo(iplane))
     1       *1000.
     
        surfmm = (sfnl(iplane)/(slplen(iplane)*fwidth(iplane)))*1000.
        baseflow = (qBase(iplane)/(slplen(iplane)*fwidth(iplane)))*1000.
          if (lunw.eq.1) then
            write (35,3300) iplane, sdate, year, prcp*1000., rm,
     1      runoff(iplane)*1000., ep(iplane)*1000., es(iplane)*1000.,
     1      eres(iplane)*1000.,
     1      sep(iplane)*1000,roffon(ielmt)*1000,subrin*1000.,
     1      sbrunf(iplane)*1000.,watcon*1000.,frozwt*1000.,
     1      snodpy(iplane)*densg(iplane),
     1      runoff(iplane)*1000.,drainq(iplane)*1000,
     1      (irdept(iplane)+iraplo(iplane))*1000,
     1      surfmm, baseflow,
     1      slplen(iplane)*fwidth(iplane)
     
          endif
      return 
      
 3300 format (1x,3(1x,I4),2(1x,f7.2),1x,e15.7,4(1x,f7.2),
     1        1x,e15.7,5(1x,f7.2),2x,e15.7,2(1x,f7.2),3(1x,f10.2))
     
      end     