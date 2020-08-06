	subroutine wshchr
c
c     + + + PURPOSE + + +
c     SR WSHCHR routes channel flow using either kinematic wave method
c     or Muskingum-Cunge method.
c
c     Called from: SR WSHDRV, SR WSHPEK
c     Author(s): L. Wang, S. Dun, J. Frankenberger
c     Reference in User Guide:
c
c     February 08, 2012
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxslp.inc'
      include 'pmxtim.inc'
      include 'pmxseg.inc'
      include 'pmxcsg.inc'
      include 'pmxchr.inc'
	include 'pmxtil.inc'
	include 'pmxtls.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     q1    - channel outflow rate, [m3/s]
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchpek.inc'
      include 'cdata1.inc'
      include 'cdata3.inc'
      include 'chydrol.inc'
      include 'cslope.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cstruct.inc'
      include 'cchvar.inc'
      include 'cchpar.inc'
      include 'cchrt.inc'
	include 'cupdate.inc'
      include 'cslpopt.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real qchpk, qchavg, ckref, cqa, tk, cx, qlavg, qref, qtmax,
     1   qmax, qmaxi, qmin, ck, c0, c1, c2, c3, c4, chvol, bw, y, eps,
     1   dtdx, chnl1, chns0, chnn0, chnz0, volin, ain, aout, asum,
     1   cxmin, aavg, qin(0:mxtchr), qlat(0:mxtchr),
     1   qs(0:mxcseg,0:mxtchr),bal, qtmin, qbasel,vbasel,volon,
     1   vsb,vsbbs,qbaset,vbaset,ckw,bt,aref,ap,dqdy,
     1   sslp,qavg,ti,qsb,qtotl,areat,areal
c
      integer i, it, itt, itpk, ih, is, nseg, ic, ishp, nt0
c
c
c     + + + LOCAL DEFINITIONS + + +
c
c     bw    - channel width, [m]
c     chnl1 - channel length, [m]
c     ishp  - shape of the channel
c     it    - current time step number
c     qbasel- channel lateral inflow from groundwater base flow of the side hillslopes, [m3/s]
c     qbaset- channel inflow from groundwater base flow of the top hillslope, [m3/s]
c     qin   - channel inflow rate, [m3/s]
c     qlat  - channel lateral inflow
c     qs    - channel flow rate, [m3/s]
c     vbasel- channel lateral inflow volume from groundwater base flow, [m3]
c     vbaset- channel inflow volume from groundwater base flow, [m3]
c     volin - channel inflow or channel lateral inflow volume, [m3]
c
c     + + + SUBROUTINES CALLED + + +
c     chrqin
c     mann
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      eps = 1.e-8
	qs = 0.
	qmax = 0.
	qmin = 0.
	qtmax = 0.
	qtmin = 0.
	volin = 0.
      chnl1 = chnlen(ichan)
      bw = chnwid(ichan)
	ishp = ishape(ichan)
	chns0 = chnslp(ichan,1)
	chnn0 = chnn(ichan)
	chnz0 = chnz(ichan)
	if(chnz0 < eps) ishp = 2
	if(ishp == 3) chnz0 = bw * chnz(ichan) / 8.        ! Parabolic-shape channel, chnz0: focal height
	cxmin = -10.
c -----------------------------------------------------------------
c To check whether it is the first time to route the current channel. If yes,
c we need to find the inflow and outflow at the initial condition (IC)
c from the runon of the channel, and the runoff, subsurface flow, and base
c flow from the related hillslopes.
	if(q1(ntchr, ichan) < -1.e5) then
	   nt0 = 0
	else
	   nt0 = 1
	endif
c -----------------------------------------------------------------
c Channel lateral inflow or outflow
c -----------------------------------------------------------------
	qlat(0) = qlich(ichan)
	do it = nt0, ntchr
	   qlat(it) = 0.
	enddo
c Base flow contribution from the side hillslopes
c     Check for hillslopes to prevent accessing out of bounds index 0
c     2/16/2012 - jrf
      areal = 0.0
      areat = 0.0
      if (nhrght(ielmt).gt.0) areal = areal + hsarea(nhrght(ielmt))
      if (nhleft(ielmt).gt.0) areal = areal + hsarea(nhleft(ielmt))
      if (nhtop(ielmt).gt.0) areat = areat + hsarea(nhtop(ielmt))

	qbasel = cbase * areal
      vbasel = qbasel * 86400.
c Total channel lateral inflow calculated by WSHCQI and WSHDRV
      volon = rvolat(ielmt) + chnvol(ielmt)
      volin = volon - rtrans(ielmt)
c Subsurface flow from the side hillslopes
      vsb = tmpsbv(nhleft(ielmt))+tmpsbv(nhrght(ielmt))
c Sum of subsurface flow and base flow
	vsbbs = vsb + vbasel
	if(volin <= vsbbs) then
c There is lateral outflow when volin < 0, or
c channel lateral inflow comes from base flow and subsurface flow only.
	   do it = nt0, ntchr
	       qlat(it) = volin/86400.
	   enddo
	else
c Channel lateral inflow comes from base flow, subsurface flow, and surface runoff.
         call chrqin(volin - vsbbs, qlat, nt0, 1)
	   do it = nt0, ntchr
	       qlat(it) = qlat(it) + vsb/86400. + qbasel
	   enddo
	endif
	volint(ichan) = volint(ichan) + volin
c -----------------------------------------------------------------
c Channel inflow
c -----------------------------------------------------------------
	qin(0) = qinich(ichan)
	do it = nt0, ntchr
	    qin(it) = 0.
	enddo
	ih=nhtop(ielmt)
	volint(ichan) = volint(ichan) + rvotop(ielmt)
      if(ih > 0) then
        qbaset = cbase * hsarea(ih)
        qsb = tmpsbv(ih) / 86400.
        call chrqin(tmpvol(ih), qin, nt0, 2)
	  do it = nt0, ntchr
	        qin(it) = qin(it) + qsb + qbaset
	  enddo
      endif
c	
	ih = nctop(ielmt)
	if(ih > 0) then
	    ic = ichid(ih)
          do it = nt0, ntchr
	        qin(it) = qin(it) + q1(it, ic)
	    enddo
	endif
c	
	ih = ncleft(ielmt)
	if(ih > 0) then
	    ic = ichid(ih)
          do it = nt0, ntchr
	        qin(it) = qin(it) + q1(it, ic)
	    enddo
	endif
c	
	ih = ncrght(ielmt)
	if(ih > 0) then
	    ic = ichid(ih)
          do it = nt0, ntchr
	        qin(it) = qin(it) + q1(it, ic)
	    enddo
	endif
	qlich(ichan) = qlat(ntchr)
	qinich(ichan) = qin(ntchr)
c -----------------------------------------------------------------
      if(q1(ntchr,ichan) < -1.e5) then
c Calculate the initial condition (IC) of outflow. The IC is assumed to be at steady state.
	    q1(0,ichan) = qin(0) + qlat(0)
c
	    if(qin(0) > 0.) then
	      call Mann(qin(0),ichan,chns0,chnn0,ishp,bw,chnz0,y)
	      if(ishp == 1) then
c Triangular-shape channel
	         ain = chnz0 * y * y
	      elseif(ishp == 2) then
c Rectangular-shape channel
	         ain = bw * y
	      elseif(ishp == 3) then
c Parabolic-shape channel
	         ain = 8./3.*y*sqrt(y*chnz0)
	      elseif(ishp >= 4) then
c Tropezoidal-shape channel
	         ain = (bw + chnz0 * y) * y
	      endif
	    else
	      ain = 0.
	    endif
c
	    if(q1(0,ichan) > 0.) then
	      call Mann(q1(0,ichan),ichan,chns0,chnn0,ishp,bw,chnz0,y)
	      if(ishp == 1) then
c Triangular-shape channel
	         aout = chnz0 * y * y
	      elseif(ishp == 2) then
c Rectangular-shape channel
	         aout = bw * y
	      elseif(ishp == 3) then
c Parabolic-shape channel
	         aout = 8./3.*y*sqrt(y*chnz0)
	      elseif(ishp >= 4) then
c Tropezoidal-shape channel
	         aout = (bw + chnz0 * y) * y
	      endif
	    else
	      aout = 0.
	    endif
	    aavg = 0.5 * (ain + aout)
	    sfnl(ichan) = aavg * chnl1
          lastStor(ichan) = sfnl(ichan)
c -----------------------------------------------------------------
	else
c The initial outflow of today (time 0000) = the final outflow of yesterday (time 2400).
	    q1(0,ichan) = q1(ntchr,ichan)
	endif
c -----------------------------------------------------------------
	do it=0, ntchr
	   qs(0, it) = qin(it)
	   if(q1(it,ichan) > qmax) qmax = q1(it,ichan)
	   if(qin(it) > qmax) then
	      qmax = qin(it)
	      itpk = it
	   endif
	   if(qin(it) < qmin) qmin = qin(it)
	   qtotl = qin(it) + qlat(it) * 0.5
	   if(qtotl < qtmin) qtmin = qtotl
	   if(qtotl > qtmax) qtmax = qtotl
	enddo
	if(qmin < 0.) qmin = 0.
	if(qmax < 0.) qmax = 0.
	if(qtmin < qmin) qtmin = qmin
	if(qtmax < qmax) qtmax = qmax
      if(ipeak==3) qref = qtmax				! KW
c      if(ipeak>=4) qref = 0.5 * (qmin + qmax)	! MC (Ponce and Chaganti, 1994; Ponce et al., 1996; Tewolde and Smithers, 2006)
      if(ipeak>=4) qref = 0.5 * (qtmin + qtmax)	! in case there is only lateral inflow but no inflow from top of channel.
	do it=0, ntchr
	   qlat(it) = qlat(it) / chnl1
	enddo
	call Mann(qref, ichan, chns0, chnn0, ishp, bw, chnz0, y)
	ckw = trise * chns0 * sqrt(9.81/y)
	if(ckw < 15) then
	   print *,'Warning: may not satisfy KW/DF wave criterion.'
	endif
	if(ishp == 1) then
c Triangular-shape channel
	   ckref = 4.*qref/(3.*chnz0*y*y)
	   bw = 2.*y*chnz0
	   bt = bw
	   aref = chnz0 * y * y
	elseif(ishp == 2) then
c Rectangular-shape channel
         bt = bw
	   ckref = qref/(bw*y)*(1.+2.*bw/(3.*(bw+2.*y)))
	   aref = bw * y
	elseif(ishp == 3) then
c Parabolic-shape channel
         bt = 4. * sqrt(chnz0 * y)                                   ! top width
	   ap = 2.*sqrt(y*(chnz0+y))+2.*chnz0*log(sqrt(1.+y/chnz0)
     1        + sqrt(y/chnz0))                                       ! wetted perimeter
	   dqdy = (2.5/y - 4./(3.*ap)*sqrt(1.+bt/y)) * qref
	   ckref = dqdy / bt
	   aref = 8./3.*y*sqrt(y*chnz0)
	elseif(ishp >= 4) then
c Tropezoidal-shape channel
         bt = bw + 2. * chnz0 * y
         sslp = sqrt(1. + chnz0 * chnz0)
	   dqdy = (bt*(5.*bw+6.*y*sslp)+4.*chnz0*y*y*sslp)
	1          /(3.*y*(bw+chnz0*y)*(bw+2.*y*sslp)) * qref
	   ckref = dqdy / bt
	   aref = (bw + chnz0 * y) * y
	endif
	dxchr = dtchr * ckref
	if (dxchr.le.0.) dxchr = 1
	nseg=chnl1/dxchr
      if(nseg > mxcseg) nseg = mxcseg
	if(nseg < 1) nseg = 1
c       not sure why next line is here	
c The following is for the single-spatial-step MC method. It's faster to compute but may lead to negtive outflow for the wave front.
c	if(ipeakm == 41) nseg = 1
	dxchr = chnl1/nseg
c
	do is = 0, nseg
	   qs(is,0) = qin(0)+(q1(0,ichan)-qin(0))*float(is)/float(nseg)
	enddo
c -----------------------------------------------------------------
c Linear kinematic wave method
c
      asum = 0.
      if(ipeak == 3)then
	  do it=1, ntchr
          if(mofapp==1) then
              qlavg = 0.5 * (qlat(it-1) + qlat(it))
          else
              qlavg = qlat(it)
          endif
	    do is =1, nseg
		  qavg = 0.5 * (qs(is,it-1) + qs(is-1,it))
	      if(abs(qavg) > 0.) then
		    dtdx = dtchr/dxchr
		    call Mann(abs(qavg),ichan,chns0,chnn0, 
	1             ishp, bw, chnz0, y)
		    if(ishp == 1) then
c Triangular-shape channel
		 	   ck = 4.*qavg/(3.*chnz0*y*y)
	        elseif(ishp == 2) then
c Rectangular-shape channel
			   ck = qavg/(bw*y)*(1.+2.*bw/(3.*(bw+2.*y)))
              elseif(ishp == 3) then
c Parabolic-shape channel
         bt = 4. * sqrt(chnz0 * y)
	   ap = 2.*sqrt(y*(chnz0+y))+2.*chnz0*log(sqrt(1.+y/chnz0)
     1        + sqrt(y/chnz0))
	   dqdy = (2.5/y - 4./(3.*ap)*sqrt(1.+bt/y)) * qavg
	   ck = dqdy / bt
	   aref = 8./3.*y*sqrt(y*chnz0)
			elseif(ishp >= 4) then
c Tropezoidal-shape channel
			   sslp = sqrt(1. + chnz0 * chnz0)
	   dqdy = ((bw+2.*chnz0*y)*(5.*bw+6.*y*sslp)+4.*chnz0*y*y*sslp)
	1		          /(3.*y*(bw+chnz0*y)*(bw+2.*y*sslp)) * qavg
			   ck = dqdy / (bw + 2. * chnz0 * y)
c -----------------------------------------------------------------
		    endif
c
c  Check for underflow so that very small numbers are not used
c  jrf - 2-22-2012. Li review this.
c 		    
		      if (ck < 1e-12) then
		        qs(is,it) = 0.0
		      else
	          cqa = 1./ck
c end change	         
		       qs(is,it) = (dtdx*qs(is-1,it) + cqa*qs(is,it-1)
     1                  + dtchr*qlavg) / (dtdx+cqa)
               endif
	      else
              qs(is,it) = qlavg * dxchr
	      endif
	    end do
		q1(it,ichan)=qs(nseg,it)
	    if(q1(it,ichan) < eps) q1(it,ichan) = 0.
	  end do
	  do is = 0, nseg
          if(abs(qs(is,ntchr)) > 0.) then
	      call Mann(abs(qs(is, ntchr)), ichan, chns0, chnn0,
	1                ishp, bw, chnz0, y)
	    endif
	    if(ishp == 1) then
c Triangular-shape channel
	      asum = asum + chnz0 * y * y
		elseif(ishp == 2) then
c Rectangular-shape channel
            asum = asum + bw * y
	    elseif(ishp ==3) then
c Parabolic-shape channel
	      asum = asum + 8./3.*y*sqrt(y*chnz0)
		elseif(ishp >= 4) then
c Tropezoidal-shape channel
		  asum = asum + (bw + chnz0 * y) * y
	    endif
	  enddo
	  aavg = asum / float(nseg + 1)
c -----------------------------------------------------------------
c Muskingum-Cunge method
c
	elseif(ipeak >= 4) then
        if(qref > 0.) then
	     tk = dxchr/ckref
	     cx = 0.5*(1.-qref/(bw*ckref*chns0*dxchr))
	     if(cx < cxmin) cx = cxmin
	  else
	     tk=0.
	     cx=0.
	  endif
	  c0=1./(2.*tk*(1.0-cx)+dtchr)
	  c1=(dtchr-2.*tk*cx)*c0
	  c2=(dtchr+2.*tk*cx)*c0
	  c3=1.-c1-c2
	  do it=1, ntchr
		 qmaxi = max(qin(it-1), q1(it-1,ichan), qin(it))
           if(mofapp==1) then
              qlavg = 0.5 * (qlat(it-1) + qlat(it))
           else
              qlavg = qlat(it)
           endif
	     c4=2.*qlavg*dxchr*dtchr*c0
	     if(qmaxi > 0. .or. qlavg > 0.) then
	        do is =1, nseg
	          if(ipeak == 5) then ! MVPMC3
	          qref = (qs(is-1,it)+qs(is-1,it-1)+qs(is,it-1)) / 3.
cw			  if(qref > 0.) then
			  if(qref < eps) qref = eps
				call Mann(qref,ichan,chns0,chnn0,ishp,bw,chnz0,y)
				if(ishp == 1) then
c Triangular-shape channel
				   ckref = 4.*qref/(3.*chnz0*y*y)
				   bw = 2.*y*chnz0
	               bt = bw
	            elseif(ishp == 2) then
c Rectangular-shape channel
                     bt = bw
				   ckref = qref/(bw*y)*(1.+2.*bw/(3.*(bw+2.*y)))
                  elseif(ishp == 3) then
c Parabolic-shape channel
                     bt = 4. * sqrt(chnz0 * y)                                   ! top width
	               ap = 2. * sqrt(y*(chnz0+y)) + 2. * chnz0
     1                   * log(sqrt(1.+y/chnz0) + sqrt(y/chnz0))                 ! wetted perimeter
	               dqdy = (2.5/y - 4./(3.*ap)*sqrt(1.+bt/y)) * qref
	               ckref = dqdy / bt
	               aref = 8./3.*y*sqrt(y*chnz0)
				elseif(ishp >= 4) then
c Tropezoidal-shape channel
                     bt = bw + 2. * chnz0 * y
				   sslp = sqrt(1. + chnz0 * chnz0)
	               dqdy = (bt*(5.*bw+6.*y*sslp)+4.*chnz0*y*y*sslp)
	1			          /(3.*y*(bw+chnz0*y)*(bw+2.*y*sslp)) * qref
				   ckref = dqdy / bt
c -----------------------------------------------------------------
				endif
				 tk = dxchr/ckref
				 cx = 0.5*(1.-qref/(bt*ckref*chns0*dxchr))
	             if(cx < cxmin) cx = cxmin
cw			  else
cw				 tk=0.
cw				 cx=0.
cw			  endif
			  c0=1./(2.*tk*(1.0-cx)+dtchr)
			  c1=(dtchr-2.*tk*cx)*c0
			  c2=(dtchr+2.*tk*cx)*c0
			  c3=1.-c1-c2
	          c4=2.*qlavg*dxchr*dtchr*c0
	          endif
            qs(is,it)=c1*qs(is-1,it)+c2*qs(is-1,it-1)+c3*qs(is,it-1)+c4
	        end do
	        q1(it,ichan)=qs(nseg,it)
	     else
	        q1(it,ichan) = 0.
	     endif
	    if(q1(it,ichan) < eps) q1(it,ichan) = 0.
        enddo
c -----------------------------------------------------------------
	    if(qin(ntchr) > 0.) then
	      call Mann(qin(ntchr),ichan,chns0,chnn0,ishp,bw,chnz0,y)
	      if(ishp == 1) then
c Triangular-shape channel
	         ain = chnz0 * y * y
	      elseif(ishp == 2) then
c Rectangular-shape channel
	         ain = bw * y
	      elseif(ishp == 3) then
c Parabolic-shape channel
	         ain = 8./3.*y*sqrt(y*chnz0)
	      elseif(ishp >= 4) then
c Tropezoidal-shape channel
	         ain = (bw + chnz0 * y) * y
	      endif
	    else
	      ain = 0.
	    endif
c
	    if(q1(ntchr,ichan) > 0.) then
	      call Mann(q1(ntchr,ichan),ichan,chns0,chnn0,ishp,bw,chnz0,y)
	      if(ishp == 1) then
c Triangular-shape channel
	         aout = chnz0 * y * y
	      elseif(ishp == 2) then
c Rectangular-shape channel
	         aout = bw * y
	      elseif(ishp == 3) then
c Parabolic-shape channel
	         aout = 8./3.*y*sqrt(y*chnz0)
	      elseif(ishp >= 4) then
c Tropezoidal-shape channel
	         aout = (bw + chnz0 * y) * y
	      endif
	    else
	      aout = 0.
	    endif
	    aavg = 0.5 * (ain + aout)
	endif
c
	sinit(ichan) = sfnl(ichan)
	sfnl(ichan) = aavg * chnl1
c -----------------------------------------------------------------
	itpk = 0
	qchpk = 0.
	do it=1, ntchr
	   if(q1(it,ichan) > qchpk) then
	       itpk = it
		   qchpk = q1(it,ichan)
	   endif
	end do
c -----------------------------------------------------------------
	ih = nctop(ielmt)
	if(ih > 0) then
	    ic = ichid(ih)
	    sfnl(ichan) = sfnl(ichan) + sfnl(ic)
	endif

c	
	ih = ncleft(ielmt)
	if(ih > 0) then
	    ic = ichid(ih)
	    sfnl(ichan) = sfnl(ichan) + sfnl(ic)
	endif
c	
	ih = ncrght(ielmt)
	if(ih > 0) then
	    ic = ichid(ih)
	    sfnl(ichan) = sfnl(ichan) + sfnl(ic)
	endif
	chvol = volint(ichan) + sinit(ichan) - sfnl(ichan)
	if(chvol < 0.) chvol = 0.
	if(qchpk <= 0.) chvol = 0.
	sfnl(ichan) = sinit(ichan) + volint(ichan) - chvol
	qchavg = chvol / 86400.
c -----------------------------------------------------------------
c	if(volint(ichan) < eps .and. chvol < eps) then
c If there is no inflow nor outflow, then put the surface storage to
c transmission loss. 
c    This was causing water balance to be off, needs to be looked at
c    more closely. Without statements water balnce is ok. jrf 2-22-2012
c	   if(sfnl(ichan) > 0.) then
c	       rtrans(ielmt) = rtrans(ielmt) + sfnl(ichan)
c	       sfnl(ichan) = 0.
c	   endif
c	endif
c -----------------------------------------------------------------
	do i=1,nchnum
	   if(ielmt == ichnum(i)) then
	    if(ichout < 3) then
	       if(ichout == 1)
     1         write(66, 104) year, sdate, ielmt, idelmt(ielmt),
     1             dtchr*itpk, qchpk
	       if(ichout == 2) 
     1         write(66, 105) year, sdate, ielmt, idelmt(ielmt),
     1            qchavg, chvol
	    elseif(qchpk < eps) then
	       ti=86400.
             write(66, 104) year,sdate,ielmt,idelmt(ielmt),ti,qchavg
	    else
	       do it=1, ntchr
                write(66, 104) year,sdate,ielmt,idelmt(ielmt),
     1             dtchr*it,q1(it,ichan)
	       enddo
	    endif
c         ccompute the water balance for this day:
c         total runon from any hillslopes or channels (rvolon)
c         runoff generated by channel alone (chnvol)
c         runoff exiting the channel (chvol)
c         surface storage for today (sfnl)
c         surface storage from previous day (lastStor)
c         transmission loss in the channel (rtrans)	    
	    bal = rvolon(ielmt)+chnvol(ielmt)-chvol-
     1	        (sfnl(ichan)-lastStor(ichan))-rtrans(ielmt)
          write(67,106) year,sdate,ielmt,idelmt(ielmt), 
	1                  rvolon(ielmt)+chnvol(ielmt),chvol,sfnl(ichan),
     1                  qbase(ichan),rtrans(ielmt),bal
	  endif
	enddo
	peakot(ielmt) = qchpk
	htpk(ielmt) = dtchr * itpk / 3600.
	if(chvol <= 0.) then
	    runvol(ielmt) = 0.0
	    rundur(ielmt) = 0.0
	else
	    runvol(ielmt) = chvol
	    rundur(ielmt) = runvol(ielmt)/peakot(ielmt)
	endif
      runoff(iplane) = runvol(ielmt)/charea(iplane)
      tmpvol(ielmt) = runvol(ielmt)
      lastStor(ichan) = sfnl(ichan)
104	format(1x,4(i5,2x), 3x, f7.0, 4x, es10.2)
105   format(1x,4(i5,2x), 2x, es10.2, 2x, f10.2)
106   format(1x,4(i5,2x), 6(1x, f10.2))
      return
	end