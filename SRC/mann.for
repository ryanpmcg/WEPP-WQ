      subroutine mann(q0, ichan, slp, chnn, ishp, bw, s, y)

      implicit none
c
c     + + + PURPOSE + + +
c     SR Mann solves depth y from Manning's equation.
c
c     Called from: SR WSHCHR
c     Author(s): L. Wang
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real q0, slp, chnn, y, bw, s
      integer ichan, ishp
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     bw    - channel width
c     chnn  - Manning's roughness coefficient n
c     q0    - known discharge
c     s     - inverse slope of the channel banks
c     slp   - channel slope
c     y     - depth of water flow
c     ichan - channel number
c     ishp  - shape of the channel
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      real area, rh, ay, ry, qi, dy, q0x, yx, eps, ap
      integer i, ichanx
c
c
c     + + + LOCAL DEFINITIONS + + +
c
c     area  - cross-sectional area of channel water flow
c     ay    - derivative, AY = dA / dY
c     qi    - calculated discharge
c     rh    - hydraulic radius
c     ry    - derivative, RY = dR / dY
c     ichanx - channel number
c
c     + + + SAVES + + +
      save ichanx, q0x, yx
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      eps = 1.e-6
      if(ichan /= ichanx .or. abs(q0 - q0x) > eps)then
c--------------------------------------------------
	   if(ishp == 1) then
c
c Triangular-shape channel
c Calculate y explicitly.
c
	      y = 2.**0.25*(1.+s*s)**0.125*(chnn*q0)**0.375/
	1          (slp**0.1875*s**0.625)
c
c--------------------------------------------------
	   elseif(ishp == 2) then
c
c Rectangular-shape channel
c Calculate y iteratively using Newton's method.
c
c First guess of y assuming bw >> y.
c            y = (chnn*q0/(sqrt(slp)*bw))**0.6
            y = 1.
c
	      i = 0
400         i = i + 1
	      area = bw * y
	      rh = area / (bw + 2. * y)
	      ay = bw                                      ! ay = dA / dy
	      ry = (bw / (bw + 2. * y))**2                 ! ry = dR / dy
            qi = sqrt(slp) / chnn * area * rh ** (2./3.)
	      dy = (q0/qi-1.) / (ay/area + 2./3.*ry/rh)
	      y = y + dy
	      if(y > 1) dy = dy / y
            if(abs(dy) > eps .and. i < 20) goto 400
c--------------------------------------------------
c--------------------------------------------------
	   elseif(ishp == 3) then
c
c Parabolic-shape channel
c Calculate y iteratively using Newton's method.
c s = parabolic focal height
c
            y = 1.
c
	      i = 0
300         i = i + 1
            bw = 4. * sqrt(y * s)                             ! top width
	      area = 2. * bw * y / 3.
	      ap = 2.*sqrt(y*(s+y))+2.*s*log(sqrt(1.+y/s)+sqrt(y/s)) ! wetted perimeter
	      rh = area / ap                                    ! hydraulic radius
	      ay = bw                                           ! ay = dA / dy
	      ry = bw/ap - 2.*area/(ap*ap)*sqrt(1.+s/y)       ! ry = dR / dy
            qi = sqrt(slp) / chnn * area * rh ** (2./3.)
	      dy = (q0/qi-1.) / (ay/area + 2./3.*ry/rh)
	      y = y + dy
	      if(y > 1) dy = dy / y
            if(abs(dy) > eps .and. i < 20) goto 300
c--------------------------------------------------
c--------------------------------------------------
	   elseif(ishp >= 4) then
c
c Tropezoidal-shape channel
c Calculate y iteratively using Newton's method.
c
            y = 1.
c
	      i = 0
200         i = i + 1
	      area = (bw + s * y) * y
	      rh = area / (bw + 2. * y * sqrt(1. + s * s))
	      ay = bw + 2. * s * y
	      ry = (bw*bw + 2.*bw*s*y + 2.*s*y*y*sqrt(1.+s*s))
	1           / (bw + 2. * y * sqrt(1. + s * s))**2
            qi = sqrt(slp) / chnn * area * rh ** (2./3.)
	      dy = (q0/qi-1.) / (ay/area + 2./3.*ry/rh)
	      y = y + dy
	      if(y > 1) dy = dy / y
            if(abs(dy) > eps .and. i < 20) goto 200
c--------------------------------------------------
c--------------------------------------------------
	   endif
c--------------------------------------------------
	   ichanx = ichan
	   q0x = q0
	   yx = y
	else
	   y = yx
	endif
c
      return
	end
