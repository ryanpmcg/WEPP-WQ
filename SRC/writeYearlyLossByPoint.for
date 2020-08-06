      subroutine writeYearlyLossByPoint(yr)               
c
c     +++PURPOSE+++
c     This function will output only the soil loss down the hillslope at 100
c     points per OFE for the specified year.
c
c     This is only used when the hourly water balance is enabled. This still needs work,
c     and testing.
c
c     Author(s):  Jim Frankenberger
c     Date: 2/15/2012

c     Verified and tested by: 
c
c
c     +++ARGUMENT DECLARATIONS+++                
      integer yr
      
      include 'pmxpln.inc'
      include 'pmxseg.inc'
      include 'pmxpts.inc'
      
      include 'csedld.inc'
      include 'cstruc.inc'
      
      integer i,j,pt
      
      pt = 0
       
      do 10 i = 1,nplane
        do 20 j = 1, 100
          pt = pt + 1
          write (80,2900) i, yr, stdist(pt), ysdist(pt), dstot(pt)
   20   continue
   10 continue
   
 2900 format (2(1x,i4),3(1x,f10.3))
      end