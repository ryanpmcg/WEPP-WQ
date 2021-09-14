      subroutine chemical_stub(bddry,st,flux,ul,thetdr,dg,fc,solthk,
     &                    mxnsl,
     &                    mxplan,sdate,FPHU,areaOFE,runoff,rmagt,smrm,
     &                    obmaxt,obmint,tmin,tmax,tave,radmj,salb,
     &                    month,sedyldMT,iplane,rain,soilw,year,
     &                    ncrop, CropName, orgmat, LAI, jdharv)


      implicit none

      integer :: month, iplane, ncrop

      integer :: mxnsl,mxplan,sdate,year,jdharv
      
      real bddry(mxnsl,mxplan),st(mxnsl,mxplan),flux(mxnsl,mxplan),
     &     ul(mxnsl),thetdr(mxnsl,mxplan),dg(mxnsl,mxplan),
     &     fc(mxnsl,mxplan),
     &     solthk(mxnsl,mxplan),FPHU,areaOFE,runoff,rmagt(mxplan),
     &     smrm(3,mxplan),obmaxt(12),obmint(12),
     &     tmin,tmax,tave,radmj,salb,sedyldMT,rain,
     &     soilw(mxnsl,mxplan),orgmat(mxnsl,mxplan),LAI
       
       character*51 :: CropName
       
       return
       
       end

       subroutine fromWeppToSwat_stub
       return
       
       end
       
       subroutine fromSwatToWepp_stub
       return
       
       end