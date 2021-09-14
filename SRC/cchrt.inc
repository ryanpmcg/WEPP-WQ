
c     begin include file cchrt.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /chrt/ichout,ntchr,nxchr,nchnum,mofapp,dtchr,dxchr,
     1             ichid(mxelem),ichnum(mxplan),q1(0:mxtchr,mxplan),
     1             sinit(mxplan),volint(mxplan),sfnl(mxplan),cbase,
     1             htpk(0:mxelem),qlich(mxplan),qinich(mxplan),trise,
     1             hsarea(mxhill),qBase(mxplan),lastStor(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer  ichout,ntchr,nxchr,nchnum,ichid,ichnum,mofapp
      real     dtchr,dxchr,q1,sinit,volint,sfnl,cbase,htpk,qlich,
     1         qinich,trise,hsarea,qBase,lastStor


c     + + + VARIABLE DEFINITIONS + + +

c     ichid  : channel number corresponding to its element ID number
c     ichnum : channel IDs for channel routing output
c     ichout : flag for channel flow output
c              0, no output
c              1, output peak flow time and rate for specified channels
c              2, output daily average flow rate and total runoff for specified channels
c              3, output flow rate at each time step for specified channels
c     mofapp : method of approximation for qin and qlat, 1 or 2
c     nchnum : number of channels for channel routing output
c     ntchr  : number of time steps
c     nxchr  : number of space intervals
c     dtchr  : time step [s]
c     dxchr  : space increment [m]
c     sfnl   : final storage of the channel system above an outlet [m^3]
c     sinit  : initial storage of the channel system above an outlet [m^3]
c     trise  : time of rise [s]
c     volint : daily inflow volume to the channel system above an outlet [m^3]
c     cbase  : unit area base flow [m3/s/m2]
c     qBase  : Baseflow channel [m^3]
c     lastStor : previous days surface storage [m^3]
c     

c     end include file cchrt.inc

