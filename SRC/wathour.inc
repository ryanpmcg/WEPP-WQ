
c     begin include file wathour.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /watht/  ui_epht(30,366), ui_esht(30,366), 
     1 ui_sepht(30,366), ui_areaht, ui_bdrkth(mxplan),
     1 ui_ssh1(mxnsl,mxplan),ui_ssh(mxnsl+1,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real   ui_epht, ui_esht, ui_sepht, ui_areaht
      real   ui_bdrkth, ui_ssh1, ui_ssh
     

c     + + + VARIABLE DEFINITIONS + + +

c     ui_epht : for plant ET average on the hillslopes
c     ui_esht : for bare soil ET average on hilslopes
c     ui_sepht: for deep percolation average on hillslopes
c     ui_areaht: for the total hillslope area
c     ui_bdrkth: thickness of bedrock (m)
c

      common /lfrtg/ ui_HUrunf,ui_SUrunf(24),ui_LfUrf(24),
     1 ui_Hcrunf,ui_Scrunf(24),ui_Lfcrf(24),ui_LFtstp, ui_run,
     1 ui_plot_out

c     + + + VARIABLE DECLARATIONS + + +

      real      ui_HUrunf,ui_SUrunf,ui_LfUrf,ui_Hcrunf,ui_Scrunf
      real      ui_Lfcrf
      integer   ui_LFtstp, ui_run, ui_plot_out


c     + + + VARIABLE DEFINITIONS + + +

c     ui_run    : set to 1 if we are running this water balance
c     ui_HUrunf : average Hortonian runoff from upstream OFE
c     ui_SUrunf : saturation runoff from upstream OFE in current time step
c     ui_LfUrf  : subsurface latteral flow from upstream OFE in current time step
c     ui_Hcrunf : average Hortonian runoff out of current OFE
c     ui_Scrunf : saturation runoff out of current OFE in current time step
c     ui_Lfcrf  : subsurface latteral flow out of current OFE in current time step
c     ui_LFtstp : the number of time steps
c     ui_bdrkth: thickness of bedrock (m)
c     ui_plot_out: set to 1 if the yearly soil loss values are output to lossYears.txt

c     end include file wathour.inc

