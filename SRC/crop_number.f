      function crop_number(CropName)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function corrects rate constants for temperature
!!    Equation is III-52 from QUAL2E

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    r20         |1/day         |value of the reaction rate coefficient at
!!                               |the standard temperature (20 degrees C)
!!    thk         |none          |temperature adjustment factor (empirical
!!                               |constant for each reaction coefficient)
!!    tmp         |deg C         |temperature on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    theta       |1/day         |value of the reaction rate coefficient at
!!                               |the local temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      character*51 :: CropName, NameCrop
      integer :: crop_number, number, eof
     
      crop_number = 0
      number = 0
      open (1113,file="cropref.dat")
      do
        read (1113,*,iostat=eof)NameCrop, number 
        if (eof < 0) exit
        if (CropName .eq. NameCrop) then
          crop_number = number
c          write(*,*) NameCrop,number,'CropName'
          exit
        endif
      enddo
    
      close (1113)
      return
      end
