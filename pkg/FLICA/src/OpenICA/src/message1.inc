      character*80 H(30,4) ! messages used by IN.for
      character*80 HK(45,4) ! messages used by PARMSET.for
      character*6  KO(10,4) ! valid character options 
      character*2  KY(10,4) ! character options for action
      character*80 HL(43,4) ! messages used by ICA1 and BY
      character*80 HM(61,4) ! messages used by ICA2 and SHRINKF
      character*80 HP(79,4) ! messages used by OUT
      character*77 HW(64,4) ! messages used by ICP4                                                                                                        
      character*10 KW(5,4)  ! options used by ICP4


      integer language

      common /lang/ language, H, HK, KO, KY, HL, HM, HW, KW

      common /outs/ HP




