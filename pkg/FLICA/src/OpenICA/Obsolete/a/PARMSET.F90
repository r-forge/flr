! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
!                                                                                                                                   
!           UNIT  PARMSET.FOR                                                                                                       
!                                                                                                                                   
!                 contains most of the code for screen dialogue,                                                                    
!                    and setting up the parameter list,                                                                             
!                                   the weights on the catches at age                                                               
!                    and also three functions to do with the VPA                                                                    
!                             calculations                                                                                          
!                                                                                                                                   
!                                                                                                                                   
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                              
!    Updated 5 November 1997 to make all screen-inputs and outputs using                                                            
!      routines in the unit 'scr_io.for', so that this can be changed                                                               
!      for each implmentation with different screen interfaces.                                                                     
!    Code to check that there are enough degrees of freedom to fit the                                                              
!      specified survey models.                                                                                                     
                                                                                                                                    
                                                                                                                                    
!///////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
      Subroutine ParmSet                                                                                                            
!                                                                                                                                   
!///////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
!     Starting-up housekeeping for various parameters                                                                               
!                                                                                                                                   
!     Reads run-time options from the screen:                                                                                       
!             Number of years for separable constraint                                                                              
!             Reference age                                                                                                         
!             Terminal selection                                                                                                    
!             First and last ages for arithmetic mean F in output table                                                             
!             Weighting options for the catch-at-age matrix                                                                         
!             Choice of catchability model for each index                                                                           
!             whether to fit a stock-recruit relationship                                                                           
!                                                                                                                                   
!     CALLS:         ReadSRRDat   ( to read in early SRR data if needed )                                                           
!                    tscan         (replaces intrinsic Microsoft FORTRAN)                                                           
!                                                                                                                                   
!                                                                                                                                   
!      This code is very simple screen-reading and error trapping.                                                                  
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      Include "INDAT.INC"                                                                                                           
      Include "SEPMODEL.INC"                                                                                                        
      Include "SRR.INC"                                                                                                             
      Include "LABELS.INC"                                                                                                          
                                                                                                                                    
                                                                                                                                    
!     Local variables                                                                                                               
                                                                                                                                    
      integer age, year, index, iage, iyear, j,k  ! local looping vars                                                              
      double precision sum, count                                                                                                   
      character*75 Text(10)                                                                                                         
      character*5 Ytext                                                                                                             
      character*1 dummy, ak(5)                                                                                                      
      double precision rdummy, MaxLambda, MinLambda                                                                                 
      double precision ascale(maxsurvey), bscale(maxbsurv)                                                                          
      integer tscan ! function                                                                                                      
                                                                                                                                    
      include "MESSAGE1.INC"                                                                                                        
                                                                                                                                    
                                                                                                                                    
!     __________________EXECUTABLE CODE OF PARMSET______________________                                                            
                                                                                                                                    
                                                                                                                                    
      MaxLambda = 10d0                                                                                                              
      MinLambda = 0.0001d0                                                                                                          

      NySep=6

      call Screen_in_i(HK(1,language),NySep,MaxSep,3,Language)                                                                      

!	write(*,*) 'Here is the value of Nysep', NySep, '****'
!       Call Add2LogFilei(NySep, Ica_log)

      Refage= firstage+3

      call Screen_in_i(HK(2,Language), Refage, lastage-3, firstage+1    &                                                           
      ,Language)                                                                                                                    
                                                                                                                                   
      dummy=KO(1,Language)
      call Screen_in_a(HK(3,Language),dummy,KO(1,language),Language)                                                                
                                                                                                                                    
      TwoSel = .false.                                                                                                              
      StepSel = .false.                                                                                                             
      if (tscan(Dummy, KY(1,language)) .eq. 0) then                                                                                 
	TwoSel = .True.
        ChangeSel= lastyear-3
	Call Screen_in_i(HK(4,Language),ChangeSel,lastyear-1,                  &                                                           
              lastyear-NySep,Language)
        dummy=' '
	call Screen_in_a(HK(5,Language),dummy,KO(2,Language),Language)                                                                     
	if (tscan(dummy, KY(2,Language)) .eq. 0) then                                                                                      
	  StepSel = .False.                                                                                                                
	 else                                                                                                                              
	  StepSel = .True.                                                                                                                 
	 endif                                                                                                                             
      endif ! two selection patterns                                                                                                
      TermS=1d0
      call Screen_in_R(HK(6,Language), TermS, 1.7d0, 0.01d0,Language)   
      TermS2=1d0 
      If (TwoSel) then                                                                                                              
        call Screen_in_R(HK(7,Language),TermS2,1.7d0,0.01d0,Language)   
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!  First and last ages for calculation of mean F                                                                                    
                                                                                                                                    
      LoFage = -1                                                                                                                   
      do while ((LoFage .gt. HiFage) .or. (LoFage .lt. firstage)        &                                                           
        .or. (HiFage .gt. lastage))                                                                                                 
        LoFage=firstage
        HiFage=lastage
	call Screen_in_i(HK(8,Language),LoFage,lastage,firstage,               &                                                           
         Language)                                                                                                                  
	call Screen_in_i(HK(9,Language), HiFage, lastage, firstage,            &                                                           
          Language)                                                                                                                 
	if (LoFage .gt. HiFage) then                                                                                                       
	  Text(1)=HK(10,Language)                                                                                                          
	  call Screen_out_a(Text,10,1)                                                                                                     
	endif                                                                                                                              
      enddo ! reading and checking first and last ages                                                                              
                                                                                                                                    
      call Screen_in_a(HK(11,Language),dummy,KO(1,Language),Language)                                                               
                                                                                                                                    
      IF ( Tscan(dummy, KY(1,Language)) .eq. 0) then                                                                                
	Text(1)= HK(12,Language)                                                                                                           
	Call SCREEN_OUT_A( Text,1,1)                                                                                                       
	                                                                                                                                   
	do age = firstage, lastage                                                                                                         
	  if (age .le. 9) then                                                                                                             
	    Call ConCat(Text(1), HK(13,Language),char(48+age))                                                                             
	  else                                                                                                                             
	    Call ConCat(Text(1), HK(13,Language),                              &                                                           
             char(48+(age/10))//char(48+age-10*(age/10))    )                                                                       
	  endif                                                                                                                            
          RelWt(age-firstage+1)=1d0
	  CAll Screen_in_r(Text(1),RelWt(age-firstage+1),                      &
              MaxLambda,MinLambda,Language)                                                                                         
	enddo                                                                                                                              
	Text(1)= HK(14,Language)                                                                                                           
	Call SCREEN_OUT_A(Text,1,1)                                                                                                        
	do year = lastyear-NySep+1, lastyear                                                                                               
	  Call IntToChar(year,ytext,5)                                                                                                     
	  Call Concat(Text(1),HK(15,Language), ytext)                                                                                      

          rdummy=1d0
          CAll Screen_in_r(Text(1),rdummy,MaxLambda,         &
               MinLambda,Language)
          YearWt(year-firstyear+1)=rdummy
	enddo ! years                                                                                                                      
                                                                                                                                    
	do year = firstyear, lastyear                                                                                                      
	  iyear = year-firstyear+1                                                                                                         
	  do age = firstage, lastage                                                                                                       
	    iage = age-firstage+1                                                                                                          
	    W(iyear,iage)=dsqrt(RelWt(iage)*YearWt(iyear))                                                                                 
	  enddo                                                                                                                            
	enddo                                                                                                                              
                                                                                                                                    
       Text(1) =HK(16,Language)                                                                                                     
       Call SCREEN_OUT_A( Text, 10,1 )                                                                                              
	    year = 2                                                                                                                       
	    Text(1)=HK(17,Language)                                                                                                        
            year=firstyear
            age=firstage
	    do while (year .ne. -1)
              rdummy=1d0
	      Call SCREEN_IN_IR(Text(1), year, age, rdummy, lastyear,          &                                                           
                lastyear-NySep,lastage,firstage,MaxLambda,MinLambda,Language)    
                if ((year .ne. -1) .and.(age .ne. -1) .and. (rdummy .ne. -1d0)) &  
                  W(year-firstyear+1,age-firstage+1)=dsqrt(rdummy)
              if (year .ne. -1) then
                if (age .ne. lastage) then
                  age=age+1
                else
                  age=firstage
                  year=year+1
                endif
              endif
	    enddo                                                                                                                          
                                                                                                                                    
      ELSE  ! Set all weights = 1                                                                                                   
                                                                                                                                    
	    do age = firstage, lastage                                                                                                     
	      RelWt(age-firstage+1) = 1d0                                                                                                  
	    enddo                                                                                                                          
                                                                                                                                    
	    do year = lastyear-NySep+1, lastyear                                                                                           
	      YearWt(year-firstyear+1) = 1d0                                                                                               
	    enddo                                                                                                                          
                                                                                                                                    
	    do year = firstyear, lastyear                                                                                                  
	      iyear = year-firstyear+1                                                                                                     
	      do age = firstage, lastage                                                                                                   
		  iage = age-firstage+1                                                                                                           
		  W(iyear,iage)=dsqrt(RelWt(iage)*YearWt(iyear))                                                                                  
	      enddo                                                                                                                        
	    enddo                                                                                                                          
                                                                                                                                    
      ENDIF                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      do index = 1, nageix                                                                                                          
	Call ConCat(Text(1),HK(18,Language),ASurvlab(index))                                                                               
        Call ConCat(Text(1),Text(1),HK(19,Language))
        dummy= KO(1,language)
	Call Screen_in_a(Text(1),dummy,KO(1,Language),Language)                                                                            
                                                                                                                                    
	if (Tscan(dummy,KY(1,Language)) .ne. 0 ) then                                                                                      
	  plusgp(index) = .true.                                                                                                           
	else                                                                                                                               
	  plusgp(index) = .false.                                                                                                          
	endif                                                                                                                              
      enddo ! Indices                                                                                                               
                                                                                                                                    
	                                                                                                                                   
      Text(1) = HK(20,Language)                                                                                                     
      Text(2) = ' '                                                                                                                 
      Text(3) = HK(21,Language)                                                                                                     
      Text(4) = HK(22,Language)                                                                                                     
      Text(5) = HK(23,Language)                                                                                                     
      Text(6) = ' '                                                                                                                 
      Text(7) = HK(24, Language)                                                                                                    
      Text(8) = HK(25, Language)                                                                                                    
      Text(9) = ' '                                                                                                                 
      call SCREEN_OUT_A(Text,10,9)                                                                                                  
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      do index = 1,nssbix                                                                                                           
	Call Concat(Text(1),HK(26,Language), BsurvLab(index))                                                                              
        CAll Concat(Text(1),Text(1),HK(27,Language))              
        dummy='L'
	Call SCREEN_IN_A(Text(1),dummy,'AaLlPp',Language)
	if ((dummy .eq. 'A') .or. (dummy .eq. 'a')) QBparm(index)=0
	if ((dummy .eq. 'L') .or. (dummy .eq. 'l')) QBparm(index)=1                                                                        
	if ((dummy .eq. 'P') .or. (dummy .eq. 'p')) QBparm(index)=2                                                                        
      enddo                                                                                                                         
                                                                                                                                    
      do index = 1,nageix                                                                                                           
	Call Concat(Text(1),HK(26,Language), AsurvLab(index))                                                                              
	CAll Concat(Text(1),Text(1),HK(27,Language))                                                                                       
        dummy='L'
        Call SCREEN_IN_A(Text(1),dummy,'AaLlPp',Language)
	if ((dummy .eq. 'A') .or. (dummy .eq. 'a')) QAparm(index)=0                                                                        
	if ((dummy .eq. 'L') .or. (dummy .eq. 'l')) QAparm(index)=1                                                                        
	if ((dummy .eq. 'P') .or. (dummy .eq. 'p')) QAparm(index)=2                                                                        
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!       count the catch-at-age observations                                                                                         
	nxdata = 0                                                                                                                         
	                                                                                                                                   
      do year = lastyear-NySep+1, lastyear                                                                                          
	do age = firstage, lastage-1                                                                                                       
	  if (CN(year-firstyear+1, age-firstage+1) .gt. 0d0) then                                                                          
	    nxdata = nxdata+1                                                                                                              
	  endif                                                                                                                            
	enddo                                                                                                                              
      enddo                                                                                                                         
                                                                                                                                    
      if (nxdata .ne. NySep*(lastage-firstage)) then                                                                                
                                                                                                                                    
	j = Nysep*(lastage-firstage)-nxdata                                                                                                
                                                                                                                                    
        ! j is number of missing observations in separable model                                                                    
                                                                                                                                    
        write(Ytext, '(I5)') j                                                                                                      
                                                                                                                                    
        CAll ConCat(Text(1), HK(28,Language), Ytext)                                                                                
        CAll ConCat(Text(1), Text(1), HK(29,Language) )                                                                             
        Call SCREEN_OUT_A(Text, 10,2)                                                                                               
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
	                                                                                                                                   
	                                                                                                                                   
	NxParm = 2*NySep-1 + (lastage-firstage) + (lastage-firstage-2)                                                                     
	                                                                                                                                   
	if (TwoSel) NxParm = NxParm+ (lastage-firstage-2)                                                                                  
                                                                                                                                    
                                                                                                                                    
	do index = 1, nssbix                                                                                                               
	  sum = 0d0                                                                                                                        
	  count = 0d0                                                                                                                      
	  Nxparm = Nxparm+1                                                                                                                
	  DO year = fbyear, lbyear                                                                                                         
	     if (Bindex(index, year-fbyear+1) .ge. 0.0 ) then                                                                              
	       sum = sum+dble(Bindex(index, year-fbyear+1))                                                                                
	       count = count+1d0                                                                                                           
	     endif                                                                                                                         
	  enddo                                                                                                                            
!                                                                                                                                   
!    CHECK ENOUGH DEGREES OF FREEDOM                                                                                                
!                                                                                                                                   
          if (count .lt. QBParm(index)) then                                                                                        
	    Text(1) = HK(30,Language)                                                                                                      
	    Call Concat(Text(2),HK(31,Language),BSurvlab(index))                                                                           
	    Call Screen_Out_A(Text,10,2)                                                                                                   
	    Stop                                                                                                                           
	  endif                                                                                                                            
                                                                                                                                    
                                                                                                                                    
	  Bscale(index) = sum/count                                                                                                        
                                                                                                                                    
	  nxdata = nxdata + idint(count)                                                                                                   
                                                                                                                                    
	enddo ! indices                                                                                                                    
                                                                                                                                    
      !  Next do the age-structured indices                                                                                         
                                                                                                                                    
	do index = 1, nageix                                                                                                               
	  do age = fage(index), lage(index)                                                                                                
	     NxParm = Nxparm+1                                                                                                             
	     sum   = 0d0                                                                                                                   
	     count = 0d0                                                                                                                   
	     DO year = fyear(index), lyear(index)                                                                                          
	      iage = age-fage(index)+1                                                                                                     
	      if (Aindex(index, year-fyear(index)+1,iage) .ne. MISSING)        &                                                           
                                                                  then                                                              
		 sum =sum+dble(Aindex(index, year-fyear(index)+1,iage))                                                                           
		 count = count+1d0                                                                                                                
	      endif                                                                                                                        
	    enddo ! years                                                                                                                  
                                                                                                                                    
	    nxdata = nxdata + idint(count)                                                                                                 
!           write(*,*) 'index, age',index,age,count,idint(count)                                                                    
	    Ascale(index) = sum/count                                                                                                      
            if (count .le. QAParm(index)) then                                                                                      
	      Text(1) = HK(30,Language)                                                                                                    
	      Call Concat(Text(2),HK(31,Language),ASurvlab(index))                                                                         
	      if (age .gt. 9) then                                                                                                         
		Call concat(Text(2),HK(42,Language),                                  &                                                           
                   char(48+age/10)//char(48+age-10*(age/10)))                                                                       
	      else                                                                                                                         
		Call concat(Text(2),HK(42,Language),                                  &                                                           
                 char(48+age)//'.')                                                                                                 
	      endif                                                                                                                        
	    Call Screen_Out_A(Text,10,2)                                                                                                   
	    Stop                                                                                                                           
	  endif                                                                                                                            
                                                                                                                                    
	  enddo     ! ages                                                                                                                 
	enddo       ! indices                                                                                                              
                                                                                                                                    
!      Stock-recruit relationship                                                                                                   
                                                                                                                                    
      dummy = KY(1,Language)
      Call SCREEN_IN_A(HK(32,Language),dummy, KO(1,Language),Language)
                                                                                                                                    
      If (tscan(dummy, KY(1,Language)) .ne. 0) then                                                                                 
	FitSRR = .true.                                                                                                                    
      else                                                                                                                          
	FitSrr = .false.                                                                                                                   
      endif                                                                                                                         
                                                                                                                                    
      lag = 0                                                                                                                       
      If (FitSRR) then                                                                                                              
	Text(1) = HK(33,Language)                                                                                                          
	Call Concat(Text(2),HK(34,Language),char(48+firstage))                                                                             
	CAll Concat(Text(2),Text(2),HK(35,Language))                                                                                       
	Call Concat(Text(3),HK(36,Language),char(48+firstage))                                                                             
	CAll Concat(Text(3),Text(3),HK(37,Language))                                                                                       
	Call Concat(Text(4),HK(38,Language),char(48+firstage+1))                                                                           
	Call Concat(Text(4),Text(4),HK(43,Language))                                                                                       
	Call SCREEN_OUT_A(Text,10,4)                                                                                                       
	call SCREEN_IN_I(HK(39,Language),lag,5,-5,Language)                                                                                
      else
        lag=-5
      endif 
                                                                                                                                    
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                             
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
      Subroutine GetPageWidth(pw, min,max)                                                                                          
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////                                                             
                                                                                                                                    
      integer pw, min, max                                                                                                          
      include 'MESSAGE1.INC'                                                                                                        
                                                                                                                                    
      CAll SCREEN_IN_I(HK(41,Language),pw, max,min,Language)                                                                        
                                                                                                                                    
      return                                                                                                                        
                                                                                                                                    
      end                                                                                                                           
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                         
                                                                                                                                    
