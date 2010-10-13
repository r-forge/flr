! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
!                                                                                                                                   
!     READER: This unit contains the principal I/O routines                                                                         
!                                                                                                                                   
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
																    
																    
																    
!/////////////////////////////////////////////////////////////////////////                                                          
																    
      Subroutine TableOut(Table)                                                                                                    
																    
!/////////////////////////////////////////////////////////////////////////                                                          
      implicit none                                                                                                                 
																    
      Include "INDAT.INC"                                                                                                           
      Include "SEPMODEL.INC"                                                                                                        
      Include "STATS.INC"                                                                                                           
      Include "SRR.INC"                                                                                                             
      Include "LABELS.INC"                                                                                                          
      Include "PREDIC.INC"                                                                                                          
      Include "MESSAGE1.INC"                                                                                                        
																    
!                                                                                                                                   
!     This is the principal output generating routine. Two files are created,                                                       
!     one which is the 'presentational' output (ICA.OUT) and one which is a                                                         
!     data transfer format (ICA.VIE) principally designed to pass data to                                                           
!     the ICAVIEW programme, but which is also useful for passing data to                                                           
!     spreadsheets etc.                                                                                                             
!                                                                                                                                   
!     Program calls:      LSFUN     (objective function, in order to calcualte residuals)                                          
!                          CALCSTATS  (calculates statistics of residuals)                                                          
!     The reason for putting these two calls in an output routine is that                                                           
!     it makes the routine more flexible; it can be called from many different                                                      
!     parts of the programmes.                                                                                                      
!                                                                                                                                   
!                                                                                                                                   
																    
																    
																    
! ---------------------------------------------------------------------                                                             
																    
																    
!      Local Variables                                                                                                              
																    
      Integer Table                 ! Not used here                                                                                 
																    
      Integer year, index,iyear,iage,                    &     ! Local lo&ping vars                                                  
       i, j, nparm, age                                                                                                             
      integer outf                                     ! Output file channel                                                        
      double precision TotalB,SpawnB                                                                                                
      double precision CalcSSB, aa, bb, cc             ! CalcSSB is a function                                                      
      logical UseRecr                                                                                                               
      integer pgp(maxsurvey)                                   ! whether last age in survey is a +gp                                
																    
																    
      integer cwidth, tlen, tscan
      external tscan
      character*1 condensed                                                                                                         
      character*77 Text, Underscore                                                                                                 
      character*5 ytext                                                                                                             
      character*6 Valid                                                                                                             
      integer pwidth                                                                                                                
      character*1 dummy                                                                                                             
      integer defaultlanguage                                                                                                       
      logical codechange                                                                                                            
      parameter (cwidth=8)                                   ! column width for tables                                              
																    
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!     ________________EXECUTABLE CODE___________________________________                                                            
																    
!------------------ Run the objective function to make sure the populations                                                         
!                      agree with the model parameters (unless inhibited by Table = -1)                                             
																    
																    
      outf = 8                                                                                                                      
      Underscore='--------------------------------------------------'   &                                                           
      //'----------'                                                                                                                
																    
      dummy = 'N'                                                                                                                   
																    
																    
      condensed=KO(4,Language)    
																    
      Call SCREEN_IN_A(HP(1,Language),condensed,KO(4,Language),         &                                                           
       Language)                                                                                                                    

      pwidth=132
      CAll GetPageWidth(pwidth,80,800)                                                                                              
																    
																    
																    
																    
																    
      DefaultLanguage=Language
																    
      If (Language .ne. 1) then  
	
	Call  Screen_in_a(HP(78,Language), dummy, KO(1,language),Language)                                                          
	if (Tscan(dummy, KY(1,Language)) .ne. 0) then                                                                               
	  DefaultLanguage=Language                                                                                                  
	  Language=1                                                                                                                
	endif
      endif


      If (Language .ne. 1) then                                                                                                     
																    
	Call Screen_in_a(HP(79,Language), dummy, KO(5,Language),Language)                                                           
	if (Tscan(dummy, KY(5,Language)) .ne. 0) then                                                                               
	   Call DosCodes                                                                                                            
	   CodeChange=.true.                                                                                                        
	endif                                                                                                                       
      endif
																    

																    
																    
																    
      open(outf, file = ica_out, status='unknown')                                                                                  
																    
      write(outf,*) HP(2,Language)                                                                                                  
      write(outf,*) Underscore(1:Tlen(HP(2,Language)))                                                                              
      write(outf,*)                                                                                                                 
      Call WriteHeader(MainTitle, pwidth, cwidth, outf)                                                                             
      write(outf,*)                                                                                                                 
																    
      CAll WriteMatrices(outf, condensed, pwidth,cwidth)                                                                            
      write(outf,*)                                                                                                                 
																    
      Call WriteStockSummary(outf)                                                                                                  
																    
      write(outf,*)                                                                                                                 
																    
      Call DocModel(outf)                                                                                                           
																    
																    
      write(outf,*)                                                                                                                 
      write(outf,*)                                                                                                                 
																    
      Call WriteParms(outf)                                                                                                         
      Call WriteResidualTable(outf, pwidth, cwidth)                                                                                 
																    
																    
! --------------------------DISTRIBUTION STATISTICS------------------------                                                         
																    
																    
      do i = 1,3                                                                                                                    
	 write(outf,*)                                                                                                                     
      enddo                                                                                                                         
																    
      write(outf,*) HP(4,Language)                                                                                                  
      write(outf,*) Underscore(1:Tlen(HP(4,Language)))                                                                              
      write(outf,*)                                                                                                                 
      Call IntToChar(lastyear-NySep+1,ytext,5)                                                                                      
      Call ConCat (Text,HP(5,Language),ytext)                                                                                       
      Call ConCat (Text,Text,HP(6,Language))                                                                                        
      Call IntToChar(lastyear,ytext,5)                                                                                              
      Call ConCat (Text,Text,ytext)                                                                                                 
      write(outf,*) Text                                                                                                            
																    
      write(outf,2107) HP(7,Language),CVar                                                                                          
      write(outf,2107) HP(8,Language),Cskew                                                                                         
      write(outf,2107) HP(9,Language),CKurt                                                                                         
      write(outf,2107) HP(10,Language),CChi2                                                                                        
      write(outf,2107) HP(11,Language),(1.-CPchi)                                                                                   
      write(outf,2106) HP(12,Language),NoCdata-                         &                                                           
       (NySep+NySep+(lastage-firstage+1-2)+(lastage-firstage+1-3))                                                                  
																    
      write(outf,*)                                                                                                                 
      write(outf,*)                                                                                                                 
																    
      if (nssbix .gt. 0) then                                                                                                       
      write(outf,*) HP(13,Language)                                                                                                 
      write(outf,*) Underscore(1:Tlen(HP(13,Language)))                                                                             
      write(outf,*)                                                                                                                 
      do index = 1, nssbix                                                                                                          
	write(outf,*)                                                                                                                      
	Call ConCat(Text,HP(14,Language),Bsurvlab(index))                                                                                  
	write(outf,777) Text                                                                                                               
777   format(' ',A79)                                                                                                               
	write(outf,*)                                                                                                                      
	write(outf,*)                                                                                                                      
	if (QBParm(index) .eq. 0) then                                                                                                     
	  write(outf,*) HP(15,Language)                                                                                                    
	else if (QBParm(index) .eq. 1) then                                                                                                
	  write(outf,*) HP(16,Language)                                                                                                    
	else if (QBparm(index) .eq. 2) then                                                                                                
	  write(outf,*) HP(17,Language)                                                                                                    
	endif                                                                                                                              
	if (plusgp(index)) write(outf,*) HP(3,Language)                                                                                    
	write(outf,*)                                                                                                                      
	write(outf,2107) HP(7,Language),BVar(index)                                                                                        
	write(outf,2107) HP(8,Language),Bskew(index)                                                                                       
	write(outf,2107) HP(9,Language),BKurt(index)                                                                                       
	write(outf,2107) HP(10,Language),Bchi2(index)                                                                                      
	write(outf,2107) HP(11,Language),(1.-BPchi(index))                                                                                 
	write(outf,2106) HP(70,Language),NoBdata(index)                                                                                    
	write(outf,2106) HP(12,Language),NoBdata(index)-                       &                                                           
	QBPArm(index)                                                                                                               
       write(outf,2107) HP(18,Language),Blambda(index)                                                                              
	write(outf,*)                                                                                                                      
      enddo ! SSB indices                                                                                                           
      endif ! any SSB indices                                                                                                       
																    
      if (nageix .gt. 0) then                                                                                                       
      write(outf,*) HP(19,Language)                                                                                                 
      write(outf,*) Underscore(1:Tlen(HP(19,Language)))                                                                             
																    
      write(outf,*)                                                                                                                 
      do index = 1, nageix                                                                                                          
	write(outf,*)                                                                                                                      
	Call ConCat(Text,HP(14,Language),Asurvlab(index))                                                                                  
	write(outf,777) Text                                                                                                               
	write(outf,*)                                                                                                                      
	write(outf,*)                                                                                                                      
	if (QAParm(index) .eq. 0) then                                                                                                     
	  write(outf,*) HP(15,Language)                                                                                                    
	else if (QAParm(index) .eq. 1) then                                                                                                
	  write(outf,*) HP(16,Language)                                                                                                    
	else if (QAparm(index) .eq. 2) then                                                                                                
	  write(outf,*) HP(17,Language)                                                                                                    
	endif                                                                                                                              
	write(outf,*)                                                                                                                      
	write(outf,2100) HP(20,Language),                                      &                                                           
		(age,age=fage(index),lage(index))                                                                                   
2100    format(A22,7X,13(I2,8X))                                                                                                    
2105    format(A22,13(F9.4,1X))                                                                                                     
2106    format(A35,7X,13(I2,8X))                                                                                                    
2107    format(A35,13(F9.4,1X))                                                                                                     
       write(outf,2105) HP(7,Language),(AVar(index,iage),               &                                                           
	    iage = 1,lage(index)-fage(index)+1)                                                                                     
       write(outf,2105)HP(8,Language),(Askew(index,iage),               &                                                           
	    iage = 1,lage(index)-fage(index)+1)                                                                                     
       write(outf,2105)HP(9,Language),(AKurt(index,iage),               &                                                           
	    iage = 1,lage(index)-fage(index)+1)                                                                                     
       write(outf,2105)HP(10,Language),(Achi2(index,iage),              &                                                           
	    iage = 1,lage(index)-fage(index)+1)                                                                                     
      write(outf,2105)HP(11,Language),(1d0-APchi(index,iage),           &                                                           
	    iage = 1,lage(index)-fage(index)+1)                                                                                     
       write(outf,2100)HP(70,Language),(NoAdata(index,iage),            &                                                           
	    iage = 1,lage(index)-fage(index)+1)                                                                                     
       write(outf,2100) HP(12,Language),(NoAdata(index,iage)            &                                                           
	    -QAParm(index), iage = 1,lage(index)-fage(index)+1)                                                                     
       write(outf,2105)HP(18,Language),(ALambda(index,iage),            &                                                           
	     iage = 1,lage(index)-fage(index)+1)                                                                                    
       write(outf,*)                                                                                                                
      enddo ! Aged indices                                                                                                          
      endif ! any aged indices                                                                                                      
																    
      CAll WriteAnov(outf)  ! Write analysis of variance table                                                                      
																    
																    
																    
      close(outf)               ! Terminates creation of ICA.OUT                                                                    
																    
      Call WriteFNR         ! write Lowestoft format output files                                                                   
      CAll WriteSen(ica_sen)         ! write SEN file                                                                               
      Call WriteSummFile(ica_sum)                                                                                                   
																    
      Call CalcStats(.false.)   ! recalculate to get unweighted variance estimates                                                  
																    
! --------------------------------------------------------------------------                                                        
!                                                                                                                                   
!      CREATION OF DATA TRANSFER FILE ICA.VIE FOR GRAPHICS PROGRAMME                                                                
!                                                                                                                                   
! --------------------------------------------------------------------------                                                        
																    
!------------------ this code is extremely straightforward                                                                          
																    
																    
      open(12, file = ica_vie, status = 'unknown')                                                                                  
      write(12,*) HP(21,Language)                                                                                                   
      year = 0                                                                                                                      
      If (TwoSel) year = 1                                                                                                          
      write(12,105) firstyear, lastyear, firstage,lastage,              &                                                           
	NySEP,  NSSBIX, NAGEIX, year                                                                                                
																    
105   format (7(I4,2X), I3)                                                                                                         
      write(12,*) HP(22,Language)                                                                                                   
      do year = firstyear, lastyear                                                                                                 
	write(12,500) year, LA(year-firstyear+1)                                                                                           
      enddo                                                                                                                         
      write(12,*) HP(23,Language)                                                                                                   
      do year = firstyear, (lastyear-Nysep) ! Conventional F, no s.d.                                                               
	write(12,510) year, F(year-firstyear+1,Refage), -1.0,-1.0                                                                          
      enddo                                                                                                                         
      j = 0                                                                                                                         
      do year = (lastyear-NySep+1), lastyear          ! these if-then-elses are to spot the                                         
	j = j+1                                       ! cases when there is no estimate of s.d.                                            
	aa = dexp(Xbest(j))                           ! for the parameters.                                                                
	if (Xlow(j) .ne. 0d0) then                                                                                                         
	  bb = dexp(Xlow(j))                                                                                                               
	else                                                                                                                               
	  bb = -1d0                                                                                                                        
	endif                                                                                                                              
	if (Xhigh(j) .ne. 0d0) then                                                                                                        
	  cc = dexp(Xhigh(j))                                                                                                              
	else                                                                                                                               
	  cc = -1d0                                                                                                                        
	endif                                                                                                                              
	write(12,510) year, aa,bb,cc                                                                                                       
      enddo                                                                                                                         
      write(12,*) HP(24,Language)                                                                                                   
      do year = firstyear, lastyear                                                                                                 
	write(12,520) year, N(year-firstyear+1,1), -1.0, -1.0                                                                              
      enddo                                                                                                                         
!           Fs       Ss                  starting poplns                                                                            
      j = (NySep+(lastage-firstage-2) + (lastage-firstage))                                                                         
      do year = (lastyear-Nysep+2), lastyear                                                                                        
	j = j+1                                                                                                                            
!        aa = dexp(Xbest(j))                    ! recruitments no longer written here.                                              
!        if (Xlow(j) .ne. 0d0) then             ! the entire population matrix is written out                                       
!          bb = dexp(Xlow(j))                   ! further down                                                                      
!        else                                                                                                                       
!          bb = -1d0                                                                                                                
!        endif                                                                                                                      
!        if (Xhigh(j) .ne. 0d0) then                                                                                                
!          cc = dexp(Xhigh(j))                                                                                                      
!        else                                                                                                                       
!          cc = -1d0                                                                                                                
!        endif                                                                                                                      
!        write(12,525) year, aa,bb,cc                                                                                               
      enddo                                                                                                                         
      if (UseRecr) then                                                                                                             
	j = j+1                                                                                                                            
	aa = dexp(Xbest(j))                                                                                                                
	if (Xlow(j) .ne. 0d0) then                                                                                                         
	  bb = dexp(Xlow(j))                                                                                                               
	else                                                                                                                               
	  bb = -1d0                                                                                                                        
	endif                                                                                                                              
	if (XHigh(j) .ne. 0d0) then                                                                                                        
	  cc = dexp(Xhigh(j))                                                                                                              
	else                                                                                                                               
	  cc = -1d0                                                                                                                        
	endif                                                                                                                              
	write(12,525)lastyear+1, aa, bb, cc                                                                                                
      else                                                                                                                          
	write(12,525)lastyear+1, N(lastyear-firstyear+2, 1),-1d0,-1d0                                                                      
      endif                                                                                                                         
																    
      write(12,*) HP(25,Language)                                                                                                   
      do year = firstyear,lastyear+1                                                                                                
	TotalB = 0d0                                                                                                                       
	do age = firstage, lastage                                                                                                         
	  TotalB = TotalB + N(year-firstyear+1,age-firstage+1)                 &                                                           
			  *dble(SW(year-firstyear+1,age-firstage+1))                                                                
	enddo ! ages                                                                                                                       
	SpawnB = CalcSSB(year)                                                                                                             
	write(12,530) year, TotalB, SpawnB                                                                                                 
      enddo ! years                                                                                                                 
																    
      write(12,*) HP(26,Language)                                                                                                   
      j = NySEP                                                                                                                     
      do age = firstage,lastage                                                                                                     
      if ((age.eq.REFAGE).or.(age .eq. lastage).or.(age.eq.lastage-1))  &                                                           
       then                                                                                                                         
	if (age .eq. refage) then                                                                                                          
	   write(12,520) age, 1.0, -1.0, -1.0                                                                                              
	else                                                                                                                               
	   write(12,525) age, TermS, -1.0, -1.0                                                                                            
	endif                                                                                                                              
      else                                                                                                                          
	j = j+1                                                                                                                            
	aa = dexp(Xbest(j))                                                                                                                
	if (Xlow(j) .ne. 0d0) then                                                                                                         
	  bb = dexp(Xlow(j))                                                                                                               
	else                                                                                                                               
	  bb = -1d0                                                                                                                        
	endif                                                                                                                              
	if (Xhigh(j) .ne. 0d0) then                                                                                                        
	  cc = dexp(Xhigh(j))                                                                                                              
	else                                                                                                                               
	  cc = -1d0                                                                                                                        
	endif                                                                                                                              
	write(12,540) age, aa,bb,cc                                                                                                        
      endif                                                                                                                         
      enddo ! ages                                                                                                                  
																    
      If (TwoSel) then                                                                                                              
	write(12,*) HP(27,Language)                                                                                                        
	j = NySEP+(lastage-firstage+1-3)                                                                                                   
	do age = firstage,lastage                                                                                                          
	if ((age.eq.REFAGE).or.(age .eq. lastage).or.(age.eq.lastage-1))       &                                                           
	 then                                                                                                                       
	  if (age .eq. refage) then                                                                                                        
	     write(12,520) age, 1.0, -1.0, -1.0                                                                                            
	  else                                                                                                                             
	     write(12,525) age, TermS2, -1.0, -1.0                                                                                         
	  endif                                                                                                                            
	else                                                                                                                               
	  j = j+1                                                                                                                          
	  aa = dexp(Xbest(j))                                                                                                              
	  if (Xlow(j) .ne. 0d0) then                                                                                                       
	    bb = dexp(Xlow(j))                                                                                                             
	  else                                                                                                                             
	    bb = -1d0                                                                                                                      
	  endif                                                                                                                            
	  if (Xhigh(j) .ne. 0d0) then                                                                                                      
	    cc = dexp(Xhigh(j))                                                                                                            
	  else                                                                                                                             
	    cc = -1d0                                                                                                                      
	  endif                                                                                                                            
	  write(12,540) age, aa,bb,cc                                                                                                      
	endif                                                                                                                              
	enddo ! ages                                                                                                                       
      endif ! second selection pattern fitted                                                                                       
      write(12,*) HP(28,Language)                                                                                                   
      do year = firstyear, lastyear+1                                                                                               
	write(12,550) (N(year-firstyear+1, age-firstage+1),                    &                                                           
	     age= firstage,lastage)                                                                                                 
      enddo  ! years                                                                                                                
      write(12,*) HP(29,Language)                                                                                                   
      do year = firstyear, lastyear+1                                                                                               
	write(12,552) (F(year-firstyear+1, age-firstage+1),                    &                                                           
	     age= firstage, lastage)                                                                                                
      enddo  ! years                                                                                                                
      write(12,*)HP(30,Language)                                                                                                    
      do year = firstyear, lastyear+1                                                                                               
	write(12,552) (NM(year-firstyear+1, age-firstage+1),                   &                                                           
	     age= firstage, lastage)                                                                                                
      enddo  ! years                                                                                                                
552   format(25(F10.7,1X))                                                                                                          
																	   
	 Nparm = 2*NYsep + (lastage-firstage-2)+(lastage-firstage-1)                                                                       
																    
	 If(TwoSel) Nparm =NParm+(lastage-firstage+1)-3                                                                                    
																    
	 UseRecr= .False.                                                                                                                  
	 do i = 1, NAgeix                                                                                                                  
	   if ((fage(i) .eq. firstage) .and.(lyear(i).eq.                      &                                                           
	     lastyear+1) .and.                                          &                                                           
	     (Aindex(i,lyear(i)-fyear(i)+1,1).ne.Missing))              &                                                           
		 UseRecr=.True.                                                                                                     
	 enddo                                                                                                                             
																	   
	 if (UseRecr) Nparm=Nparm+1                                                                                                        
																    
	 write(12,*) HP(31,Language)                                                                                                       
	 do index = 1, nssbix                                                                                                              
	   write(12,570) index, QBParm(index), dsqrt(BVar(index))                                                                          
	   do i = 1,QBParm(index)                                                                                                          
	     Nparm=Nparm+1                                                                                                                 
	     if (Xlow(Nparm) .eq. 0d0) Xlow(Nparm) = -1d0                                                                                  
	     if (Xhigh(Nparm).eq. 0d0) Xhigh(Nparm)= -1d0                                                                                  
	     write(12,540) index, Xbest(nparm),                                &                                                           
		     Xlow(nparm), Xhigh(nparm)                                                                                      
	   enddo ! catchability parameters                                                                                                 
	 enddo   ! SSB indices                                                                                                             
																    
	 if (Nageix .gt. 0)                                                    &                                                           
	     write(12,*) HP(32,Language)                                                                                            
	 do index = 1, nageix                                                                                                              
	 if (plusgp(index)) then                                                                                                           
	   pgp(index) =1                                                                                                                   
	 else                                                                                                                              
	   pgp(index) =0                                                                                                                   
	 endif                                                                                                                             
	 write(12,*) fage(index),lage(index), Timing(index), pgp(index),       &                                                           
	  QAParm(index)                                                                                                             
	 write(12,137) (dsqrt(AVar(index,iage)),                               &                                                           
		   iage=1, lage(index)-fage(index)+1)                                                                               
137      format(13(E20.11,1X))                                                                                                      
	 do age = fage(index), lage(index)                                                                                                 
	   do i =1,QAparm(index)                                                                                                           
	    Nparm = Nparm+1                                                                                                                
	    if (Xlow(Nparm) .eq. 0d0) Xlow(Nparm) = -1d0                                                                                   
	    if (Xhigh(Nparm).eq. 0d0) Xhigh(Nparm)= -1d0                                                                                   
	    write(12,560) index, age, Xbest(Nparm),                            &                                                           
	    Xlow(Nparm), Xhigh(Nparm)                                                                                               
	   enddo ! parameters                                                                                                              
	 enddo ! ages                                                                                                                      
		  write(12,*)                                                                                                                     
	 enddo ! indices                                                                                                                   
																    
      close(12)                                                                                                                     
																    
      if (CodeChange) then                                                                                                          
	Call WinCodes                                                                                                               
      endif                                                                                                                         
																    
      language=DefaultLanguage                                                                                                      
																    
500   format (I4, 2X, F20.4)                                                                                                        
510   format (I4, 2X, 3(F15.4,1X))                                                                                                  
520   format (I4, 2X, 3(F15.0,1X))                                                                                                  
525   format (I4, 2X, 3(E15.5,1X))                                                                                                  
530   format (I4, 2X, 2(F15.0,1X))                                                                                                  
540   format (I4, 2X, 3(F15.8,1X))                                                                                                  
550   format (21(F15.0,1X))                                                                                                         
560   format (I4, 2X, I4, 2X, 3(F20.12,1X))                                                                                         
570   format (I4, 2X, I4, 1X,E18.8)                                                                                                 
																    
																    
																    
      return ! from subroutine Table                                                                                                
      end                                                                                                                           
																    
																    
																    
! ///////////////////////////////////////////////////////////////////////                                                           
																    
      Subroutine WriteMatrices(outf, condensed, pwidth, cwidth)                                                                     
																    
! ///////////////////////////////////////////////////////////////////////                                                           
																    
      include "INDAT.INC"                                                                                                           
      include "LABELS.INC"                                                                                                          
      include "SEPMODEL.INC"                                                                                                        
      include "PREDIC.INC"                                                                                                          
      include "MESSAGE1.INC"                                                                                                        
      integer tlen                                                                                                                  
      character*77 text, Underscore                                                                                                 
      character*5 ytext                                                                                                             
																    
      integer outf, pwidth, cwidth, syear, iyear, year, age, iage                                                                   
      character*1 condensed, dummy                                                                                                  
																    
      Character*50 Title                                                                                                            
      double precision Data(maxyear,maxage), autoscale                                                                              
      Character*3 ast                                                                                                               
																    
      autoscale = 100d0 ! forces automatic scaling                                                                                  
																    
      ast = HP(40,Language) ! labelling for age column                                                                              
																    
      Underscore = '------------------------------------------------'   &                                                           
      //'-----------------'                                                                                                         
																    
      if ((condensed .eq. 'n') .or. (condensed .eq.'N')                 &                                                           
	 .or.(condensed .eq. 'd') .or. (condensed .eq. 'D')  ) then                                                                 
																    
	Title=HP(33,Language)                                                                                                              
																    
	   do year = firstyear, lastyear                                                                                            
	     iyear =year-firstyear+1                                                                                                       
	     do iage=1,lastage-firstage+1                                                                                           
	       if (CN(iyear,iage) .ne. Missing) then                                                                                
		 Data(iyear,iage)=CN(iyear,iage)*1000d0                                                                             
	       else                                                                                                                 
		 Data(iyear,iage)=missing                                                                                           
	       endif                                                                                                                
	     enddo                                                                                                                         
	   enddo                                                                                                                           
																    
	Call WriteTable(Title,maxyear,maxage, Data,                     &                                                           
	 firstyear,                                                     &                                                           
	 lastyear,firstage,lastage,cwidth, pwidth, outf,ast,missing,    &                                                           
	 autoscale)                                                                                                                 
																    
	 if ((condensed .eq. 'd') .or. (condensed .eq. 'D')) then                                                                          
	   Title = HP(34,Language)                                                                                                         
																    
	   do year = lastyear-NySep+1, lastyear                                                                                            
	     iyear =year-firstyear+1                                                                                                       
	     syear = year-(lastyear-NySep+1)+1                                                                                             
	     do iage=1,lastage-firstage+1-1                                                                                                
	       Data(syear,iage)=(PredCN(iyear,iage)*1000d0)                                                                         
	     enddo                                                                                                                         
	   enddo                                                                                                                           
																    
	   iyear=lastyear-NySep+1                                                                                                          
	   iage = lastage-1                                                                                                                
	   CAll WriteTable(Title,maxyear,maxage,Data,                          &                                                           
	     iyear,lastyear,firstage,iage,cwidth,pwidth,                &                                                           
	     outf,ast,missing, autoscale )                                                                                          
																    
																    
																    
																    
																    
	 endif ! extended output                                                                                                           
																    
																    
																    
	Title=HP(35,Language)                                                                                                              
																    
	Call WriteTable(Title, maxyear,maxage,CW,firstyear,                    &                                                           
	 lastyear,firstage,lastage,cwidth, pwidth, outf,ast,missing,    &                                                           
	 0d0)                                                                                                                       
																    
	Title=HP(36,Language)                                                                                                              
	Call WriteTable(Title, maxyear,maxage,SW,firstyear,                    &                                                           
	 lastyear,firstage,lastage,cwidth, pwidth, outf,ast,missing,    &                                                           
	 0d0)                                                                                                                       
																			 
	Title=HP(37,Language)                                                                                                              
	Call WriteTable(Title, maxyear,maxage,NM,firstyear,                    &                                                           
	 lastyear,firstage,lastage,cwidth, pwidth, outf,ast,missing,    &                                                           
	 0d0 )                                                                                                                      
																    
	Title=HP(38,Language)                                                                                                              
	Call WriteTable(Title, maxyear,maxage,MO,firstyear,                    &                                                           
	 lastyear,firstage,lastage,cwidth, pwidth, outf,ast,missing,    &                                                           
	 0d0)                                                                                                                       
																    
	 if (nssbix .gt. 0) then                                                                                                           
																    
	   write(outf,*) HP(39,Language)                                                                                                   
	   write(outf,*) Underscore(1:Tlen(HP(39,Language)))                                                                               
	   write(outf,*)                                                                                                                   
																    
	   do index = 1, nssbix                                                                                                            
	     do iyear=1,lbyear-fbyear+1                                                                                                    
		if (Bindex(index,iyear) .ne. MISSING) then                                                                                        
		  Data(iyear,1)=dexp(Bindex(index,iyear))                                                                                         
		else                                                                                                                              
		  Data(iyear,1)= MISSING                                                                                            
		endif                                                                                                                             
	     enddo                                                                                                                         
																	   
	     Title = BSurvLab(index)                                                                                                       
	     Call WriteTable(BSurvLab(index),maxyear,maxage,                   &                                                           
	      data,fbyear,                                              &                                                           
	     lbyear,1,1,cwidth, pwidth, outf,'   ',missing,autoscale)                                                               
																	   
	   enddo ! SSB indices                                                                                                             
	 endif ! any SSB indices                                                                                                           
																    
																    
! ----------------- Write the table of age-structured indices                                                                       
	 If (NageIx .gt. 0) then                                                                                                           
	   write(outf,*) HP(40,Language)                                                                                                   
	   write(outf,*) Underscore(1:Tlen(HP(40,Language)))                                                                               
	   write(outf,*)                                                                                                                   
	 endif                                                                                                                             
																    
	 do index = 1, nageix                                                                                                              
	   dummy=CHAR(index+48)                                                                                                            
	   Title =  ASurvLab(index)                                                                                                        
	   do iage=1,lage(index)-fage(index)+1                                                                                             
	     do iyear=1,lyear(index)-fyear(index)+1                                                                                        
	       If (Aindex(index,iyear,iage) .ne. MISSING ) then                                                                            
		 Data(iyear,iage)=dexp(Aindex(index,iyear,iage))                                                                                  
	       else                                                                                                                        
		 Data(iyear,iage)=-99d0                                                                                                           
	       endif                                                                                                                       
	     enddo                                                                                                                         
	   enddo                                                                                                                           
																    
	   Call WriteTable(Title,maxyear,maxage,data,fyear(index),             &                                                           
	     lyear(index),fage(index),lage(index),cwidth, pwidth,       &                                                           
	     outf,ast,missing,autoscale)                                                                                            
	 enddo ! age-structured indices                                                                                                    
																    
																	   
																    
       endif ! Condensed output requested                                                                                           
																    
																    
																    
! ----------------- Write the table of fishing mortalities                                                                          
																	   
	Title =HP(41,Language)                                                                                                             
	Call WriteTable(Title,maxyear,maxage,F,firstyear,                      &                                                           
	 lastyear,firstage,lastage,cwidth, pwidth, outf,ast,missing,    &                                                           
	 0d0)                                                                                                                       
																    
																    
	Title =HP(42,Language)                                                                                                             
	iyear = lastyear+1                                                                                                                 
																    
																    
	   do year = firstyear, lastyear+1                                                                                          
	     iyear =year-firstyear+1                                                                                                       
	     do iage=1,lastage-firstage+1                                                                                           
	       Data(iyear,iage)=(N(iyear,iage)*1000d0)                                                                              
	     enddo                                                                                                                         
	   enddo                                                                                                                           
																    
																    
	Call WriteTable(Title,maxyear,maxage,Data,firstyear,            &                                                           
	 lastyear+1,firstage,lastage,cwidth, pwidth, outf,ast,          &                                                           
	  missing,autoscale)                                                                                                        
																	   
																			 
	If ((condensed .eq. 'd').or. (condensed .eq. 'D')) then                                                                            
																	   
!         The weighting factors for the catches at age                                                                              
																    
	  do year= lastyear-NySep+1, lastyear                                                                                              
	    syear = year-(lastyear-NySep+1)+1                                                                                              
	    iyear = year-firstyear+1                                                                                                       
	    do iage=1,lastage-firstage+1-1                                                                                                 
	      Data(syear,iage)=                                                &                                                           
	       W(iyear,iage)*W(iyear,iage)                                                                                          
	    enddo                                                                                                                          
	  enddo                                                                                                                            
																	   
	  Title= HP(71,Language)                                                                                                           
	  iyear=lastyear-NySep+1                                                                                                           
	  iage=lastage-1                                                                                                                   
	  Call WriteTable(Title, maxyear,maxage,Data,iyear,                    &                                                           
	   lastyear,firstage,iage,cwidth, pwidth, outf,ast,missing,     &                                                           
	   0d0)                                                                                                                     
																    
																    
																    
	 if (nssbix .gt. 0) then                                                                                                           
																    
	   write(outf,*) HP(43,Language)                                                                                                   
	   write(outf,*) Underscore(1:Tlen(HP(43,Language)))                                                                               
	   write(outf,*)                                                                                                                   
																    
	   do index = 1, nssbix                                                                                                            
	     do iyear=1,lbyear-fbyear+1                                                                                                    
		if (Bindex(index,iyear) .ne. MISSING) then                                                                                        
		  Data(iyear,1)=dexp(PredBindex(index,iyear))                                                                                     
		else                                                                                                                              
		  Data(iyear,1)=MISSING                                                                                             
		endif                                                                                                                             
	     enddo                                                                                                                         
																	   
	     Title = BSurvLab(index)                                                                                                       
	     Call WriteTable(BSurvLab(index),maxyear,maxage,                   &                                                           
	      data,fbyear,                                              &                                                           
	     lbyear,1,1,cwidth, pwidth, outf,'   ',missing,autoscale)                                                               
																	   
	   enddo ! SSB indices                                                                                                             
	 endif ! any SSB indices                                                                                                           
																    
																    
! ----------------- Write the table of age-structured indices                                                                       
																    
	 if ( Nageix .gt. 0) then                                                                                                   
	   write(outf,*) HP(44,Language)                                                                                            
	   write(outf,*) Underscore(1:Tlen(HP(44,Language)))                                                                        
	   write(outf,*)                                                                                                            
	 endif                                                                                                                      
	 do index = 1, nageix                                                                                                              
	   dummy=CHAR(index+48)                                                                                                            
	   Call ConCat(Title, ASurvLab(index), HP(72,Language))                                                                            
	   do iage=1,lage(index)-fage(index)+1                                                                                             
	     do iyear=1,lyear(index)-fyear(index)+1                                                                                        
	       If (Aindex(index,iyear,iage) .ne. MISSING) then                                                                             
		 Data(iyear,iage)=dexp(PredAindex(index,iyear,iage))                                                                              
	       else                                                                                                                        
		 Data(iyear,iage)=MISSING                                                                                                         
	       endif                                                                                                                       
	     enddo                                                                                                                         
	   enddo                                                                                                                           
																    
	   Call WriteTable(Title,maxyear,maxage,data,fyear(index),             &                                                           
	     lyear(index),fage(index),lage(index),cwidth, pwidth,       &                                                           
	     outf,ast,missing,autoscale)                                                                                            
	 enddo ! age-structured indices                                                                                                    
																    
!        The fitted selection pattern                                                                                               
																    
	Title = HP(73,Language)                                                                                                            
	do iyear=1, lastyear-firstyear+1                                                                                                   
	  do iage= 1, lastage-firstage+1                                                                                                   
	    Data(iyear,iage)=F(iyear,iage)/F(iyear,refage-firstage+1)                                                                      
	  enddo                                                                                                                            
	enddo                                                                                                                              
																    
	Call WriteTable(Title,maxyear,maxage,data,firstyear,                   &                                                           
	     lastyear,firstage,lastage,cwidth, pwidth,                  &                                                           
	     outf,ast,missing, 0d0)                                                                                                 
																    
																    
	endif ! extended output requested                                                                                                  
																    
      return                                                                                                                        
      end                                                                                                                           
																    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                         
																    
																    
																    
! ////////////////////////////////////////////////////////////////////////                                                          
																    
       Subroutine WriteStockSummary(outf)                                                                                           
																    
! /////////////////////////////////////////////////////////////////////////                                                         
																    
      include "INDAT.INC"                                                                                                           
      include "LABELS.INC"                                                                                                          
      include "SEPMODEL.INC"                                                                                                        
      include "MESSAGE1.INC"                                                                                                        
      integer tlen                                                                                                                  
      character*77 text, Title                                                                                                      
      character*5 ytext                                                                                                             
																    
      integer outf, iyear, iage                                                                                                     
      double precision SSB(maxyear), TotB(maxyear)               ! spawning and total biomasses                                     
      double precision RefF(maxyear)                                    ! reference F                                               
      double precision CalcSSB                                                                                                      
      character*1 ColDel ! Column delimiter symbol                                                                                  
      Character*132 LineText                                                                                                        
      Character*2 AgeLabel(2)                                                                                                       
																    
																    
      Parameter (ColDel = char(179))                                                                                                
																    
																    
! ----------------- Stock summary table: spawning biomass, recruitment, Mean F, etc.                                                
																    
																    
																    
																    
																    
																    
      do iyear = 1, lastyear-firstyear+1                                                                                            
	SSB(iyear) = CalcSSB(iyear+firstyear-1)                                                                                            
	TotB(iyear) = 0.0                                                                                                                  
	RefF(iyear) = 0.0                                                                                                                  
	do iage = 1, lastage-firstage+1                                                                                                    
	  TotB(iyear)=TotB(iyear)+N(iyear,iage)*SW(iyear,iage)                                                                             
	enddo ! ages                                                                                                                       
	do iage = loFage-firstage+1, HiFage-firstage+1                                                                                     
	  RefF(iyear)=RefF(iyear)+F(iyear,iage)/dble(HiFage-LoFage+1)                                                                      
	enddo                                                                                                                              
      enddo ! years                                                                                                                 
																    
      write(outf,*)                                                                                                                 
      Title =       '                   '//HP(45,Language)                                                                          
      write(outf,*) Title                                                                                                           
      write(outf,*)                                                                                                                 
																    
!      Call WriteLine(outf, 2,77,char(196))                                                                                         
																    
      write(outf,*)                                                                                                                 
																    
      write(outf,'(A)') HP(46,Language)                                                                                             
      Title=HP(47,Language)                                                                                                         
      Title(19:19) = char(48+firstage) ! this inserts age at recruitment                                                            
      write(outf,'(A)') Title                                                                                                       
																    
																    
      if (LoFage .le. 9) then                                                                                                       
	AgeLabel(1)=' '//char( LoFage - (10*(LoFage/10))+48)//' '                                                                          
      else                                                                                                                          
	AgeLabel(1)= char(  (LoFage/10) + 48)//                                &                                                           
	 char( LoFage - (10*(LoFage/10))+48)//' '                                                                                   
      endif                                                                                                                         
      if (HiFage .le. 9) then                                                                                                       
	AgeLabel(2)=' '//char( HiFage - (10*(HiFage/10))+48)//' '                                                                          
      else                                                                                                                          
	AgeLabel(2)= char(  (HiFage/10) + 48)//                                &                                                           
	 char( HiFage - (10*(HiFage/10))+48)//' '                                                                                   
      endif                                                                                                                         
																    
      Title=HP(48,Language)   ! put the age-range for mean F in the title line                                                      
      Title(63:64)=AgeLabel(1)                                                                                                      
      Title(65:65)='-'                                                                                                              
      Title(66:67)=AgeLabel(2)                                                                                                      
      Write(outf,'(A)') Title                                                                                                       
																    
																    
!      Call WriteLine(outf, 2,77, char(196))                                                                                        
      write(outf,*)                                                                                                                 
																    
505   Format(A132)                                                                                                                  
																    
      do iyear = 1, lastyear-firstyear+1                                                                                            
	write(outf,160) iyear+firstyear-1, int(N(iyear,1)/10),                 &                                                           
	 int(TotB(iyear)),                                              &                                                           
	 int(SSB(iyear)),int(LA(iyear)), LA(iyear)/SSB(iyear),          &                                                           
	 RefF(iyear),                                                   &                                                           
	 int(SOP(iyear))                                                                                                            
      enddo                                                                                                                         
160   format(3X,I4,3X,I9,'0',3X,I7,3X,I7,3X,I7,3X,F6.4,3X,F6.4,3X,I3)                                                               
																    
      return                                                                                                                        
      end ! of routine WriteStock Summary                                                                                           
																    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                         
																    
																    
! ////////////////////////////////////////////////////////////////////////                                                          
																    
       Subroutine Writeline(fch, start, end, symbol)                                                                                
																    
! ////////////////////////////////////////////////////////////////////////                                                          
																    
       integer fch, start, end, i                                                                                                   
       character*1 symbol                                                                                                           
       character*500 text                                                                                                           
																    
       do i= 1, start                                                                                                               
	 Text(i:i) = ' '                                                                                                                   
       enddo                                                                                                                        
																    
       do i=start+1,end-1                                                                                                           
	 Text(i:i) = symbol                                                                                                                
       enddo                                                                                                                        
       write(fch,'(A)') Text(1:end)                                                                                                 
																    
																    
       return                                                                                                                       
       end ! of subroutine WriteLine                                                                                                
																    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
																    
																    
																    
																    
! //////////////////////////////////////////////////////////////////                                                                
																    
      Subroutine WriteParms(fch)                                                                                                    
																    
! //////////////////////////////////////////////////////////////////                                                                
																    
      include "INDAT.INC"                                                                                                           
      include "SEPMODEL.INC"                                                                                                        
      include "LABELS.INC"                                                                                                          
      include "SRR.INC"                                                                                                             
      include "MESSAGE1.INC"                                                                                                        
      integer tlen                                                                                                                  
      character*77 text                                                                                                             
      character*5 ytext                                                                                                             
																    
																    
      character*1 ColDel ! Column delimiter symbol                                                                                  
      Character*132 LineText                                                                                                        
																    
      integer fch, year, age, nparm, i                                                                                              
      double precision OutItem(7), CV                                                                                               
      logical ExpOutput   ! Whether to print output in exponential notation                                                         
      logical UseRecr     ! Whether there is a survey prediction of recruitment                                                     
																    
      ColDel = (char(179))  ! column delimiter symbol                                                                               
!                                                                                                                                   
!                                                                                                                                   
!------------------ The parameter list for the model proper                                                                         
!                          If there are no estimates for the +/- s.d.                                                               
!                          then these are flagged with '-1'                                                                         
!                                                                                                                                   
!               Titles of each section are highlighted by :  <----------                                                            
!                                                                                                                                   
!                                                                                                                                   
																    
																    
      write(fch,*) HP(49,Language)                                                                                                  
      write(fch,*)                                                                                                                  
      write(fch,*) HP(50,Language)                                                                                                  
      write(fch,*) HP(51,Language)                                                                                                  
      write(fch,*) HP(52,Language)                                                                                                  
																    
																    
!     Write the header strip                                                                                                        
																    
																    
!     First the F and S for the model                                                                                               
																    
      nparm = 0                                                                                                                     
																    
      write(fch,*) HP(74,Language)                                                                                                  
																    
      do year = lastyear-nysep+1, lastyear                                                                                          
	Nparm = Nparm+1                                                                                                                    
	CV = Xhigh(Nparm)-Xbest(Nparm)                                                                                                     
	write(fch,9030) Nparm, year, dexp(Xbest(Nparm)),                       &                                                           
	 int(100.*CV), dexp( Xbest(Nparm) - 1.96*CV),                   &                                                           
	dexp(Xbest(Nparm)+1.96*CV),                                     &                                                           
	dexp(Xbest(Nparm)-CV), dexp(Xbest(Nparm)+CV),                   &                                                           
	dexp(Xbest(Nparm)+CV*CV/2d0)                                                                                                
      enddo                                                                                                                         
																    
																    
																    
!     WRITE OUT SELECTION PATTERN                                                                                                   
!     ---------------------------                                                                                                   
																    
      write(fch,*)                                                                                                                  
      If (.not. TwoSel) then                                                                                                        
	write(fch,*) HP(53,Language)                                                                                                       
      else                                                                                                                          
	call IntToChar(lastyear-NySep+1, ytext, 5)                                                                                         
	call ConCat(Text,HP(54,Language),Ytext)                                                                                            
	call IntToChar(ChangeSel, ytext, 5)                                                                                                
	call ConCat(Text, Text, ytext)                                                                                                     
	write(fch,*) Text                                                                                                                  
      endif                                                                                                                         
																    
      do age = firstage, lastage-1                                                                                                  
	if ((age .ne. refage) .and. (age .ne. lastage-1)) then                                                                             
	  Nparm=Nparm+1                                                                                                                    
	  CV = Xhigh(Nparm)-Xbest(Nparm)                                                                                                   
	  write(fch,9030) Nparm, age, dexp(Xbest(Nparm)),                      &                                                           
	  int(100.*CV), dexp( Xbest(Nparm) - 1.96*CV),                  &                                                           
	  dexp(Xbest(Nparm)+1.96*CV),                                   &                                                           
	  dexp(Xbest(Nparm)-CV), dexp(Xbest(Nparm)+CV),                 &                                                           
	  dexp(Xbest(Nparm)+CV*CV/2d0)                                                                                              
	endif                                                                                                                              
	if (Refage .eq. Age)                                                   &                                                           
	write(fch,9050) refage,1d0,HP(56,Language)                                                                                  
	if (age .eq. lastage-1)                                                &                                                           
	write(fch, 9050) age,TermS,HP(57,Language)                                                                                  
      enddo ! ages                                                                                                                  
																	   
      If (TwoSel) then                                                                                                              
	write(fch,*)                                                                                                                       
	write(fch,*)                                                                                                                       
	Call IntToChar(ChangeSel+1,ytext,5)                                                                                                
	Call ConCat(Text, HP(55,Language), ytext)                                                                                          
	Call ConCat(Text, Text,HP(75,Language))                                                                                            
	Call IntToChar(lastyear,ytext,5)                                                                                                   
	Call ConCat(Text, Text,ytext)                                                                                                      
	write(fch,*) Text                                                                                                                  
	do age = firstage, lastage-1                                                                                                       
	 if ((age .ne. refage) .and. (age .ne. lastage-1)) then                                                                            
	   Nparm=Nparm+1                                                                                                                   
	   CV = Xhigh(Nparm)-Xbest(Nparm)                                                                                                  
	   write(fch,9030) Nparm, age, dexp(Xbest(Nparm)),                     &                                                           
	   int(100.*CV), dexp( Xbest(Nparm) - 1.96*CV),                 &                                                           
	   dexp(Xbest(Nparm)+1.96*CV),                                  &                                                           
	   dexp(Xbest(Nparm)-CV), dexp(Xbest(Nparm)+CV),                &                                                           
	   dexp(Xbest(Nparm)+CV*CV/2d0)                                                                                             
	 endif                                                                                                                             
	 if (Refage .eq. Age)                                                  &                                                           
	   write(fch,9050) refage,1d0, HP(56,Language)                                                                              
	   if (age .eq. lastage-1)                                             &                                                           
	   write(fch, 9050) age,TermS2,HP(57,Language)                                                                              
	 enddo ! ages                                                                                                                      
      endif ! need to write out the second selection pattern                                                                        
																    
																    
!     POPULATION ABUNDANCES                                                                                                         
																    
      write(fch,*)                                                                                                                  
      call IntToChar(lastyear,ytext,5)                                                                                              
      call ConCat(Text,HP(58,Language),Ytext)                                                                                       
																    
      write(fch,*) Text                                                                                                             
      do age = firstage, lastage-1                                                                                                  
	Nparm=Nparm+1                                                                                                                      
																	   
	Call ItemList(NParm, OutItem, ExpOutput)                                                                                           
																	   
	if (.not. ExpOutput) then                                                                                                          
	  Write(fch, 1060) Nparm, age, (int(OutItem(i)), i=1,7)                                                                            
	else                                                                                                                               
	  Write(fch, 1061) Nparm, age, OutItem(1), int(OutItem(2)),            &                                                           
	   (OutItem(i), i=3,7)                                                                                                      
	endif                                                                                                                              
      enddo ! years                                                                                                                 
																    
																    
1060    format(3X,I2,3X,I4,2X,I9,1X,I3,1X,5(1X,I9))                                                                                 
1061    format(3X,I2,3X,I4,2X,E9.4,1X,I3,1X,5(1X,E9.4))                                                                             
																    
      write(fch,*)                                                                                                                  
																    
																    
      call IntToChar(lastage-1,ytext(1:3),3)                                                                                        
      call ConCat(Text,HP(59,Language),Ytext(1:3))                                                                                  
      write(fch,9029) Text                                                                                                          
9029  format (A35,1X,I2)                                                                                                            
																    
      do year= lastyear-nysep+1, lastyear-1                                                                                         
	Nparm=Nparm+1                                                                                                                      
	Call ItemList(NParm, OutItem,ExpOutput)                                                                                            
																	   
	if (.not. ExpOutput) then                                                                                                          
	  Write(fch, 1060) Nparm, year, (int(OutItem(i)), i=1,7)                                                                           
	else                                                                                                                               
	  Write(fch, 1061) Nparm, year, OutItem(1),Int(OutItem(2)),            &                                                           
	   (OutItem(i), i=3,7)                                                                                                      
	endif                                                                                                                              
      enddo ! years                                                                                                                 
																    
																    
!  The recruitment prediction                                                                                                       
																    
      UseRecr = .false.                                                                                                             
																    
      do index = 1, nageix                                                                                                          
	if ((fage(index) .eq. firstage) .and. (lyear(index).eq.                &                                                           
	       lastyear+1)) UseRecr = .true.                                                                                        
      enddo                                                                                                                         
																    
      If (UseRecr) then                                                                                                             
	write(fch,*)                                                                                                                       
																    
	call IntToChar(lastyear+1,ytext,5)                                                                                                 
	call ConCat(Text,HP(60,Language),Ytext)                                                                                            
	write(fch,*) Text                                                                                                                  
	Nparm = Nparm+1                                                                                                                    
	Call ItemList(NParm, OutItem, ExpOutput)                                                                                           
	if (.not. ExpOutput) then                                                                                                          
	  Write(fch, 1060) Nparm, year, (int(OutItem(i)), i=1,7)                                                                           
	else                                                                                                                               
	  Write(fch, 1061) Nparm, year, OutItem(1),int(OutItem(2)),            &                                                           
	   (OutItem(i), i=3,7)                                                                                                      
	endif                                                                                                                              
      endif                                                                                                                         
																	   
																	   
      write(fch,*)                                                                                                                  
																	   
	    if (nssbix .gt. 0) then                                                                                                        
	      write(fch,*) HP(61,Language)                                                                                                 
	      do index = 1, nssbix                                                                                                         
		write(fch,*) BSurvLab(index)                                                                                                      
		if (QBparm(index) .eq. 0 ) then                                                                                                   
		  write(fch,*) HP(63,Language)                                                                                      
		endif                                                                                                                             
		if (QBparm(index) .eq. 1 ) then                                                                                                   
		write(fch,*) HP(64,Language)                                                                                        
		Nparm = Nparm +1                                                                                                                  
		CV= Xhigh(nparm) - Xbest(nparm)                                                                                                   
		write(fch,8000)Nparm,index,' Q ',                                     &                                                           
		dexp(Xbest(Nparm)),int(CV*100.),                        &                                                           
		dexp(Xhigh(Nparm)-1.96*CV),dexp(Xhigh(Nparm)+1.96*CV),  &                                                           
		dexp(Xhigh(Nparm)-CV),dexp(Xhigh(Nparm)+CV),            &                                                           
		dexp(Xhigh(Nparm)+CV*CV/2.)                                                                                         
																		  
		endif ! QBparm = 1                                                                                                                
		if (QBparm(index) .eq. 2) then                                                                                                    
		write(fch,*) HP(65,Language)                                                                                        
		Nparm = Nparm+1                                                                                                                   
		write(fch,8000)Nparm,index,' Q ',                                     &                                                           
		dexp(Xbest(Nparm)),int(CV*100.),                        &                                                           
		dexp(Xhigh(Nparm)-1.96*CV),dexp(Xhigh(Nparm)+1.96*CV),  &                                                           
		dexp(Xhigh(Nparm)-CV),dexp(Xhigh(Nparm)+CV),            &                                                           
		dexp(Xhigh(Nparm)+CV*CV/2.)                                                                                         
																		  
		Nparm = Nparm+1                                                                                                                   
		write(fch,8000)Nparm,index,' K ',                                     &                                                           
		dexp(Xbest(Nparm)),int(CV*100.),                        &                                                           
		dexp(Xhigh(Nparm)-1.96*CV),dexp(Xhigh(Nparm)+1.96*CV),  &                                                           
		dexp(Xhigh(Nparm)-CV),dexp(Xhigh(Nparm)+CV),            &                                                           
		dexp(Xhigh(Nparm)+CV/2.)                                                                                            
																    
		endif ! Power model                                                                                                               
8000            format (3X,I2,2X,I2,1X,A3,1X,G9.4,1X,I3,5(1X,G9.4))                                                                 
	      enddo  ! indices                                                                                                             
	    endif ! there are any ssb indices                                                                                              
	    write(fch,*)                                                                                                                   
	    write(fch,*)                                                                                                                   
																    
	    if (nageix .gt. 0) then                                                                                                        
	      write(fch,*)                                                                                                                 
	      write(fch,*) HP(62,Language)                                                                                                 
	      do index = 1, nageix                                                                                                         
		write(fch,8011) ASurvLab(index)                                                                                                   
		write(fch,*)                                                                                                                      
		if (QAParm(index) .eq. 0) then                                                                                                    
		  write(fch,*) HP(63,Language)                                                                                                    
		  write(fch,*)                                                                                                                    
		endif                                                                                                                             
		if (QAParm(index) .eq. 1) then                                                                                                    
		  write(fch,*) HP(64,Language)                                                                                                    
		  do age = fage(index), lage(index)                                                                                               
		    Nparm = Nparm+1                                                                                                               
		    CV= Xhigh(nparm) - Xbest(nparm)                                                                                               
		    write(fch,8000)Nparm,age,' Q ',                                   &                                                           
		    dexp(Xbest(Nparm)),int(CV*100.),                    &                                                           
		    dexp(Xhigh(Nparm)-1.96*CV),                         &                                                           
		     dexp(Xhigh(Nparm)+1.96*CV),                        &                                                           
		    dexp(Xhigh(Nparm)-CV),dexp(Xhigh(Nparm)+CV),        &                                                           
		    dexp(Xhigh(Nparm)+CV*CV/2.)                                                                                     
		  enddo ! ages                                                                                                                    
		  write(fch,*)                                                                                                                    
		endif                                                                                                                             
		if (QAParm(index) .eq. 2) then                                                                                                    
		  write(fch,*) HP(65,Language)                                                                                                    
		  do age = fage(index), lage(index)                                                                                               
		    Nparm = Nparm+1                                                                                                               
		    CV= Xhigh(nparm) - Xbest(nparm)                                                                                               
		    write(fch,8000)Nparm,age,' Q ',                                   &                                                           
		    dexp(Xbest(Nparm)),int(CV*100.),                    &                                                           
		    dexp(Xhigh(Nparm)-1.96*CV),                         &                                                           
		    dexp(Xhigh(Nparm)+1.96*CV),                         &                                                           
		    dexp(Xhigh(Nparm)-CV),dexp(Xhigh(Nparm)+CV),        &                                                           
		    dexp(Xhigh(Nparm)+CV*CV/2.)                                                                                     
		    Nparm = Nparm+1                                                                                                               
		    CV= Xhigh(nparm) - Xbest(nparm)                                                                                               
		    write(fch,8000)Nparm,age,' K ',                                   &                                                           
		    dexp(Xbest(Nparm)),int(CV*100.),                    &                                                           
		    dexp(Xhigh(Nparm)-1.96*CV),                         &                                                           
		    dexp(Xhigh(Nparm)+1.96*CV),                         &                                                           
		    dexp(Xhigh(Nparm)-CV),dexp(Xhigh(Nparm)+CV),        &                                                           
		    dexp(Xhigh(Nparm)+CV*CV/2.)                                                                                     
		  enddo ! ages                                                                                                                    
		endif ! Power model                                                                                                               
		write(fch,*)                                                                                                                      
	       enddo ! indices                                                                                                             
	    endif ! any age-structured indices                                                                                             
																    
	    If (FitSRR) then                                                                                                               
	      write(fch,*)                                                                                                                 
	      write(fch,*) HP(66,Language)                                                                                                 
	      Nparm = Nparm+1                                                                                                              
	      CV= Xhigh(nparm) - Xbest(nparm)                                                                                              
	      write(fch,8000)Nparm,age,' a ',                                  &                                                           
	      dexp(Xbest(Nparm)),int(CV*100.),                          &                                                           
	      dexp(Xhigh(Nparm)-1.96*CV),                               &                                                           
	      dexp(Xhigh(Nparm)+1.96*CV),                               &                                                           
	      dexp(Xhigh(Nparm)-CV),dexp(Xhigh(Nparm)+CV),              &                                                           
	      dexp(Xhigh(Nparm)+CV*CV/2.)                                                                                           
	      Nparm = Nparm+1                                                                                                              
	      CV= Xhigh(nparm) - Xbest(nparm)                                                                                              
	      write(fch,8000)Nparm,age,' b ',                                  &                                                           
	      dexp(Xbest(Nparm)),int(CV*100.),                          &                                                           
	      dexp(Xhigh(Nparm)-1.96*CV),                               &                                                           
	      dexp(Xhigh(Nparm)+1.96*CV),                               &                                                           
	      dexp(Xhigh(Nparm)-CV),dexp(Xhigh(Nparm)+CV),              &                                                           
	      dexp(Xhigh(Nparm)+CV*CV/2.)                                                                                           
	    endif                                                                                                                          
																    
																    
																    
8011  format(' ',A79)                                                                                                               
9000  format(5X,30(4X,I4))      ! Note output file up to 245 chars wide                                                             
9010  format(I2, 3X,30(F8.0))                                                                                                       
9020  format(I2, 3X,30(F8.4))                                                                                                       
																    
9030  format(3X,I2,3X,I4,1X,3X,F7.4,1X,I3,5(3X,F7.4))                                                                               
9050  format(8X,I4,4X,F7.4,4X,A35)                                                                                                  
																    
9035  format(I2,3X,A4,3X,3(E16.7,1X))                                                                                               
9040  format(I2,3X,I2,3X,3(F15.0,2X))                                                                                               
																    
9100  format(I2, 3X, 30(E10.3))                                                                                                     
9110  format(5X, 30(6X,I4))                                                                                                         
																    
      return                                                                                                                        
      end                                                                                                                           
																    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                      
																    
																    
! ///////////////////////////////////////////////////////////////////                                                               
																    
	Subroutine ItemList( NParm, OutItem, ExpOutput)                                                                                    
																    
! //////////////////////////////////////////////////////////////////                                                                
																	   
	Include "INDAT.INC"                                                                                                                
	Include "SEPMODEL.INC"                                                                                                             
																    
	double precision OutItem(7), CV                                                                                                    
	integer Nparm, i                                                                                                                   
	Logical ExpOutput                                                                                                                  
																    
																	   
	CV= (Xhigh(Nparm)-Xbest(Nparm))                                                                                                    
	OutItem(1) = dexp(Xbest(Nparm))                                                                                                    
	OutItem(2) =  CV*100.                                                                                                              
	OutItem(3) = dexp( Xbest(Nparm) - 1.96*CV)                                                                                         
	OutItem(4) = dexp( Xbest(Nparm) + 1.96*CV)                                                                                         
	OutItem(5) = dexp( Xbest(Nparm) - CV)                                                                                              
	OutItem(6) = dexp( Xbest(Nparm) + CV)                                                                                              
	OutItem(7) = dexp( Xbest(Nparm) + CV*CV/2d0)                                                                                       
																    
	ExpOutput= .False.                                                                                                                 
																    
	do i = 1,7                                                                                                                         
	  If (OutItem(i) .ge. 999999999.) ExpOutput = .True.                                                                               
	enddo                                                                                                                              
																    
	return                                                                                                                      
	end ! of subroutine ItemList                                                                                                       
																    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
																    
																    
! //////////////////////////////////////////////////////////////////////                                                            
																    
      Subroutine WriteResidualTable(fch,pwidth,cwidth)                                                                              
																    
! ///////////////////////////////////////////////////////////////////////                                                           
																    
      include "INDAT.INC"                                                                                                           
      include "LABELS.INC"                                                                                                          
      include "PREDIC.INC"                                                                                                          
      include "SEPMODEL.INC"                                                                                                        
      include "MESSAGE1.INC"                                                                                                        
																    
      integer fch,pwidth, cwidth, year, iyear, syear, iage                                                                          
      integer tlen                                                                                                                  
      character*77 text                                                                                                             
      character*3 ast                                                                                                               
																    
																    
																    
																    
      double precision data(maxyear,maxage),autoscale                                                                               
      character*50 Title, Underscore                                                                                                
																    
																    
      Underscore = '-------------------------------------------------'                                                              
      autoscale = 0d0 ! no scaling of residuals                                                                                     
																    
      ast= HP(77,Language)                                                                                                          
																    
      write(fch,*)                                                                                                                  
      write(fch,*) HP(67,Language)                                                                                                  
      write(fch,*) Underscore(1:Tlen(HP(67,Language)))                                                                              
      write(fch,*)                                                                                                                  
																	   
      Title=HP(76,Language)                                                                                                         
      do year= lastyear-NySep+1, lastyear                                                                                           
	iyear=year-firstyear+1                                                                                                             
	syear=year-(lastyear-NySep+1)+1                                                                                                    
	do iage=1, lastage-firstage                                                                                                        
	  If (CN(iyear,iage) .gt. 0d0) then                                                                                                
	    Data(syear,iage)=dlog(CN(iyear,iage)/PredCN(iyear,iage))                                                                       
	  else                                                                                                                             
	    Data(syear,iage)=MISSING                                                                                                       
	  endif                                                                                                                            
	enddo                                                                                                                              
      enddo                                                                                                                         
																    
      iyear=lastyear-NySep+1                                                                                                        
      iage=lastage-1                                                                                                                
      Call WriteTable(Title,maxyear,maxage,data,                        &                                                           
       iyear,lastyear,firstage,iage,cwidth, pwidth,                     &                                                           
       fch,ast,missing,0d0)                                                                                                         
																    
      if (nssbix .gt. 0) then                                                                                                       
	write(fch,*) HP(68,Language)                                                                                                       
	write(fch,*) Underscore(1:Tlen(HP(68,Language)))                                                                                   
	write(fch,*)                                                                                                                       
																    
	do index = 1, nssbix                                                                                                               
	  do iyear=1,lbyear-fbyear+1                                                                                                       
	    if (Bindex(index,iyear) .ne. MISSING) then                                                                                     
	      Data(iyear,1)=Bindex(index,iyear)-                               &                                                           
			       PredBindex(index,iyear)                                                                              
	    else                                                                                                                           
	      Data(iyear,1)=MISSING                                                                                                        
	    endif                                                                                                                          
	  enddo                                                                                                                            
	  Title = BSurvLab(index)                                                                                                          
																    
	  Call WriteTable(BSurvLab(index),maxyear,maxage,                      &                                                           
	  data,fbyear,                                                  &                                                           
	  lbyear,1,1,cwidth, pwidth, fch,'   ',missing,autoscale)                                                                   
	enddo ! SSB indices                                                                                                                
      endif ! any SSB indices                                                                                                       
																    
																    
! ----------------- Write the table of age-structured indices                                                                       
      if (Nageix .gt. 0) then                                                                                                       
	 write(fch,*) HP(69,Language)                                                                                                      
	 write(fch,*) Underscore(1:Tlen(HP(69,Language)))                                                                                  
	 write(fch,*)                                                                                                                      
      endif                                                                                                                         
																    
	 do index = 1, nageix                                                                                                              
	   Title =  ASurvLab(index)                                                                                                        
	   do iage=1,lage(index)-fage(index)+1                                                                                             
	     do iyear=1,lyear(index)-fyear(index)+1                                                                                        
	       If (Aindex(index,iyear,iage).ne.MISSING) then                                                                               
		 Data(iyear,iage)=Aindex(index,iyear,iage)                            &                                                           
		 -PredAindex(index,iyear,iage)                                                                                      
	       else                                                                                                                        
		 Data(iyear,iage)=MISSING                                                                                                         
	       endif                                                                                                                       
	     enddo                                                                                                                         
	   enddo                                                                                                                           
																    
	   Call WriteTable(Title,maxyear,maxage,data,fyear(index),             &                                                           
	     lyear(index),fage(index),lage(index),cwidth, pwidth,       &                                                           
	     fch,ast,missing,autoscale)                                                                                             
	 enddo ! age-structured indices                                                                                                    
																    
																    
																    
      return                                                                                                                        
      end ! of procedure WriteResidualTable                                                                                         
																    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
																    
																    
! ///////////////////////////////////////////////////////////////////////                                                           
																    
      Subroutine WriteFNR                                                                                                           
																    
! ///////////////////////////////////////////////////////////////////////                                                           
!                                                                                                                                   
!     Generates the Lowestoft-format output and survey resids etc.                                                                  
!                                                                                                                                   
!                                                                                                                                   
      Include "INDAT.INC"                                                                                                           
      Include "SEPMODEL.INC"                                                                                                        
      Include "STATS.INC"                                                                                                           
      Include "SRR.INC"                                                                                                             
      Include "LABELS.INC"                                                                                                          
      Include "PREDIC.INC"                                                                                                          
																    
      integer iyear, iage,  year                                                                                                    
      character*132 Title                                                                                                           
      double precision Data(maxyear, maxage)                                                                                        
      double precision SData(maxsurvey, maxyear, maxage)                                                                            
      double precision Bdata(maxBsurv,maxyear), autoscale                                                                           
																    
      autoscale = 100d0                                                                                                             
      Title = MainTitle//                                               &                                                           
       ': ICA v1.4 ESTIMATED FISHING MORTALITY '                                                                                    
      Call WriteLSTF(maxyear,maxage,firstyear,lastyear,firstage,        &                                                           
       lastage,Title, F,ica_f,99)                                                                                                   
																    
      Title = MainTitle//                                               &                                                           
       ': ICA v1.4 ESTIMATED POPULATION ABUNDANCE '                                                                                 
      iyear = lastyear+1                                                                                                            
      Call WriteLSTF(maxyear,maxage,firstyear,iyear,firstage,lastage,   &                                                           
       Title, N,ica_n,99)                                                                                                           
																    
      Title = MainTitle//                                               &                                                           
       ': ICA v1.4 PREDICTED CATCHES IN NUMBER (Thousands)'                                                                         
      do year = lastyear-NySep+1, lastyear                                                                                          
	iyear = year-(lastyear-NySep+1)+1                                                                                                  
	do iage = 1,lastage-firstage+1                                                                                                     
	  Data(iyear,iage)=PredCN(year-firstyear+1,iage)                                                                                   
	enddo                                                                                                                              
      enddo                                                                                                                         
      iyear = lastyear-NYsep+1                                                                                                      
      Call WriteLSTF(maxyear,maxage,iyear,lastyear,firstage,lastage,    &                                                           
       Title, Data,icacn_prd,99)                                                                                                    
																    
      Title = MainTitle//                                               &                                                           
       ': ICA v1.4 CATCH AT AGE RESIDUALS '                                                                                         
      do year = lastyear-NySep+1, lastyear                                                                                          
	iyear = year-(lastyear-NySep+1)+1                                                                                                  
	do iage = 1,lastage-firstage                                                                                                       
	  Data(iyear,iage)=dlog(CN(year-firstyear+1,iage))-                    &                                                           
		 dlog(PredCN(year-firstyear+1,iage))                                                                                
	enddo                                                                                                                              
      enddo                                                                                                                         
																    
																    
      iyear = lastyear-NYsep+1                                                                                                      
      Call WriteLSTF(maxyear,maxage,iyear,lastyear,firstage,lastage,    &                                                           
       Title, Data,icacn_res,99)                                                                                                    
																    
																    
      Title = MainTitle//                                               &                                                           
       ': ICA v1.4 ABUNDANCE INDEX RESIDUALS'                                                                                       
      do index = 1, Nageix                                                                                                          
	do iyear = 1, lyear(index)-fyear(index)+1                                                                                          
	  do iage = 1,lage(index)-fage(index)+1                                                                                            
	    If(Aindex(index,iyear,iage) .ne. MISSING) then                                                                                 
	      SData(index,iyear,iage)=Aindex(index,iyear,iage)-                &                                                           
		PredAindex(index,iyear,iage)                                                                                        
	    else                                                                                                                           
	      Sdata(index,iyear,iage)= 99.99d0                                                                                             
	    endif                                                                                                                          
	  enddo !ages                                                                                                                      
	enddo ! years                                                                                                                      
      enddo   ! indices                                                                                                             
																    
      Call WriteSurv(Title,Sdata,ICAASURV_RES)                                                                                      
																    
      Title = MainTitle//                                               &                                                           
       ': ICA v1.4 PREDICTED ABUNDANCE INDEX VALUES'                                                                                
      do index = 1, Nageix                                                                                                          
	do iyear = 1, lyear(index)-fyear(index)+1                                                                                          
	  do iage = 1,lage(index)-fage(index)+1                                                                                            
	    If(Aindex(index,iyear,iage) .ne. MISSING) then                                                                                 
	      SData(index,iyear,iage)=                                         &                                                           
		 dexp(PredAindex(index,iyear,iage))                                                                                 
	    else                                                                                                                           
	      Sdata(index,iyear,iage)= Missing                                                                                             
	    endif                                                                                                                          
	  enddo !ages                                                                                                                      
	enddo ! years                                                                                                                      
      enddo   ! indices                                                                                                             
																    
      Call WriteSurv(Title,SData,ICAASURV_PRD)                                                                                      
																    
																    
      If (nssbix .gt. 0) then ! write RCT files                                                                                     
																    
	Title = MainTitle//                                                    &                                                           
	 ': ICA v1.4 PREDICTED SSB INDEX VALUES'                                                                                    
																    
	do index=1,Nssbix                                                                                                                  
	  do iyear = 1, lbyear-fbyear+1                                                                                                    
	    if (Bindex(index,iyear) .ne. MISSING) then                                                                                     
	      Bdata(index,iyear) = dexp(PredBindex(index,iyear))                                                                           
	    else                                                                                                                           
	      Bdata(index,iyear) = MISSING                                                                                                 
	    endif                                                                                                                          
	  enddo                                                                                                                            
	enddo                                                                                                                              
																    
	CAll WriteRCT(Title, Bdata,ICABSURV_PRD)                                                                                           
																    
	Title = MainTitle//                                                    &                                                           
	 ': ICA v1.4 SSB INDEX RESIDUALS'                                                                                           
																    
	do index=1,Nssbix                                                                                                                  
	  do iyear = 1, lbyear-fbyear+1                                                                                                    
	    if (Bindex(index,iyear) .ne. MISSING) then                                                                                     
	      Bdata(index,iyear) =                                             &                                                           
		 Bindex(index,iyear)-PredBindex(index,iyear)                                                                        
	    else                                                                                                                           
	      Bdata(index,iyear) = MISSING                                                                                                 
	    endif                                                                                                                          
	  enddo                                                                                                                            
	enddo                                                                                                                              
																    
	CAll WriteRCT(Title, Bdata,ICABSURV_RES)                                                                                           
																    
      endif ! any SSB surveys                                                                                                       
																    
      return                                                                                                                        
      end                                                                                                                           
																    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
																    
																    
! ///////////////////////////////////////////////////////////////////////                                                           
																    
      Subroutine WriteLSTF(maxyear,maxage,firstyear,lastyear,firstage,  &                                                           
       lastage, Title, data, filename, filenum)                                                                                     
																    
! ///////////////////////////////////////////////////////////////////////                                                           
!     write out data in Lowestoft flat ascii format                                                                                 
																    
      integer firstyear, lastyear, firstage, lastage, maxyear, maxage                                                               
      double precision Data(maxyear, maxage)                                                                                        
      integer fch, filenum, iyear, iage                                                                                             
      character* (*) Title                                                                                                          
      character* (*) filename                                                                                                       
																    
      if ((lastyear-firstyear+1.gt. maxyear).or.                        &                                                           
	 (lastage-firstage+1 .gt. maxage)) then                                                                                     
	 write(*,*) 'WRITELSTF: Array addressing error '                                                                                   
	 stop                                                                                                                              
      endif                                                                                                                         
      if ((lastage-firstage+1) .gt. 100)                                &                                                           
	  write(*,*)                                                    &                                                           
	 'WRITELSTF: Age range exceeds data format. ',                  &                                                           
	 'Output will be wrapped.'                                                                                                  
																    
																    
																    
      fch = 17                                                                                                                      
      open(fch, file=filename, status='unknown')                                                                                    
      write(fch, 100) Title                                                                                                         
      write(fch, 200) 1, filenum                                                                                                    
      write(fch, 200) firstyear, lastyear                                                                                           
      write(fch, 200) firstage, lastage                                                                                             
      write(fch, 200) 1                                                                                                             
      do iyear = 1, lastyear-firstyear+1                                                                                            
	write(fch, 300) (Data(iyear,iage), iage=1, lastage-firstage+1)                                                                     
      enddo                                                                                                                         
																    
      close(fch)                                                                                                                    
      return                                                                                                                        
																    
100   format (' ',A132)                                                                                                             
200   format (' ',I4,4X,I4)                                                                                                         
300   format (' ',100(1X,G11.5))                                                                                                    
																    
      end    ! of routine WriteLSTF                                                                                                 
																    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                      
																    
																    
																    
!                                                                                                                                   
! ///////////////////////////////////////////////////////////////////////                                                           
																    
      Subroutine WriteSurv(Title, data, filename)                                                                                   
																    
! ///////////////////////////////////////////////////////////////////////                                                           
!     write out data in Stuart's "fleet" format                                                                                     
																    
       Include "INDAT.INC"                                                                                                          
       Include "LABELS.INC"                                                                                                         
       double precision data(maxsurvey,maxyear,maxage)                                                                              
       character* (*) filename, Title                                                                                               
       integer fch, iyear, iage, index                                                                                              
																    
       fch = 17                                                                                                                     
!       i = Nageix + 100                                                                                                            
       open(fch, file=filename, status='unknown')                                                                                   
       write(fch,100) Title                                                                                                         
       write(fch,200) Nageix, 0                                                                                                     
       do index = 1, Nageix                                                                                                         
	 write(fch,100) ASurvLab(index)                                                                                                    
	 write(fch,200) fyear(index), lyear(index)                                                                                         
	 write(fch,250) 1,1,Timing(index),Timing(index)                                                                                    
	 write(fch,200) fage(index), lage(index)                                                                                           
	 do iyear=1,lyear(index)-fyear(index)+1                                                                                            
	    write(fch,300) (Data(index,iyear,iage),                            &                                                           
		 iage=1,lage(index)-fage(index)+1)                                                                                  
	 enddo ! years                                                                                                                     
       enddo   ! fleets                                                                                                             
      close(fch)                                                                                                                    
																    
      return                                                                                                                        
																    
100   format (' ',A)                                                                                                                
200   format (' ',I4,4X,I4)                                                                                                         
250   format (' ',I4,4X,I4,4X,G11.5, G11.5)                                                                                         
300   format (' ',100(1X,G11.5))                                                                                                    
																    
      end ! of routine WriteSurv                                                                                                    
																    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
																    
																    
																    
! ////////////////////////////////////////////////////////////////////////                                                          
																    
      Subroutine WriteRCT(Title, Bdata,filename)                                                                                    
																    
! /////////////////////////////////////////////////////////////////////////                                                         
																    
      include "INDAT.INC"                                                                                                           
      include "LABELS.INC"                                                                                                          
																    
																    
      integer  fch, year, index                                                                                                     
      character* (*) Title, filename                                                                                                
      double precision Bdata(maxbsurv,maxyear), CAlcSSB,SSB                                                                         
																    
      fch = 17                                                                                                                      
      open(fch, file=filename, status='unknown')                                                                                    
																    
      write(fch,100) Title                                                                                                          
      write(fch,200) 1, lbyear-fbyear+1, nssbix                                                                                     
      write(fch,300)'Year','VPA',(BSurvLab(index), index=1,nssbix)                                                                  
      do year = fbyear,lbyear                                                                                                       
	SSB=CalcSSB(year)                                                                                                                  
	write(fch,400) year,SSB,                                               &                                                           
	  (Bdata(index,year-fbyear+1),                                  &                                                           
	  index=1,Nssbix)                                                                                                           
      enddo ! years                                                                                                                 
																    
      close(17)                                                                                                                     
																    
100   format(' ',A)                                                                                                                 
200   format(' ',3(I4,4X))                                                                                                          
300   format(' ',10( '''',A20,'''',1X))                                                                                             
400   format(' ',I4,4X,G11.5,1X,20(G11.5,1X))                                                                                       
																    
																    
      end ! of routine WriteRCT                                                                                                     
																    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
																    
																    
! ///////////////////////////////////////////////////////////////////////                                                           
																    
      Integer Function Tlen(string)                                                                                                 
																    
! //////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
!     Returns length of a string, not counting trailing blanks                                                                      
!                                                                                                                                   
      character*(*) string                                                                                                          
      integer i                                                                                                                     
																    
      i = len(string)                                                                                                               
      do while (string(i:i) .eq. ' ')                                                                                               
	i=i-1                                                                                                                              
      enddo                                                                                                                         
      i=i+1                                                                                                                         
      Tlen = i                                                                                                                      
      return                                                                                                                        
      end                                                                                                                           
																    
																    
																    
																    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
       Subroutine DocModel(i) ! documents model fit summary to screen                                                               
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      implicit none                                                                                                                 
      Include "INDAT.INC"                                                                                                           
      Include "SEPMODEL.INC"                                                                                                        
      Include "STATS.INC"                                                                                                           
      Include "SRR.INC"                                                                                                             
      Include "PREDIC.INC"                                                                                                          
      Include "MESSAGE1.INC"                                                                                                        
      character*77 Text(15)                                                                                                         
      character*5 ytext                                                                                                             
                                                                                                                                    
                                                                                                                                    
      integer i ! the output channel if +ve, otherwise writes to screen                                                             
      integer j                                                                                                                     
                                                                                                                                    
      Text(1)= '----------------------------------------------------'   &                                                           
      //'-------------'                                                                                                             
                                                                                                                                    
      Call IntToChar(Nysep,ytext(1:3),3)                                                                                            
      Call ConCat(Text(2),HL(21,Language),ytext(1:3))                                                                               
                                                                                                                                    
      Call IntToChar(firstage,ytext,5)                                                                                              
      Call ConCat(Text(3),HL(22,Language),ytext)                                                                                    
      Call Concat(Text(3),Text(3),' . . .')                                                                                         
      Call IntToChar(lastage,ytext,5)                                                                                               
      Call Concat(Text(3),Text(3),ytext)                                                                                            
                                                                                                                                    
      Call IntToChar(firstyear,ytext,5)                                                                                             
      Call ConCat(Text(4),HL(23,Language),ytext)                                                                                    
      Call Concat(Text(4),Text(4),' . . .')                                                                                         
      Call IntToChar(lastyear,ytext,5)                                                                                              
      Call Concat(Text(4),Text(4),ytext)                                                                                            
                                                                                                                                    
      Call IntToChar(Nssbix,ytext(1:3),3)                                                                                           
      Call ConCat(Text(5),HL(24,Language),ytext(1:3))                                                                               
                                                                                                                                    
      Call IntToChar(Nageix,ytext(1:3),3)                                                                                           
      Call ConCat(Text(6),HL(25,Language),ytext(1:3))                                                                               
                                                                                                                                    
      if (FitSRR) then                                                                                                              
        Text(7)=HL(26,Language)                                                                                                     
      else                                                                                                                          
        Text(7)='  '                                                                                                                
      endif                                                                                                                         
                                                                                                                                    
      Call IntToChar(Nxparm,ytext(1:3),3)                                                                                           
      Call ConCat(Text(8),HL(27,Language),ytext(1:3))                                                                               
                                                                                                                                    
      Call IntToChar(Nxdata,ytext,5)                                                                                                
      Call ConCat(Text(9),HL(28,Language),ytext)                                                                                    
      Text(10) = ' '                                                                                                                
                                                                                                                                    
      If (TwoSel) then                                                                                                              
        Text(11)= HL(29,Language)                                                                                                   
        Call IntToChar(ChangeSel,ytext,5)                                                                                           
        Call Concat(Text(12),HL(32,Language),ytext)                                                                                 
        If (StepSel) then                                                                                                           
          Text(13)=HL(30,Language)                                                                                                  
        else                                                                                                                        
          Text(13)=HL(31,Language)                                                                                                  
        endif                                                                                                                       
      else                                                                                                                          
        Text(11)= HL(33,Language)                                                                                                   
        Text(12)= ' '                                                                                                               
        Text(13)=Text(12)                                                                                                           
      endif                                                                                                                         
      Text(14)= ' '                                                                                                                 
      Text(15)=Text(1)                                                                                                              
      if (i .le. 0) then                                                                                                            
        Call Screen_out_a(Text,15,15)                                                                                               
      else                                                                                                                          
        do j =1,15                                                                                                                  
          write(i,100) Text(j)                                                                                                      
        enddo                                                                                                                       
      endif                                                                                                                         
100   format (' ', A)                                                                                                               
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                      


! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      Subroutine WriteANOV(fch)                                                                                                     
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
! ------------------ Write the analysis of variance table to file                                                                   
                                                                                                                                    
      include "INDAT.INC"                                                                                                           
      include "SEPMODEL.INC"                                                                                                        
      include "STATS.INC"                                                                                                           
      include "LABELS.INC"                                                                                                          
      include "SRR.INC"                                                                                                             
      include "MESSAGE1.INC"                                                                                                        
                                                                                                                                    
      integer fch, sumdf, sumparm, sumdata, table, iage                                                                             
      double precision sumvar, sumssq, SSQ                                                                                          
      character*80 LineText                                                                                                         
                                                                                                                                    
      write(fch,100) HM(32,Language)                                                                                                
      write(fch,101) '----------------------------'                                                                                 
                                                                                                                                    
101   format(A26)                                                                                                                   
                                                                                                                                    
                                                                                                                                    
      do Table  =1, 2                                                                                                               
                                                                                                                                    
      if (Table .eq. 1) then                                                                                                        
        Call CalcStats(.false.)                                                                                                     
        write(fch,*)                                                                                                                
        write(fch,*) HM(34,Language)                                                                                                
        write(fch,*)                                                                                                                
      else                                                                                                                          
        Call CalcStats(.true.)                                                                                                      
        write(fch,*)                                                                                                                
        write(fch,*) HM(35,Language)                                                                                                
        write(fch,*)                                                                                                                
      endif                                                                                                                         
                                                                                                                                    
      SSQ = CVar*dble(dfsep)                                                                                                        
                                                                                                                                    
      do index = 1,Nssbix                                                                                                           
        SSQ = SSQ + Bvar(index)* dble(NoBdata(index)-QBParm(index))                                                                 
      enddo                                                                                                                         
      do index = 1, Nageix                                                                                                          
        do iage=1,lage(index)-fage(index)+1                                                                                         
          SSQ=SSQ+Avar(index,iage)*                                     &                                                           
                dble(NoAdata(index,iage)-QAParm(index))                                                                             
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
      IF (FitSRR) SSQ = SSQ + SRRVar*(NoSRRdata-2)                                                                                  
                                                                                                                                    
                                                                                                                                    
      Linetext= ' '                                                                                                                 
      write(fch,*) LineText                                                                                                         
      write(fch,'(A38)') HM(40,Language)                                                                                            
      write(fch,102) ' ',HM(36,Language),HM(37,Language),               &                                                           
       HM(38,Language),HM(39,Language),HM(40,Language)                                                                              
102   format(A38,1X,A7,1X,A7,1X,A10,1X,A4,1X,A8)                                                                                    
103   format(A38,F8.4,1X,I7,1X,I10,1X,I4,1X,F8.4)                                                                                   
                                                                                                                                    
                                                                                                                                    
      write(fch,103) HM(41,Language),SSQ,nxdata,nxparm,                 &                                                           
       nxdata-nxparm,SSQ/dble(nxdata-nxparm)                                                                                        
                                                                                                                                    
      write(fch,103) HM(42,Language),CVar*dfSep,NoCdata,                &                                                           
       NoCdata-dfsep,dfsep,CVar                                                                                                     
                                                                                                                                    
      if (Nssbix .gt. 0) then                                                                                                       
        write(fch,*)' '                                                                                                             
        write(fch,103) HM(43,Language)                                                                                              
      endif                                                                                                                         
      do index=1,nssbix                                                                                                             
        write(fch,103) BSurvLab(index),BVar(index)*                     &                                                           
         (NoBdata(index)-QBParm(index)),NoBdata(index),QBparm(index),   &                                                           
         (NoBdata(index)-QBParm(index)),BVar(index)                                                                                 
      enddo                                                                                                                         
                                                                                                                                    
      if (Nageix .gt. 0) then                                                                                                       
        write(fch,*) ' '                                                                                                            
        write(fch,*) HM(44,Language)                                                                                                
      endif                                                                                                                         
                                                                                                                                    
      do index=1, nageix                                                                                                            
        sumssq=0d0                                                                                                                  
        sumdata=0                                                                                                                   
        sumparm= QAparm(index)*(lage(index)-fage(index)+1)                                                                          
        sumdf = 0                                                                                                                   
        sumvar = 0d0                                                                                                                
        do iage=1,lage(index)-fage(index)+1                                                                                         
          sumdata= sumdata+NoAdata(index,iage)                                                                                      
          sumssq = sumssq+Avar(index,iage)*(NoAdata(index,iage)-        &                                                           
                QAParm(index))                                                                                                      
          sumdf = sumdf+ (NoAdata(index,iage)-                          &                                                           
                QAParm(index))                                                                                                      
        enddo ! ages                                                                                                                
        write(fch,103) ASurvLab(index),sumssq,sumdata,sumparm,sumdf,    &                                                           
            sumssq/dble(sumdf)                                                                                                      
                                                                                                                                    
        write(fch,*)                                                                                                                
                                                                                                                                    
                                                                                                                                    
      enddo ! aged indices                                                                                                          
                                                                                                                                    
      if (FitSRR) then                                                                                                              
        write(fch,103) HM(45,Language),SRRVar*(NoSRRdata-2),NoSrrData,  &                                                           
        2,NoSrrData-2,SrrVar                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      enddo ! of the two tables, weighted and unweighted                                                                            
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      return                                                                                                                        
                                                                                                                                    
100   format(' ',A39,2X,F8.4,2X,I4,4X,I2,4X,I3,3X,F8.4)                                                                             
                                                                                                                                    
      end ! of routine WRITEANOV                                                                                                    
                                                                                                                                    
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                     
                                                                                                                                    

