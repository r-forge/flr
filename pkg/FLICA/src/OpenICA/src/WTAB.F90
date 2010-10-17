                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
       Subroutine WriteTable (Title, maxyear, maxage, Data,             &                                                           
          firstyear, lastyear,                                          &                                                           
          firstage, lastage, cwidth, pwidth, fch, ylabel,missing,       &                                                           
          inscale)                                                                                                                  
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
	implicit none                                                                                                                      
!          Example formats for column width = 7                                                                                     
!                                                                                                                                   
!          Value                   Printed as                                                                                       
!          1000000000.              1000.       Millions                                                                            
!             1000000.              1000.       Thousands                                                                           
!                1000.              1000.       Units                                                                               
!                  10.                10.       Units                                                                               
!                   0.1                0.1000   Units                                                                               
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
	 integer maxyear, maxage, startyear, endyear, i                                                                                    
	 double precision Data(Maxyear,Maxage), missing                                                                                    
	 integer firstage, lastage, firstyear, lastyear, year,age                                                                          
	 integer cwidth ! column width                                                                                                     
	 integer pwidth ! page width                                                                                                       
	 integer fch    ! file channel                                                                                                     
	 character*(*) Title  ! title for section                                                                                          
	 Character*500 Blank                                                                                                               
	 character*40 xForm, yForm, legend ! format code for printing                                                                      
         character*3 Ylabel                 ! label for the  'age' column                                                           
	 character*1 xc1, xc2, xc3, xc4, xc5, xc6, xc7, xc8                                                                                
	 integer notabs, subtables                                                                                                         
                                                                                                                                    
	 double precision maximum, minimum, maxvalue, scale,inscale                                                                        
	 integer iyear, iage, nocols, spaces                                                                                               
                                                                                                                                    
!        'SCALE' is used as a flag on entry. If set between -9 and +9 in                                                            
!        steps of 3 it is left alone and use to scale the tables.                                                                   
!        If outside that range, then automatic rescaling is invoked                                                                 
                                                                                                                                    
	 do i =1,500                                                                                                                       
	   Blank(i:i) = ' '                                                                                                                
	 enddo                                                                                                                             
                                                                                                                                    
	 scale = inscale                                                                                                                   
                                                                                                                                    
	 if ((pwidth .lt. 30) .or. (pwidth .ge.500)) then                                                                                  
	   write(*,*) 'Write Table: Page width error: ',pwidth                                                                             
	   write(*,*) 'out of range',30,' to ',500                                                                                         
	   stop                                                                                                                            
	 else if ((cwidth .lt.6) .or. (cwidth .gt. 25)) then                                                                               
	   write(*,*) 'Write Table: Column width error: ',cwidth                                                                           
	   write(*,*) 'out of range',6,' to ',25                                                                                           
	   stop                                                                                                                            
	 endif                                                                                                                             
                                                                                                                                    
!        Find the largest and smallest values in the data set                                                                       
	                                                                                                                                   
	 maximum= -1d23                                                                                                                    
	 minimum =1d23                                                                                                                     
                                                                                                                                    
	 do iyear=1, lastyear-firstyear+1                                                                                                  
	   do iage = 1, lastage-firstage+1                                                                                                 
                                                                                                                                    
	    if ((Data(iyear,iage).gt.maximum) .and.                            &                                                           
             (Data(iyear,iage) .ne. missing)) maximum=Data(iyear,iage)                                                              
	    if ((Data(iyear,iage).lt.minimum) .and.                            &                                                           
              (Data(iyear,iage) .ne. missing)) minimum=Data(iyear,iage)                                                             
	   enddo                                                                                                                           
	 enddo                                                                                                                             
                                                                                                                                    
	 if (minimum .lt. 0d0) then  ! need an extra leading space                                                                         
	   maxvalue = 10d0**dble((cwidth-4))-1d0                                                                                           
	 else                                                                                                                              
	   maxvalue = 10d0**dble((cwidth-3))-1d0                                                                                           
	 endif                                                                                                                             
	                                                                                                                                   
	 if (dabs(minimum) .gt. maximum) then                                                                                              
	   maximum =dabs(minimum)                                                                                                          
	   minimum =0d0                                                                                                                    
	 endif                                                                                                                             
                                                                                                                                    
                                                                                                                                    
	! maxvalue is maximum value that can be printed                                                                                    
	! with no scaling                                                                                                                  
	! one space, one decimal, extra space if needed for minus sign                                                                     
                                                                                                                                    
	if ((scale.eq.-6d0).or.(scale.eq.-3d0).or.(scale .eq.0d0)              &                                                           
         .or.(scale.eq.3d0).or.(scale .eq. 6d0).or.(scale .eq. 9d0)     &                                                           
         .or.(scale .eq.-9d0)) then                                                                                                 
	  ! do nothing                                                                                                                     
	 else ! automatic rescale                                                                                                          
	   scale = -6d0                                                                                                                    
           do while (maximum/(10d0**scale) .gt. (maxvalue))                                                                         
	     scale = scale + 3d0                                                                                                           
	   enddo                                                                                                                           
	 endif                                                                                                                             
                                                                                                                                    
                                                                                                                                    
!        if (maximum .lt. 0.001) scale = 9d0 ! this indicates out of scale , use exp notation                                       
	                                                                                                                                   
                                                                                                                                    
                                                                                                                                    
!       Re-code the missing values                                                                                                  
                                                                                                                                    
	 do iyear=1, lastyear-firstyear+1                                                                                                  
	   do iage = 1, lastage-firstage+1                                                                                                 
	     if (Data(iyear,iage) .eq. missing)                                &                                                           
                 Data(iyear,iage) = (maxvalue*10d0**scale*10d0)                                                                     
	   enddo                                                                                                                           
	 enddo                                                                                                                             
                                                                                                                                    
                                                                                                                                    
!   Find the remaining space in which decimals can be printed                                                                       
                                                                                                                                    
	 spaces = 0                                                                                                                        
         do while ((maxvalue .gt.                                       &                                                           
            (dabs(maximum)/10d0**scale)*10d0**(spaces-1)) .and.         &                                                           
            (spaces .le. 10))                                                                                                       
	   spaces = spaces+1                                                                                                               
	 enddo                                                                                                                             
	 spaces = spaces-2 ! to leave at least one space empty between numbers, and space for decimal                                      
	 if (spaces .gt. 9) spaces = 9    ! to avoid unnecessary detail                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!         write(*,*) Title,maximum,minimum,scale                                                                                    
                                                                                                                                    
                                                                                                                                    
	 nocols = (pwidth - cwidth-5+4)/cwidth                                                                                             
                                                                                                                                    
!         if (maximum .gt. 10d0) then                                                                                               
!          xc1 = CHAR(int(nocols/10) +48)                                                                                           
!          xc2 =CHAR(nocols-10*(int(nocols/10))+48)                                                                                 
!          xc3=CHAR(  int((cwidth-1)/10)  +48)                                                                                      
!          xc4=CHAR(48+cwidth-1- (10*int((cwidth-1)/10)))                                                                           
!          xc7=CHAR( int((cwidth-5)/10)  +48)                                                                                       
!          xc8=CHAR(48+cwidth-5- (10*int((cwidth-5)/10)))                                                                           
!          xc9=CHAR(48+spaces)                                                                                                      
!          xform = '(1X,I2,'//xc7//xc8//'X,''|'''//xc1//xc2//                                                                       
!     *             '(1X,F'//xc3//xc4//'.0))'                                                                                       
!         else                                                                                                                      
!          xc1 =  CHAR(int(nocols/10) +48)                                                                                          
!          xc2 = CHAR(  nocols - 10*(int(nocols/10)) +48)                                                                           
!          xc3=CHAR(  int((cwidth-1)/10)  +48)                                                                                      
!          xc4=CHAR( cwidth-1 - 10*int( (cwidth-1)/10)+48 )                                                                         
!          xc5=CHAR(  int((cwidth-1-2)/10)  +48)                                                                                    
!          xc6=CHAR( cwidth-1-2-10*int( (cwidth-1-2)/10)+48)                                                                        
!          xc7=CHAR( int((cwidth-5)/10)  +48)                                                                                       
!          xc8=CHAR(48+cwidth-5- (10*int((cwidth-5)/10)))                                                                           
!          xform='(1X,I2,'//xc7//xc8//'X,''|'''//xc1//xc2//'(1X,F'//xc3//                                                           
!     *     xc4//'.'//xc5//xc6//'))'                                                                                                
!         endif                                                                                                                     
	                                                                                                                                   
	xc1 =  CHAR(int(nocols/10) +48)                                                                                                    
	xc2 = CHAR(  nocols - 10*(int(nocols/10)) +48)                                                                                     
	xc3=CHAR(  int((cwidth-1)/10)  +48)                                                                                                
	xc4=CHAR( cwidth-1 - 10*int( (cwidth-1)/10)+48 )                                                                                   
	xc5=CHAR(  int((spaces)/10)  +48)                                                                                                  
	xc6=CHAR( spaces-10*int( (spaces)/10)+48)                                                                                          
	xc7=CHAR( int((cwidth-5)/10)  +48)                                                                                                 
	xc8=CHAR(48+cwidth-5- (10*int((cwidth-5)/10)))                                                                                     
	xform='(1X,I2,'//xc7//xc8//'X,''|'''//xc1//xc2//'(1X,F'//xc3//         &                                                           
         xc4//'.'//xc5//xc6//'))'                                                                                                   
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
	 if (scale .eq. 0.) then                                                                                                           
           legend = '   '  !  'Units'                                                                                               
	 else  if  (scale .eq. 3.) then                                                                                                    
           legend = 'x 10 ^ 3' !  'Thousands'                                                                                       
	 else if (scale .eq. 6.) then                                                                                                      
           legend = 'x 10 ^ 6' !  'Millions'                                                                                        
	 else if (scale .eq. 9.) then                                                                                                      
           legend = 'x 10 ^ 9' !  'Thousand Millions'                                                                               
	 else if (scale .gt. 9) then                                                                                                       
           legend = '   ' !  'Units'                                                                                                
	   scale = -1.                                                                                                                     
	 else if (scale .eq. -9.) then                                                                                                     
	   legend = 'x 10 ^ -9 '                                                                                                           
	 else if (scale .eq. -6.) then                                                                                                     
	   legend = 'x 10 ^ -6'                                                                                                            
	 else if (scale .eq. -3.)  then                                                                                                    
	   legend = 'x 10 ^ -3'                                                                                                            
	 endif                                                                                                                             
	                                                                                                                                   
	                                                                                                                                   
	                                                                                                                                   
                                                                                                                                    
107       if (scale .eq. -1) then             ! scaling failed, use exponential notation                                            
	  xc1 = CHAR(int(nocols/10) +48)                                                                                                   
	  xc2 =CHAR(nocols-10*(int(nocols/10))+48)                                                                                         
	  xc3=CHAR(  int((cwidth-1)/10)  +48)                                                                                              
	  xc4=CHAR(48+cwidth-1- (10*int((cwidth-1)/10)))                                                                                   
	  xc5=CHAR(  int((cwidth-1-2)/10)  +48)                                                                                            
	  xc6=CHAR(48+cwidth-1-2- (10*int((cwidth-1-2)/10)))                                                                               
	  xc7=CHAR( int((cwidth-5)/10)  +48)                                                                                               
	  xc8=CHAR(48+cwidth-5- (10*int((cwidth-4)/10)))                                                                                   
	  xform = '(1X,I2,'//xc7//xc8//'X,''|'''//xc1//xc2//'(1X,E'//          &                                                           
           xc3//xc4//'.1))'                                                                                                         
	 endif                                                                                                                             
	                                                                                                                                   
!        Format for the year labels at top                                                                                          
	  xc3=CHAR(  int((cwidth-5)/10)  +48)                                                                                              
	  xc4=CHAR(48+cwidth-5- (10*int((cwidth-5)/10)))                                                                                   
	  yform = '('''// ylabel // ''''//  xc3//xc4//'X,''|'''                &                                                           
           //xc1//xc2//'(1X,'//                                         &                                                           
           xc3//xc4//'X,I4))'                                                                                                       
                                                                                                                                    
!        Calculate number of sub-tables                                                                                             
                                                                                                                                    
	 notabs = (lastyear-firstyear+1)/nocols                                                                                            
                                                                                                                                    
	 if ( dble((lastyear-firstyear+1))/dble(nocols)                        &                                                           
             .gt.dble(notabs)) notabs = notabs+1                                                                                    
                                                                                                                                    
!         add in the formatting for the age on left side                                                                            
!         with column width = cwidth                                                                                                
!         as <two spaces>nn<variable space>                                                                                         
!         write(*,*) xform                                                                                                          
!         write(*,*) yform                                                                                                          
                                                                                                                                    
	 do subtables = 1, notabs                                                                                                          
	   startyear = firstyear+(subtables-1)*nocols                                                                                      
	   endyear = startyear+nocols-1                                                                                                    
	   if (endyear .gt. lastyear) endyear=lastyear                                                                                     
	                                                                                                                                   
	   Call WriteHeader(Title,pwidth, cwidth, fch)                                                                                     
	                                                                                                                                   
                                                                                                                                    
	   CAll LinePlus ((endyear-startyear+1), cwidth, fch)                                                                              
	   write(fch,yform) (year, year =startyear,endyear)                                                                                
	   CAll LinePlus ((endyear-startyear+1), cwidth, fch)                                                                              
                                                                                                                                    
	   do age= firstage,lastage                                                                                                        
!             if (scale .gt. 0d0) then                                                                                              
	       write (fch,xform)age,(DATA(year-firstyear+1,                    &                                                           
                age-firstage+1)/(10d0**scale),year=startyear,endyear)                                                               
!             else                                                                                                                  
!               write (fch,xform) age,(DATA(year-firstyear+1,                                                                       
!     *          age-firstage+1),year=startyear,endyear)                                                                            
!              endif                                                                                                                
                                                                                                                                    
	   enddo                                                                                                                           
	   CAll LinePlus ((endyear-startyear+1), cwidth,fch)                                                                               
                                                                                                                                    
	   write(fch,'(A)') Blank(1:cwidth-5+4)//legend                                                                                    
                                                                                                                                    
!           write (fch,*) xform, scale, maxvalue, maximum                                                                           
	                                                                                                                                   
	   write(fch,*)                                                                                                                    
	   write(fch,*)                                                                                                                    
	 enddo ! subtables                                                                                                                 
	 write (fch,*)                                                                                                                     
	 return                                                                                                                            
                                                                                                                                    
	 end                                                                                                                               
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                             
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      Subroutine WriteHeader(Title,pwidth, cwidth, fch)                                                                             
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      integer pwidth, cwidth, fch, i, length, l_trim                                                                                
      Character*(*) Title                                                                                                           
      Character*500 Text                                                                                                            
                                                                                                                                    
      do i=1,500                                                                                                                    
	Text(i:i)=' '                                                                                                                      
      enddo                                                                                                                         
                                                                                                                                    
      length = l_trim(Title)                                                                                                        
                                                                                                                                    
                                                                                                                                    
      do i=1,l_trim(Title)                                                                                                          
	Text(i+cwidth:i+cwidth)=Title(i:i)                                                                                                 
      enddo                                                                                                                         
                                                                                                                                    
      write(fch, '(A)') Text(1:cwidth+l_trim(Title))                                                                                
                                                                                                                                    
                                                                                                                                    
      do i=1,cwidth+l_trim(Title)                                                                                                   
       if (i .le. cwidth) then                                                                                                      
         Text(i:i)=' '                                                                                                              
       else                                                                                                                         
         Text(i:i)='-'                                                                                                              
       endif                                                                                                                        
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      write(fch, '(A)') Text(1:cwidth+l_trim(Title))                                                                                
                                                                                                                                    
      return                                                                                                                        
                                                                                                                                    
      end                                                                                                                           
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
	   Subroutine LinePlus( columns, cwidth, fch)                                                                                      
                                                                                                                                    
!////////////////////////////////////////////////////////////////////////                                                           
!          writes a horizontal line with + at column positions                                                                      
!                                                                                                                                   
!                                                                                                                                   
	   integer columns, cwidth, i, fch, width                                                                                          
	   character*500 Text                                                                                                              
                                                                                                                                    
	   width = cwidth-5+4 + (cwidth*columns)                                                                                           
                                                                                                                                    
	   do i=1,width                                                                                                                    
	     if (i .eq. cwidth-5+4) then                                                                                                   
	       Text(i:i) =  '+'                                                                                                            
	     else                                                                                                                          
	       Text(i:i)='-'                                                                                                               
	     endif                                                                                                                         
	   enddo                                                                                                                           
	   write(fch,'(A)') Text(1:width)                                                                                                  
                                                                                                                                    
	   return                                                                                                                          
	   end                                                                                                                             
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
       Integer Function L_TRIM(ltext)                                                                                               
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
	Character*(*) ltext                                                                                                                
	integer i                                                                                                                          
                                                                                                                                    
	i = LEN(ltext)                                                                                                                     
	do while ((i.ge.1) .and. (ltext(i:i).eq.' '))                                                                                      
	  i = i-1                                                                                                                          
	enddo                                                                                                                              
	L_TRIM=i                                                                                                                           
	return                                                                                                                             
	end                                                                                                                                
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                    
	                                                                                                                                   
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
       Subroutine WriteSen(filename)                                                                                                
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
!     Generates an input file for sensitivity analysis programme                                                                    
                                                                                                                                    
                                                                                                                                    
      Include "indat.inc"                                                                                                           
      Include "sepmodel.inc"                                                                                                        
                                                                                                                                    
      integer fch, i, iyear, age, Nparm, iage, iyear2,yy,year                                                                       
      character*6 xc, lastxc                                                                                                        
      double precision Sel, CV, mean, lastmean,meanREC, CVREC                                                                       
      character* (*) filename                                                                                                       
                                                                                                                                    
      fch = 17                                                                                                                      
      xc = ' '                                                                                                                      
      open(fch, file = filename, status='unknown')                                                                                  
                                                                                                                                    
      write(fch,*) ' Input to sensitivity analysis '                                                                                
      write(fch,100) firstage, lastage, lastyear, 3                                                                                 
      write(fch,100) 1,0,0                                                                                                          
                                                                                                                                    
!     Calculate Geom mean recruitment                                                                                               
                                                                                                                                    
      iyear = (lastyear-2)-firstyear+1                                                                                              
                                                                                                                                    
      do year = firstyear,lastyear                                                                                                  
	N(year-firstyear+1,1) = dlog(N(year-firstyear+1,1))                                                                                
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      iage =1                                                                                                                       
      CAll MeanCV(N, maxyear, maxage,                                   &                                                           
         iage,1, iyear, missing, meanREC, cvREC)                                                                                    
                                                                                                                                    
      do year = firstyear,lastyear                                                                                                  
	N(year-firstyear+1,1) = dexp(N(year-firstyear+1,1))                                                                                
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!     go to the end of the selection parameters                                                                                     
      Nparm = NySep+(lastage-firstage+1)-3                                                                                          
      If (TwoSel) Nparm = Nparm+(lastage-firstage+1)-3                                                                              
      do age = firstage, lastage                                                                                                    
	if (age .le. 9) then                                                                                                               
	  xc = ''''//' N'//CHAR(48+age)//''''                                                                                              
	else                                                                                                                               
	  xc = ''''//'N'//CHAR(48+ (age/10) )//CHAR(48+age-10*(age/10))        &                                                           
           //''''                                                                                                                   
	endif ! ages under 9                                                                                                               
	if (age .eq. firstage) then  ! RECRUITMENT                                                                                         
	  nparm = nparm+1                                                                                                                  
	  write(fch,200) xc,MeanREC, CVRec                                                                                                 
	else if (age .ne. lastage) then                                                                                                    
	  nparm = nparm+1                                                                                                                  
	  write(fch,200) xc,                                                   &                                                           
             dexp(Xbest(nparm)),(Xhigh(nparm)-Xbest(nparm))                                                                         
	else                                     ! PLUS GROUP                                                                              
	  write(fch,200)xc,N(lastyear-firstyear+1,lastage-firstage+1),         &                                                           
           (Xhigh(nparm)-Xbest(nparm))                                                                                              
	endif ! not plus-group                                                                                                             
      enddo                                                                                                                         
      Nparm = NySep                                                                                                                 
      If (TwoSel) Nparm = (lastage-firstage+1)-3 ! the Selections                                                                   
      do age = firstage,lastage                                                                                                     
	if (age .le. 9) then                                                                                                               
	  xc = ''''//'SH'//CHAR(48+age)//''''                                                                                              
	else                                                                                                                               
	  xc = ''''//'SH'//CHAR(48+ (age/10))//char(48+age-10*(age/10))        &                                                           
          //''''                                                                                                                    
	endif                                                                                                                              
	If (age .eq. Refage) then                                                                                                          
	   Sel = 1d0                                                                                                                       
	   CV = 0d0                                                                                                                        
	else if ((age .eq. lastage) .or. (age .eq. lastage-1)) then                                                                        
	   if (TwoSel) then                                                                                                                
	     Sel = TermS2                                                                                                                  
	     CV = 0d0                                                                                                                      
	   else                                                                                                                            
	     Sel = TermS                                                                                                                   
	     CV = 0d0                                                                                                                      
	   endif                                                                                                                           
	else                                                                                                                               
	  Nparm = Nparm+1                                                                                                                  
	  Sel = dexp(Xbest(nparm))                                                                                                         
	  CV = (Xhigh(nparm)-Xlow(nparm))                                                                                                  
	endif                                                                                                                              
	write(fch,200) xc,Sel,CV                                                                                                           
                                                                                                                                    
      enddo ! ages: Selection pattern                                                                                               
                                                                                                                                    
                                                                                                                                    
      iyear = (lastyear-2)-firstyear+1                                                                                              
      do age=firstage,lastage                                                                                                       
	if (age .le. 9) then                                                                                                               
	  xc = ''''//'WH'//CHAR(48+age)//''''                                                                                              
	else                                                                                                                               
	  xc = ''''//'WH'//CHAR(48+(age/10))//char(48+age-10*(age/10))         &                                                           
           //''''                                                                                                                   
	endif                                                                                                                              
	iage = age-firstage+1                                                                                                              
	iyear2=lastyear-firstyear+1                                                                                                        
	CAll MeanCV(CW, maxyear, maxage,                                       &                                                           
         iage,iyear,iyear2,missing,mean,cv)                                                                                         
	write(fch,200) xc,Mean,CV                                                                                                          
      enddo                                                                                                                         
      do age=firstage,lastage                                                                                                       
	if (age .le. 9) then                                                                                                               
	  xc = ''''//'WS'//CHAR(48+age)//''''                                                                                              
	else                                                                                                                               
	  xc = ''''//'WS'//CHAR(48+ (age/10))//char(48+age-10*(age/10))        &                                                           
           //''''                                                                                                                   
	endif                                                                                                                              
	iage=age-firstage+1                                                                                                                
	iyear2=lastyear-firstyear+1                                                                                                        
	CAll MeanCV(SW, maxyear, maxage,                                       &                                                           
         iage,iyear,iyear2,missing,mean,cv)                                                                                         
	write(fch,200) xc,Mean,CV                                                                                                          
      enddo                                                                                                                         
                                                                                                                                    
      do age=firstage,lastage                                                                                                       
	if (age .le. 9) then                                                                                                               
	  xc = ''''//' M'//CHAR(48+age)//''''                                                                                              
	else                                                                                                                               
	  xc = ''''//' M'//CHAR(48+ (age/10))//char(48+age-10*(age/10))        &                                                           
           //''''                                                                                                                   
	endif                                                                                                                              
	iage=age-firstage+1                                                                                                                
	iyear2=lastyear-firstyear+1                                                                                                        
                                                                                                                                    
	CAll MeanCV(NM, maxyear, maxage,                                       &                                                           
         iage,iyear,iyear2, missing, mean, cv)                                                                                      
	write(fch,200) xc,Mean,CV                                                                                                          
      enddo                                                                                                                         
      Mean = 0.5                                                                                                                    
      Lastmean = 0.5                                                                                                                
      do age=firstage,lastage                                                                                                       
	if (age .le. 9) then                                                                                                               
	  xc = ''''//'MT'//CHAR(48+age)//''''                                                                                              
	else                                                                                                                               
	  xc = ''''//'MT'//CHAR(48+ (age/10))//char(48+age-10*(age/10))        &                                                           
           //''''                                                                                                                   
	endif                                                                                                                              
	iage=age-firstage+1                                                                                                                
	iyear2=lastyear-firstyear+1                                                                                                        
                                                                                                                                    
	CAll MeanCV(MO, maxyear, maxage,                                       &                                                           
         iage,iyear,iyear2,missing,mean,cv)                                                                                         
	                                                                                                                                   
	 lastmean=mean                                                                                                                     
	 lastxc= xc                                                                                                                        
	 if ((mean .eq. 1d0) .or. (mean .eq. 0d0) .or.                         &                                                           
             (lastmean .eq. 1d0) .or. (lastmean .eq. 0d0)) then                                                                     
	     write(fch,200) lastxc,lastMean, 0d0                                                                                           
	 else                                                                                                                              
	    write(fch,200) lastxc,lastMean, 0.1d0                                                                                          
	 endif                                                                                                                             
      enddo                                                                                                                         
      write(fch,200) xc,Mean, 0d0                                                                                                   
                                                                                                                                    
      if (lastyear .lt. 2000) then                                                                                                  
	yy = lastyear-1900                                                                                                                 
      else                                                                                                                          
	yy = lastyear-2000                                                                                                                 
      endif                                                                                                                         
      xc=''''//' K'//CHAR(48+(yy/10))//char(48+yy-10*(yy/10))           &                                                           
           //''''                                                                                                                   
      write(fch,200) xc, 1d0,0.1d0                                                                                                  
      if (lastyear+1 .lt. 2000) then                                                                                                
	yy = lastyear+1-1900                                                                                                               
      else                                                                                                                          
	yy = lastyear+1-2000                                                                                                               
      endif                                                                                                                         
      xc=''''//' K'//CHAR(48+(yy/10))//char(48+yy-10*(yy/10))           &                                                           
           //''''                                                                                                                   
      write(fch,200) xc, 1d0,0.1d0                                                                                                  
      if (lastyear+2 .lt. 2000) then                                                                                                
	yy = lastyear+2-1900                                                                                                               
      else                                                                                                                          
	yy = lastyear+2-2000                                                                                                               
      endif                                                                                                                         
      xc=''''//' K'//CHAR(48+(yy/10))//char(48+yy-10*(yy/10))           &                                                           
           //''''                                                                                                                   
      write(fch,200) xc, 1d0,0.1d0
      write(fch,'(I4)') -1
                                                                                                                                    
100   format(' ', 4(I4,1X,',',1X) )                                                                                                 
200   format( ' ',A6, 3X,', ',E25.12,3X,', ',E25.12)                                                                                
                                                                                                                                    
                                                                                                                                    
      close(fch)                                                                                                                    
      return                                                                                                                        
      end ! of routine WriteSen                                                                                                     
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                         
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
      Subroutine MeanCV(Matrix, physical_cols, physical_rows,           &                                                           
       data_row, start, endval, missing, mean, cv)                                                                                  
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      integer physical_rows, physical_cols                                                                                          
      double precision Matrix(physical_cols,Physical_rows)                                                                          
      double precision mean, CV, missing                                                                                            
      integer data_row, start, endval, count, i                                                                                     
                                                                                                                                    
!     calculate the mean                                                                                                            
      Mean  = 0d0                                                                                                                   
      count =0                                                                                                                      
      if ((start .lt. 1) .or. (endval .gt. physical_cols)) then                                                                     
	write(*,*) 'MeanCV: Column error',start,endval                                                                                     
	stop                                                                                                                               
      endif                                                                                                                         
      if ((data_row .lt. 1) .or. (data_row.gt.physical_rows)) then                                                                  
	write(*,*) 'MeanCV : Row Error',data_row                                                                                           
	stop                                                                                                                               
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      do i = start, endval                                                                                                          
	if (Matrix(i,data_row) .ne. Missing) then                                                                                          
	  Mean = Mean+Matrix(i,data_row)                                                                                                   
	  count = count+1                                                                                                                  
	endif                                                                                                                              
      enddo                                                                                                                         
      Mean = Mean/dble(count)                                                                                                       
                                                                                                                                    
                                                                                                                                    
      CV = 0d0                                                                                                                      
      do i = start, endval                                                                                                          
       if (Matrix(i,data_row) .ne. Missing) then                                                                                    
	CV = CV + (Matrix(i,data_row)-Mean)*(Matrix(i,data_row)-Mean)                                                                      
       endif                                                                                                                        
      enddo                                                                                                                         
      if (count .le. 1) then                                                                                                        
	CV = MISSING                                                                                                                       
      else                                                                                                                          
       CV = CV/(count-1) ! variance                                                                                                 
       if (CV .gt. 0d0) then                                                                                                        
	 CV = DSQRT(CV)/Mean   ! CV                                                                                                        
       else                                                                                                                         
	 CV = 0d0                                                                                                                          
       endif                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
      return                                                                                                                        
      end ! of routine MeanCV                                                                                                       
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
                                                                                                                                    
      Subroutine WriteSummFile(filename)                                                                                            
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
      include "indat.inc"                                                                                                           
      include "sepmodel.inc"                                                                                                        
      include "labels.inc"                                                                                                          
                                                                                                                                    
      integer iyear, iage, age                                                                                                      
      character* (*) filename                                                                                                       
      character*100 xline                                                                                                           
                                                                                                                                    
      double precision CalcSSB, TSB, SSB, MeanF                                                                                     
                                                                                                                                    
      open(17, file=filename, status='unknown')                                                                                     
                                                                                                                                    
      xLine = 'Stock Summary '//MainTitle                                                                                           
      write(17,100) xLine                                                                                                           
      write(17,200) 12                                                                                                              
      write(17,300) 1,0,0   ! codes to indicate only human consumption category                                                     
      write(17,*) 'Year '                                                                                                           
      write(17,300) Firstyear, lastyear                                                                                             
      Write(17,400) firstage                                                                                                        
      write(17,300) firstage, 1000                                                                                                  
      write(17,*) 'SSB, (tonnes) '                                                                                                  
      write(17,300) 1                                                                                                               
      write(17,*) 'TSB, (tonnes) '                                                                                                  
      write(17,300) 1                                                                                                               
      write(17,*) 'Catch,  Total (tonnes) '                                                                                         
      write(17,300) 1                                                                                                               
      write(17,*) 'Catch,  H. Cons (tonnes) '                                                                                       
      write(17,300) 1                                                                                                               
      write(17,*) 'Not used '                                                                                                       
      write(17,300) 1                                                                                                               
      write(17,*) 'Not used '                                                                                                       
      write(17,300) 1                                                                                                               
      write(17,*) 'Mean F, Total '                                                                                                  
      write(17,300) LoFage, HiFage                                                                                                  
      write(17,*) 'Mean F, H. Cons. '                                                                                               
      write(17,300) LoFage, HiFage                                                                                                  
      write(17,*) 'Not used '                                                                                                       
      write(17,300) 0, 0                                                                                                            
      write(17,*) 'Not used '                                                                                                       
      write(17,300) 0, 0                                                                                                            
      do iyear = 1, lastyear-firstyear+1                                                                                            
	TSB = 0d0                                                                                                                          
	                                                                                                                                   
	do iage = 1, lastage-firstage+1                                                                                                    
	  TSB = TSB+N(iyear,iage)*SW(iyear,iage)                                                                                           
	enddo                                                                                                                              
                                                                                                                                    
                                                                                                                                    
	MeanF = 0d0                                                                                                                        
	do age = LoFage, HiFage                                                                                                            
	  MeanF = MeanF + F(iyear,age-firstage+1)                                                                                          
	enddo                                                                                                                              
	MeanF = MeanF/dble(HiFage-LoFage+1)                                                                                                
                                                                                                                                    
                                                                                                                                    
	SSB = CalcSSB(iyear+firstyear-1)                                                                                                   
	write(17,500) iyear+firstyear-1,                                       &                                                           
        N(iyear,1),SSB, TSB, LA(iyear), LA(iyear), 0d0,0d0,             &                                                           
        MeanF, MeanF, 0d0, 0d0                                                                                                      
      enddo ! years                                                                                                                 
      close (17)                                                                                                                    
                                                                                                                                    
100   format(' ',A)                                                                                                                 
200   format(' ',I4)                                                                                                                
300   format(' ',3(2X,I6))                                                                                                          
400   format(' ','Recruits, age ',I2, ' , ( thousands ) ')                                                                          
500   format(' ',1X,I6,2X,11(G12.4) )                                                                                               
      end

