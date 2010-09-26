                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
      Subroutine ReadBlock                                                                                                          
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
      ! Reads the values from the 'ICA.TMP' temporary file                                                                          
      ! into the three common blocks, ready to start the                                                                            
      ! minimisation.                                                                                                               
      !                                                                                                                             
      ! The corresponding WRITEBLK programme is in a separate unit,                                                                 
      ! named 'Writeblk'                                                                                                            
      !                                                                                                                             
                                                                                                                                    
      implicit none                                                                                                                 
      Include "Indat.inc"                                                                                                           
      Include "SepModel.inc"                                                                                                        
      Include "SRR.inc"                                                                                                             
      Include 'Message1.inc'
      character*77 line

!     ------------------------LOCAL VARIABLES--------------------------                                                             
      integer age, iage, fch, ioerr,  index, idummy, year,iyear,i                                                                   
      double precision rdummy
      character*77 Text(1)
!     ------------------------------------------------------------------                                                            
                                                                                                                                    
!     --------------------------EXECUTABLE CODE-------------------------                                                            
                                                                                                                                    
      fch = 14                                                                                                                      

      Open (UNIT=fch,STATUS='OLD',IOSTAT=ioerr,FILE=ICA_TMP)                                                                      
                                                                                                                                    
      if (ioerr .ne. 0) then 
        Text(1)=HW(58,1) ! error message in english if file not found
        Call Screen_out_a(Text,1,1)
        stop
      endif                                                                                                                         
                                                                                                                                    
      read(fch,7000)firstage,lastage,firstyear,lastyear,nageix,nssbix   &                                                           
       , PF, PM, NxParm                                                                                                             
      if (nxParm .gt. maxparm) then                                                                                                 
	write(*,*)'Too many parameters. Recompile with maxparm >',             &                                                           
           nxparm                                                                                                                   
	stop                                                                                                                               
      else if ( (lastage-firstage+1).gt. maxage) then                                                                               
	write(*,*) 'Too many ages. Recompile with maxage > ',                  &                                                           
         (lastage-firstage+1)                                                                                                       
	stop                                                                                                                               
      else if (( lastyear-firstyear+1) .gt. maxyear) then                                                                           
	write(*,*) 'Too many years. Recompile with maxyear > ',                &                                                           
       (lastyear-firstyear+1)                                                                                                       
	stop                                                                                                                               
      else if (nageix .gt. maxsurvey) then                                                                                          
	write(*,*) 'Too many aged surveys. Recompile with maxsurvey >'         &                                                           
       ,nageix                                                                                                                      
	stop                                                                                                                               
      else if (nssbix .gt. maxbsurv) then                                                                                           
	write(*,*) 'Too many SSB indices. Recompile with maxbsurv >'           &                                                           
        ,nssbix                                                                                                                     
	 stop                                                                                                                              
      endif                                                                                                                         
                                                                                                                                    
7000  format(6(I4,1X),2(E25.16,1X),I4)                                                                                              
                                                                                                                                    
!     INIDICES                                                                                                                      
                                                                                                                                    
      if (nssbix .gt. 0) then                                                                                                       
	read(fch,7010) fbyear, lbyear                                                                                                      
7010  format(2(I4,1X))                                                                                                              
	do year = fbyear,lbyear                                                                                                            
	  read(fch,7020) (Bindex(index,year-fbyear+1),                         &                                                           
               index =1,nssbix)                                                                                                     
	enddo ! years of the ssb indices                                                                                                   
      endif   ! any ssb indices                                                                                                     
7020  format(8(E25.16,1X))                                                                                                          
      do index = 1,nageix                                                                                                           
	read(fch,7030) fage(index),lage(index),fyear(index),                   &                                                           
                               lyear(index), Timing(index)                                                                          
      enddo                                                                                                                         
7030  format(4(I4,1X),1X,E25.16)                                                                                                    
                                                                                                                                    
      do index = 1,nageix                                                                                                           
	do year = fyear(index),lyear(index)                                                                                                
	  iyear = year-fyear(index)+1                                                                                                      
	  read(fch, 7040) (Aindex(index,iyear,age-fage(index)+1),              &                                                           
              age=fage(index),lage(index))                                                                                          
	enddo                                                                                                                              
      enddo                                                                                                                         
7040  format(15(E25.16,1X))                                                                                                         
                                                                                                                                    
!     AGE-STRUCTURED DATA                                                                                                           
                                                                                                                                    
      do year = firstyear,lastyear+1                                                                                                
	iyear = year-firstyear+1                                                                                                           
	do age = firstage, lastage                                                                                                         
	  iage = age-firstage+1                                                                                                            
	  read(fch,7045) CN(iyear,iage),SW(iyear,iage),MO(iyear,iage),         &                                                           
               NM(iyear,iage), N(iyear,iage), F(iyear,iage)                                                                         
	enddo                                                                                                                              
      enddo                                                                                                                         
7045  format (6(E25.16,1X))                                                                                                         
                                                                                                                                    
!     LANDINGS                                                                                                                      
                                                                                                                                    
      do year = firstyear, lastyear                                                                                                 
	read(fch, 7050) LA(year-firstyear+1)                                                                                               
      enddo                                                                                                                         
7050  format (E25.16)                                                                                                               
                                                                                                                                    
!     THE CONTROL BLOCK FOR THE MODEL FITTING                                                                                       
                                                                                                                                    
      read(fch, 7060) UseSep, Writeout, Full, TwoSel                                                                                
7060  format(4L3)                                                                                                                   
                                                                                                                                    
      read(fch, 7070) NySep, RefAge, NxData, NxParm, TermS,TermS2                                                                   
7070  format(4(I4,1X),2(E25.16,1X))                                                                                                 
                                                                                                                                    
      do i = 1,Nxparm                                                                                                               
	read(fch, 7080) Xbest(i), Xlow(i), Xhigh(i)                                                                                        
      enddo                                                                                                                         
7080  format(3(E25.16),1X)                                                                                                          
                                                                                                                                    
      do index = 1,nssbix                                                                                                           
	read(fch,7090) Blambda(index)                                                                                                      
      enddo                                                                                                                         
                                                                                                                                    
      do index = 1, nageix                                                                                                          
	read(fch, 7090) (Alambda(index,iage),iage=1,                           &                                                           
         lage(index)-fage(index)+1)                                                                                                 
      enddo                                                                                                                         
7090  format(13(E25.16,1X))                                                                                                         
                                                                                                                                    
      do age = firstage, lastage                                                                                                    
	iage = age-firstage+1                                                                                                              
	read(fch,7100) RelWt(iage)                                                                                                         
      enddo                                                                                                                         
7100  format(E25.16)                                                                                                                
                                                                                                                                    
      do age = firstage, lastage                                                                                                    
	iage = age-firstage+1                                                                                                              
	do year = firstyear, lastyear                                                                                                      
	  iyear = year-firstyear+1                                                                                                         
	  read(fch, 7110) W(iyear, iage)                                                                                                   
	enddo                                                                                                                              
      enddo                                                                                                                         
7110  format(E25.16)                                                                                                                
                                                                                                                                    
      do i = 1, MAX(nageix,nssbix) 
	if (i .le. nssbix) then                                                                                                            
	  read(fch, 7120) rdummy, rdummy, Plusgp(i), QAparm(i),                &                                                           
          QBParm(i)                                                                                                                 
	else                                                                                                                               
	  read(fch, 7120) rdummy, rdummy, Plusgp(i), QAParm(i),                &                                                           
          idummy                                                                                                                    
	endif                                                                                                                              
      enddo                                                                                                                         
7120  format(2(E25.16),L3,2(I2,1X))                                                                                                 
                                                                                                                                    
!     the Stock-Recruit parameters                                                                                                  
                                                                                                                                    
      read(fch,*) lag, FitSRR  
                                                                                                                                    
!      if (FitSRR) then                                                                                                             
!        read(fch, *) SRRLambda, SRRVar                                                                                             
!        read(fch, *) fryear, lag, a, b                                                                                             
!        do i = 1,20                                                                                                                
!          read(fch, *) OldStock(i), OldRecruit(i)                                                                                  
!        enddo                                                                                                                      
!      endif                                                                                                                        
                                                                                                                                    
!     Weights by year on the catches at age                                                                                         
                                                                                                                                    
      do iyear=1,lastyear-firstyear+1                                                                                               
	read(fch,*) YearWt(iyear)                                                                                                          
      enddo                                                                                                                         
                                                                                                                                    
!     First and last age for calculating reference F                                                                                
                                                                                                                                    
      read(fch,*) LoFage, HiFage

! language chosen in ICA

      read(fch,*) language
                                                                                                                                    
!8100  format (L3)                                                                                                                  
!8110  format (I4,1X,I2,1X,2E25.16)                                                                                                 
!8120  format (2E25.16)                                                                                                             
                                                                                                                                    
      close(fch)                                                                                                                    
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
