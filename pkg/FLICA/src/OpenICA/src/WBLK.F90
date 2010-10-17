! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
      Subroutine WriteBlock                                                                                                         
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////                                                               
                                                                                                                                    
      !                                                                                                                             
      ! Writes out the values in the three common blocks to a                                                                       
      ! temporary file named ICA_TMP. These are the                                                                                 
      ! starting values for the minimisation.                                                                                       
      !                                                                                                                             
                                                                                                                                    
                                                                                                                                    
      Include "indat.inc"                                                                                                           
      Include "sepmodel.inc"                                                                                                        
      Include "SRR.inc"
      include 'message1.inc'
                                                                                                                                    
!     ------------------------LOCAL VARIABLES--------------------------                                                             
      integer age, year, iage, iyear,  fch, ioerr, i, index                                                                         
!     ------------------------------------------------------------------                                                            
                                                                                                                                    
!     --------------------------EXECUTABLE CODE-------------------------                                                            
                                                                                                                                    
      fch = 14                                                                                                                      
                                                                                                                                    
      Open (UNIT=14,STATUS='UNKNOWN',IOSTAT=ioerr,FILE=ICA_TMP)                                                                     
                                                                                                                                    
      if (ioerr .ne. 0) then                                                                                                        
        if (ioerr .eq. 6422) then                                                                                                   
          write(*,*) 'Error writing temporary file. Disk is full.'                                                                  
        else                                                                                                                        
          write(*,*) 'Error in writing temporary file.'                                                                             
          write(*,*) 'Run-time error number ',ioerr                                                                                 
          write(*,*) 'See Microsoft FORTRAN Reference manual for the meaning of this error message.'
        endif                                                                                                                       
      endif                                                                                                                         
                                                                                                                                    
      write(fch,7000)firstage,lastage,firstyear,lastyear,nageix,nssbix  &                                                           
       , PF, PM, NxParm                                                                                                             
7000  format(6(I4,1X),2(E25.16,1X), I4)                                                                                             
                                                                                                                                    
!     INIDICES                                                                                                                      
                                                                                                                                    
      if (nssbix .gt. 0) then                                                                                                       
        write(fch,7010) fbyear, lbyear                                                                                              
7010  format(2(I4,1X))                                                                                                              
        do year = fbyear,lbyear                                                                                                     
          write(fch,7020) (Bindex(index,year-fbyear+1),                 &                                                           
               index =1,nssbix)                                                                                                     
        enddo ! years of the ssb indices                                                                                            
      endif   ! any ssb indices                                                                                                     
7020  format(50(E25.16,1X))                                                                                                         
      do index = 1,nageix                                                                                                           
        write(fch,7030) fage(index),lage(index),fyear(index),           &                                                           
                               lyear(index), Timing(index)                                                                          
      enddo                                                                                                                         
7030  format(4(I4,1X),1X,E25.16)                                                                                                    
                                                                                                                                    
      do index = 1,nageix                                                                                                           
        do year = fyear(index),lyear(index)                                                                                         
          iyear = year-fyear(index)+1                                                                                               
          write(fch, 7040) (Aindex(index,iyear,age-fage(index)+1),      &                                                           
              age=fage(index),lage(index))                                                                                          
        enddo                                                                                                                       
      enddo                                                                                                                         
7040  format(100(E25.16,1X))                                                                                                        
                                                                                                                                    
!     AGE-STRUCTURED DATA                                                                                                           
                                                                                                                                    
      do year = firstyear,lastyear+1                                                                                                
        iyear = year-firstyear+1                                                                                                    
        do age = firstage, lastage                                                                                                  
          iage = age-firstage+1                                                                                                     
          write(fch,7045) CN(iyear,iage),SW(iyear,iage),MO(iyear,iage), &                                                           
               NM(iyear,iage), N(iyear,iage), F(iyear,iage)                                                                         
        enddo                                                                                                                       
      enddo                                                                                                                         
7045  format (6(E25.16,1X))                                                                                                         
                                                                                                                                    
!     LANDINGS                                                                                                                      
                                                                                                                                    
      do year = firstyear, lastyear                                                                                                 
        write(fch, 7050) LA(year-firstyear+1)                                                                                       
      enddo                                                                                                                         
7050  format (E25.16)                                                                                                               
                                                                                                                                    
!     THE CONTROL BLOCK FOR THE MODEL FITTING                                                                                       
                                                                                                                                    
      write(fch, 7060) UseSep, Writeout, Full, TwoSel                                                                               
7060  format(4L3)                                                                                                                   
                                                                                                                                    
      write(fch, 7070) NySep, RefAge, NxData, NxParm, TermS, TermS2                                                                 
7070  format(4(I4,1X),2(E25.16, 1X))                                                                                                
                                                                                                                                    
      do i = 1,Nxparm                                                                                                               
        write(fch, 7080) Xbest(i), Xlow(i), Xhigh(i)                                                                                
      enddo                                                                                                                         
7080  format(3(E25.16),1X)                                                                                                          
                                                                                                                                    
      do index = 1,nssbix                                                                                                           
        write(fch,7090) Blambda(index)                                                                                              
      enddo                                                                                                                         
                                                                                                                                    
      do index = 1, nageix                                                                                                          
        write(fch, 7090) (Alambda(index,iage),                          &                                                           
         iage=1, lage(index)-fage(index)+1)                                                                                         
      enddo                                                                                                                         
7090  format(100(E25.16,1X))                                                                                                        
                                                                                                                                    
      do age = firstage, lastage                                                                                                    
        iage = age-firstage+1                                                                                                       
        write(fch,7100) RelWt(iage)                                                                                                 
      enddo                                                                                                                         
7100  format(E25.16)                                                                                                                
                                                                                                                                    
      do age = firstage, lastage                                                                                                    
        iage = age-firstage+1                                                                                                       
        do year = firstyear, lastyear                                                                                               
          iyear = year-firstyear+1                                                                                                  
          write(fch, 7110) W(iyear, iage)                                                                                           
        enddo                                                                                                                       
      enddo                                                                                                                         
      
! TESTING CODE                                                                                                                                    

!      write(*,*) 'TEST '
!      write(*,*) '>nage nssb >',nageix,nssbix
!      write(*,*) '>max >',MAX(nageix,nssbix)


! END TESTING CODE

      do i = 1,Max(nageix,nssbix)
!        write(*,*) '>i1  >',i,MAX(nageix,nssbix) ! also test
        if ( i .le. nssbix) then                                                                                                    
!          write(*,*)'>i2 >',i
          write(fch,7120) 0.0, 0.0, Plusgp(i),QAParm(i),                &                                                           
          QBParm(i)                                                                                                                 
        else                                                                                                                        
!          write(*,*)'>i3 >',i
          write(fch,7120) 0.0,0.0,Plusgp(i),QAparm(i), 0                                                                            
        endif                                                                                                                       
      enddo                                                                                                                         

7110  format(E25.16)
7120  format(2(E25.16),L3,2(I2,1X))                                                                                                 
                                                                                                                                    
!     the Stock-Recruit parameters                                                                                                  
                                                                                                                                    
      write(fch,8101) lag, FitSRR
!      write(*,*) 'TEST HERE'
!      write(*,*) FitSRR
!      write(*,8101) lag,FitSRR

8101  format (1X,I4,1X,L3) 
                                                                                                                                    
!      if (FitSRR) then                                                                                                             
!        write(fch, 8120) SRRLambda, SRRVar                                                                                         
!        write(fch, 8110) fryear, lag, a, b                                                                                         
!        do i = 1,20                                                                                                                
!          write(fch, 8120) OldStock(i), OldRecruit(i)                                                                              
!        enddo                                                                                                                      
!      endif                                                                                                                        
                                                                                                                                    
!     Weights on the years in the catch-at-age matrix                                                                               
                                                                                                                                    
      do iyear = 1, lastyear-firstyear+1                                                                                            
        write(fch, 8120) YearWt(iyear)                                                                                              
      enddo                                                                                                                         
                                                                                                                                    
      write(fch, 8130 ) LoFage, HiFage                                                                                              
      write(fch, '(I2)') language

8100  format (' ',L3)                                                                                                               
8110  format (' ',I4,1X,I2,1X,2E25.16)                                                                                              
8120  format (' ',2E25.16)                                                                                                          
8130  format (' ',I4,1X,I4)                                                                                                         
                                                                                                                                    
      close(fch)                                                                                                                    
      return                                                                                                                        
      end
