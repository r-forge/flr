! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
!                                                                                                                                   
!    UNIT SRR.FOR   Contains subroutines relating to estimation of                                                                  
!                   stock-recruit relationship                                                                                      
!                                                                                                                                   
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
                                                                                                                                    
      Subroutine ApproxSRR ! Estimate of Bev-Holt SRR by linear regression                                                          
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!     Method for this is given by Hilborn and Walters, 1992 (Quantitiative Fisheries Stock Assessment)                              
!          as Eqn. 7.6.5. on p. 270. Although this is not the preferred method                                                      
!          it is probably good enough to get a starting estimate.                                                                   
!                                                                                                                                   
!     A linear regression is calculated on                                                                                          
!                                                                                                                                   
!              Stock/Recruit   =   b/a + 1/a . Stock                                                                                
!                                                                                                                                   
!     where b/a and 1/a, hence a and b are parameters to estimate.                                                                  
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      INCLUDE "SRR.INC"                                                                                                             
      INCLUDE "INDAT.INC"                                                                                                           
                                                                                                                                    
      double precision X (maxyear), Y (maxyear)                                                                                     
      integer year, iyear, nodata , i, test                                                                                         
      real sumX, sumY, sumX2, SumXY                                                                                                 
      real slope, intercept                                                                                                         
                                                                                                                                    
! ------------------------- EXECUTABLE CODE -------------------------------                                                         
                                                                                                                                    
      nodata=(lastyear-firstyear+1) ! no of SSB estimates for which there are corresponding recruitment obs.                        
                                                                                                                                    
!      if (nodata .ge. 40) then                                                                                                     
!        write(*,*) 'Too many data for Stock-recruit estimation routine.'                                                           
!        stop                                                                                                                       
!      endif                                                                                                                        
                                                                                                                                    
      Call GetSRR(X,Y, nodata)                                                                                                      
                                                                                                                                    
                                                                                                                                    
! Data transformation                                                                                                               
                                                                                                                                    
      do i = 1, Nodata                                                                                                              
        Y(i) =  X(i)/Y(i)  ! dependent variable is stock per recruit                                                                
      enddo                                                                                                                         
                                                                                                                                    
! Now a simple regression of Y on X : See Hilborn & Walters page 270 Eqn. 7.6.5.                                                    
                                                                                                                                    
      SumX = 0.0                                                                                                                    
      SumY = 0.0                                                                                                                    
      SumX2 = 0.0                                                                                                                   
      SumXY = 0.0                                                                                                                   
                                                                                                                                    
      do i = 1, nodata                                                                                                              
         SumX = SumX+X(i)                                                                                                           
         SumY = SumY+Y(i)                                                                                                           
         SumX2= SumX2+X(i)*X(i)                                                                                                     
         SumXY= SumXY+X(i)*Y(i)                                                                                                     
      enddo                                                                                                                         
                                                                                                                                    
      SumXY = SUMXY - (SumX*SumY)/float(NoData)                                                                                     
      SumX2  = SumX2  - (SumX*SumX)/float(NoData)                                                                                   
                                                                                                                                    
      slope = SumXY/SumX2                                                                                                           
                                                                                                                                    
      intercept = SumY/float(Nodata) - slope * SumX/float(Nodata)                                                                   
                                                                                                                                    
      a = 1./slope                                                                                                                  
                                                                                                                                    
      b = intercept * a                                                                                                             
                                                                                                                                    
!     Estimate the error variance about log-log fit                                                                                 
                                                                                                                                    
! Data detransformation                                                                                                             
                                                                                                                                    
      SumX2 = 0.0                                                                                                                   
      do i  = 1, Nodata                                                                                                             
        Y(i) =  dlog(X(i)/Y(i))          ! dependent variable is log recruits                                                       
        X(i) =  dlog( (X(i)*a)/(b+X(i) )) ! Eqn. 7.6.6. of Holborn & Walters                                                        
        SumX2 = SumX2 + (Y(i)-X(i)) * (Y(i)-X(i))                                                                                   
      enddo                                                                                                                         
      SRRLambda = SumX2/float(Nodata-2)                                                                                             
                                                                                                                                    
!     END OF LINEAR APPROXIMATION, NOW FIND PRECISE SOLUTION WITH                                                                   
!     NONLINEAR REGRESSION                                                                                                          
                                                                                                                                    
      Return                                                                                                                        
      End                                                                                                                           
                                                                                                                                    
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine GetSRR(X,Y,nodata) ! Extract stock and recruit data from main datasets                                             
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
      implicit none                                                                                                                 
      INCLUDE "INDAT.INC"                                                                                                           
      INCLUDE "SRR.INC"                                                                                                             
                                                                                                                                    
                                                                                                                                    
      integer i, nodata, year                                                                                                       
      double precision X(maxyear), Y(maxyear)                                                                                       
      double precision CalcSSB                                                                                                      
                                                                                                                                    
!                                                                                                                                    
      write(*,*) 'Includes scaled 1972-83 Scaled West Mackerel SRR'
                                                                                                                                    
      i = 0                                                                                                                         
                                                                                                                                    
!      write(*,*) 'Debug SRR: 1'                                                                                                    
                                                                                                                                    
     fychosen=1972

     !  1972, 
      X(1)=3470846.625d0
      Y(1) =2255703.75
     !  1973
      X(2) = 3584453.625
      Y(2) = 4958021.25
     ! 1974 
      X(3) =3613257
      Y(3) = 3852821.25
     ! 1975
     X(4) = 3329910
     Y(4) = 5493015
    ! 1976
     X(5) =2929663.125
     Y(5)= 5674398.75
    ! 1977
     X(6) =2910768.75
     Y(6) =1073902.5
    ! 1978
    X(7) =3114870.75
    Y(7) =3739938.75
    ! 1979
    X(8) = 2741618.25
    Y(8) = 6153041.25
    ! 1980
    X(9) = 2332311.75
    Y(9) = 6109481.25
    ! 1981
    X(10)=2431901.25
    Y(10)=7534901.25
    ! 1982
    X(11)=2309515.875
    Y(11) =2074680
    ! 1983,
    X(12) = 2586211.875
    Y(12)= 1531653.75
                                                                                                                                    
    i = 12
        do year = 1984, lychosen   ! fychosen, lychosen                                                                                               
          i = i+1                                                                                                                   
!          write(*,*) 'Debug SRR: 2.5',year                                                                                         
          X(i) = CalcSSB(year)                                                                                                      
!          write(*,*) 'Debug SRR: 2.6',year,i,firstyear,                                                                            
!     *                N(year-firstyear+1,1)                                                                                        
          Y(i) = N(year-firstyear+1, 1)       !   array 1 is firstage                                                               
!          write(*,*) 'Debug SRR: 2.7', i, Y(i)                                                                                     
        enddo                                                                                                                       
                                                                                                                                    
!      write(*,*) 'Debug SRR: 3'                                                                                                    
                                                                                                                                    
      nodata = i                                                                                                                    
                                                                                                                                    
!     At this stage X and Y are the stock and recruits in each year.                                                                
!     Now match corresponding stock and recruits, by                                                                                
!     shuffling and truncating the series.                                                                                          
                                                                                                                                    
                                                                                                                                    
      nodata = nodata-lag                                                                                                           
                                                                                                                                    
      do i = 1, nodata                                                                                                              
        Y(i) = Y(i+lag)                                                                                                             
      enddo                                                                                                                         
!      write(*,*) 'Debug SRR: 4'                                                                                                    
      return                                                                                                                        
                                                                                                                                    
      end  ! Of subroutine GetSRR                                                                                                   
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
      Subroutine WriteSRRFile                                                                                                       
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
      implicit none                                                                                                                 
      include "indat.inc"                                                                                                           
      Include "SRR.inc"                                                                                                             
                                                                                                                                    
      Double Precision Stock(maxyear), Recruits(maxyear)                                                                            
      integer ioerr, nodats, i                                                                                                      
                                                                                                                                    
      Call GetSRR(Stock, Recruits, Nodats)                                                                                          
                                                                                                                                    
      open( 14, file='ica.srr',iostat=ioerr,status='unknown')                                                                       
                                                                                                                                    
      write(14,*) firstyear, nodats                                                                                                 
      write(14,*) a, b                                                                                                              
      do i = 1,nodats                                                                                                               
        write(14,*) (firstyear+i-1), Stock(i), Recruits(i)                                                                          
      enddo                                                                                                                         
                                                                                                                                    
      if (ioerr .ne. 0) then                                                                                                        
        write(*,*)' I/O Error number ',ioerr,' trying to write ICA.SRR'                                                             
        stop                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
      close(14)                                                                                                                     
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
