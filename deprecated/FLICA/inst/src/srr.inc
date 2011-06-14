module srr_module

	use kind_module
	use data_definition
	use message_definition
	use screen_io_module

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!                 
!    UNIT SRR.INC   Contains subroutines relating to estimation of         
!    stock-recruit relationship          
!                 
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

contains   
  
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
                  
      real (wp), dimension (maxyear) :: X, Y
      integer :: nodata, i             
      real (wp) :: sumX, sumY, sumX2, SumXY         
      real (wp) :: slope, intercept                
                  
! ------------------------- EXECUTABLE CODE -------------------------------
                  
      nodata=lastyear-firstyear+1 ! no of SSB estimates for which there are corresponding recruitment obs.       
                  
      Call GetSRR(X,Y, nodata)       
                  
! Data transformation                
                  
!      open( 114, file='c:\temp\srr.txt',iostat=ioerr,status='unknown')              
                  
!          do i = 1, Nodata               
!             write(114,*) i, X(i), Y(i)
!          enddo                        
 
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
                  
      SumXY = SUMXY - (SumX*SumY)/dble(NoData)          
      SumX2  = SumX2  - (SumX*SumX)/dble(NoData)        
                  
      slope = SumXY/SumX2            
                  
      intercept = SumY/dble(Nodata) - slope * SumX/dble(Nodata)            
                  
      a = 1./slope
                  
      b = intercept * a              

 !     write(114,*) slope, intercept, a, b
     
 !    close(114)   
                  
!     Estimate the error variance about log-log fit     
                  
! Data detransformation              

      SumX2 = 0.0 
      do i  = 1, Nodata              
        Y(i) =  dlog(X(i)/Y(i))          ! dependent variable is log recruits                 
        X(i) =  dlog( (X(i)*a)/(b+X(i) )) ! Eqn. 7.6.6. of Holborn & Walters                  
        SumX2 = SumX2 + (Y(i)-X(i)) * (Y(i)-X(i))       
      enddo       
      ! SRRLambda = SumX2/dble(Nodata-2)                  
      ! - commented out just now as I'M NOT SURE WHAT IT DOES!

!     END OF LINEAR APPROXIMATION, NOW FIND IMPROVED SOLUTION WITH         
!     NONLINEAR REGRESSION           
                  
      Return      
      End subroutine ApproxSRR
                  
! //////////////////////////////////////////////////////////////////////   
                  
      Subroutine GetSRR(X,Y,nodata) ! Extract stock and recruit data from main datasets       
                  
! //////////////////////////////////////////////////////////////////////   
                  
      integer :: i, nodata, year        
      real (wp), dimension (maxyear), intent (inout) :: X, Y
      ! double precision CalcSSB       
                  
      i = 0       
                  
        do year = firstyear, lastyear
          i = i+1 
          X(i) =  CalcSSBsrr(year)      
          Y(i) = N(year-firstyear+1, 1) !   array dimension 1 is firstage  
!          write(*,*) 'GetSRR: ',i,X(i),Y(i)            
        enddo     
                  
                  
      nodata = i  
                  
!     At this stage X and Y are the stock and recruits in each year.       
!     Now match corresponding stock and recruits, by    
!     shuffling and truncating the series.              
                  
                  
      nodata = nodata-lag            
                  
!      write(*,*) nodata, lag        
                  
      do i = 1, nodata               
        Y(i) = Y(i+lag)              
!        write(*,*) 'GetSRR 2 : ',i,X(i),Y(i+lag)       
      enddo       
                  
      return      
                  
      end subroutine GetSRR    
                  
! //////////////////////////////////////////////////////////////////////// 
                  
      Subroutine WriteSRRFile        
                  
! //////////////////////////////////////////////////////////////////////// 
                  
      real (wp), dimension (maxyear) :: Stock, Recruits
      integer :: ioerr, i, nodats       
                  
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
      end subroutine WriteSRRFile

!///////////////////////////////////////////////////////////////////////      
!                       
      double precision Function CalcSSBsrr(Year)      
!                       
!///////////////////////////////////////////////////////////////////////      
!                       
!     Calculates the SSB in a given year from fitted populations,             
!                    at the time of spawning.      
!                       
                        
                        
!     Local variables   
                        
      integer :: year, iage
      double precision ssb                         
                        
                        
!     ____________________EXECUTABLE CODE______________________________       
                        
      ssb = 0d0         
      do iage = 1,lastage-firstage+1               
	if (MO(year-firstyear+1, iage) .gt. 0d0) then     
	 ssb=ssb+(N(year-firstyear+1, iage)*        &     
                 dble(MO(year-firstyear+1, iage))*                      &     
                 dble(SW(year-firstyear+1, iage))*                      &     
                 dexp(- F(year-firstyear+1, iage)*PF                    &     
                      -NM(year-firstyear+1, iage)*PM) )                       
	endif                  
      enddo             
      CalcSSBsrr = SSB     
      return            
      end function CalcSSBsrr
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       

end module srr_module                 
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
