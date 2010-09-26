c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
c
c    UNIT SRR.FOR   Contains subroutines relating to estimation of 
c                   stock-recruit relationship
c
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






c /////////////////////////////////////////////////////////////////////////

      Subroutine ApproxSRR ! Estimate of Bev-Holt SRR by linear regression

c /////////////////////////////////////////////////////////////////////////
c
c     Method for this is given by Hilborn and Walters, 1992 (Quantitiative Fisheries Stock Assessment)  
c          as Eqn. 7.6.5. on p. 270. Although this is not the preferred method       
c          it is probably good enough to get a starting estimate.
c
c     A linear regression is calculated on
c
c              Stock/Recruit   =   b/a + 1/a . Stock
c
c     where b/a and 1/a, hence a and b are parameters to estimate.
c
c
      implicit none
      INCLUDE "SRR.INC"
      INCLUDE "INDAT.INC"
      
      double precision X (maxyear), Y (maxyear) 
      integer year, iyear, nodata , i, test
      real sumX, sumY, sumX2, SumXY
      real slope, intercept

c ------------------------- EXECUTABLE CODE -------------------------------

      nodata=(lastyear-firstyear+1) ! no of SSB estimates for which there are corresponding recruitment obs.
      
c      if (nodata .ge. 40) then
c        write(*,*) 'Too many data for Stock-recruit estimation routine.'
c        stop
c      endif

      Call GetSRR(X,Y, nodata)


c Data transformation

      do i = 1, Nodata
        Y(i) =  X(i)/Y(i)  ! dependent variable is stock per recruit
      enddo

c Now a simple regression of Y on X : See Hilborn & Walters page 270 Eqn. 7.6.5.

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

c     Estimate the error variance about log-log fit    

c Data detransformation

      SumX2 = 0.0
      do i  = 1, Nodata
        Y(i) =  dlog(X(i)/Y(i))          ! dependent variable is log recruits
        X(i) =  dlog( (X(i)*a)/(b+X(i) )) ! Eqn. 7.6.6. of Holborn & Walters
        SumX2 = SumX2 + (Y(i)-X(i)) * (Y(i)-X(i))
      enddo
      SRRLambda = SumX2/float(Nodata-2)

c     END OF LINEAR APPROXIMATION, NOW FIND PRECISE SOLUTION WITH 
c     NONLINEAR REGRESSION

      Return
      End


c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






        

c //////////////////////////////////////////////////////////////////////

      Subroutine GetSRR(X,Y,nodata) ! Extract stock and recruit data from main datasets 

c //////////////////////////////////////////////////////////////////////      
      implicit none
      INCLUDE "INDAT.INC"
      INCLUDE "SRR.INC"


      integer i, nodata, year 
      double precision X(maxyear), Y(maxyear)
      double precision CalcSSB




      i = 0

c      write(*,*) 'Debug SRR: 1'


        do year = fychosen, lychosen
          i = i+1
c          write(*,*) 'Debug SRR: 2.5',year
          X(i) = CalcSSB(year)  
c          write(*,*) 'Debug SRR: 2.6',year,i,firstyear,
c     *                N(year-firstyear+1,1)
          Y(i) = N(year-firstyear+1, 1)       !   array 1 is firstage 
c          write(*,*) 'Debug SRR: 2.7', i, Y(i) 
        enddo  
      
c      write(*,*) 'Debug SRR: 3'

      nodata = i

c     At this stage X and Y are the stock and recruits in each year.
c     Now match corresponding stock and recruits, by
c     shuffling and truncating the series.

      
      nodata = nodata-lag

      do i = 1, nodata
        Y(i) = Y(i+lag) 
      enddo
c      write(*,*) 'Debug SRR: 4'
      return 

      end  ! Of subroutine GetSRR



c ////////////////////////////////////////////////////////////////////////

      Subroutine WriteSRRFile

c ////////////////////////////////////////////////////////////////////////
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






        
        
      


     

