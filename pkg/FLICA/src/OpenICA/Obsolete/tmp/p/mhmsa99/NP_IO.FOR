c      
c    I/O Routines for version 3 of ICPROJ
c
c
c ////////////////////////////////////////////////////////////////////

      Subroutine  ReadProjFile ! Read Projection File

c ///////////////////////////////////////////////////////////////////
      implicit none
      Include 'Indat.inc'
      Include 'projc3.inc'

      integer year, iyear, age, iage, ioerr, ifleet, dummy
      character*132 line
      character*40 filename

c -------------------- EXECUTABLE CODE -----------------------------

      write(*,*)
122   continue
      ioerr = 1
      filename = 'x'
      do while ((filename .ne. ' ') .and. (ioerr .ne. 0))

        if (filename .eq. 'x') then
          write(*,100) 'Enter the name of the projection file '
          read(*,110) filename
        else
          write(*,130) 'Error number',ioerr, 'trying to read', 
     *                  filename     
          write(*,140) 'Enter a new filename to retry'
          read(*,*) filename
          if (filename .eq. ' ') then
            write(*,*) 'Read attempt abandoned.'
            stop
          endif
        endif
        open(12, file= filename, iostat=ioerr, status = 'old',
     *    recl=6000)
        if (ioerr .ne. 0) goto 122
        read(12,120) line
        read(12,120) line
        read(12,*) NFleet, YtPro
        YtPro=YtPro+1 ! because the NPro(1) = N(lastyear)
        read(12,120) line

        read(12,120) line
        do iage = 1, lastage-firstage+1 
          read(12,*) age, (CRatio(ifleet,iage), ifleet =1,Nfleet)   !------- catch ratio by fleet and age in last year of analysis
        enddo

        read(12,120) line
        do iage = 1, lastage-firstage+1 
          read(12,*) age, (Retention(ifleet,iage), ifleet =1,Nfleet)   !------- catch ratio by fleet and age in last year of analysis
        enddo

        read(12,120) line
        do iyear = 2, YtPro
          read(12,*) year, (ConsCat(ifleet, iyear),ifleet=1,Nfleet) !------ F multiplier by fleet and year
          if (year .ne. lastyear+iyear-1) then
           write(*,121)'Error in years specified for projection. These 
     *should start at ',lastyear+1 
121        format(' ',A63,1X,I4)        
          endif

         enddo

c        do ifleet = 1, Nfleet
c          Conscat(ifleet, YtPro+1) = Conscat(ifleet, YtPro)
c        enddo

        read(12,120) line
        do iage = 1, lastage-firstage+1
          read(12,*) age, (CWPro(ifleet, iage), ifleet=1, Nfleet)    !------ catch weight in projections
        enddo
        read(12,120) line
        do iage = 1, lastage-firstage+1
          read(12,*) age, (DiscWt(ifleet, iage), ifleet=1, Nfleet)    !------ catch weight in projections
        enddo

c       Read in TargetFM and CVTarg        
        read(12,120) line        
        read(12,*) ftyear
        read(12,120) line        
        do iyear = ftyear-lastyear+1, YtPro
          read(12,*) year,(TargetFM(ifleet,iyear), ifleet =1,Nfleet)
          if (year .ne. lastyear+iyear-1) then 
            write(*,*) 'Format error in projection file'
            write(*,*) 'while attempting to read Target Multipliers'
            write(*,*) 'should run from ',ftyear,' to ',
     *       lastyear+YtPro-1
            stop
          endif
        enddo
        read(12,120) line        
        do iyear = ftyear-lastyear+1, YtPro
          read(12,*) year,(CVTarg(ifleet,iyear), ifleet =1,Nfleet)
          if (year .ne. lastyear+iyear-1) then
            write(*,*) 'Format error in projection file'
            write(*,*)' attempting to read CV of Target Multipliers.'
            stop
          endif
        enddo
      close(12)
      enddo


100   format(' ',A38, ' --> ')  ! \
110   format(A40)
120   format(A132)
130   format(A13,1X,I4,1X,A15,1X,A30)
140   format(A29,' --> ') ! \

      return 
      end


c /////////////////////////////////////////////////////////////////////////

      Subroutine ChkCatRatio ! Check catch ratios

c /////////////////////////////////////////////////////////////////////////
c
c      The ratios of catches in number by fleet at each age should sum to 1 at
c          each age, but it is easy to make an error here. If such an error is
c          made then fish will either be 'invented' or lost from the projection.
c       The routine makes sure that the ratios do sum to exactly 1. A 5% tolerance
c          is allowed as an error in typing the file, which is then rounded so
c          that the ratio sums to one exactly. If the sum of ratios is further than 5%
c          from 1, it is assumed that a significant error has been made in the file and
c          the programme stops with a warning message.
c
c
      implicit none
      Include 'Indat.inc'
      Include 'projc3.inc'
      integer iage, ifleet

      double precision RatioSum

      do iage = 1,lastage-firstage+1 
        RatioSum = 0.0
        do ifleet = 1, Nfleet
          RatioSum = RatioSum + CRatio(ifleet, iage)
        enddo  ! fleets
        If ((RatioSum .lt. 0.95) .or. (RatioSum .gt. 1.05)) then ! 5% rounding error allowed
          write(*,*) 'Catch ratios sum to',RatioSum,'for age',
     *    firstage+iage-1
144   format(' ',A19,1X,F8.4,1X,A7,1X,I2)
          write(*,*) 'Correct this error before retrying.'
          stop
        endif

        ! if it's within 5% of 1, the just round it off

        do ifleet = 1, NFleet
          CRatio(ifleet, iage) = CRatio(ifleet, iage) /RatioSum ! Ensures catch ratios sum exactly to 1
        enddo  ! fleets
      enddo ! ages

      return
      end

      
c ////////////////////////////////////////////////////////////////////

      Subroutine SetProjVars

c ///////////////////////////////////////////////////////////////////

c
c    Stock weights, maturity ogives and natural mortality must be 
c    specified for the projections. The most usual way to set these is by 
c    using a mean over historic data. As these have already been 
c    entered and are accessible in the INDAT common block, it seems
c    appropriate to calculate these from the available data rather than
c    calculate them externally and re-enter them by hand.
c
c    This programme provides an option for calculating these three parameters
c    from a user-specified range of the historic data.
c
c
c
      implicit none
      include 'indat.inc'
      include 'projc3.inc'

      integer iyear,iage, datyear
      double precision sum, sumsq, value, SQSAFE, SAFELOG

      write(*,*) 
      write(*,*)'Population parameters for the projections are set by t'
     *,'aking a mean over a '
      write(*,*) 'number of the last years of the data set.'
      write(*,*) 'Specify the years for taking these means. '


c        NATURAL MORTALITY

      DatYear = 1
      do while ((DatYear .gt. lastyear) .or. (DatYear .lt. firstyear))
        write(*,*)
        write(*,100) 'Use mean natural mortality from ',lastyear,
     *   ' back to '
        read(*,*) DatYear
        if (DatYear .lt. firstyear) 
     *   write(*,127)'ERROR: Earliest data year is',firstyear,'RETRY.'
        if (DatYear .gt. lastyear)
     *   write(*,128) 'ERROR: No data later than',lastyear,' RETRY.'
      enddo ! error-trapping loop
      
127   format(' ',A31,1X,I4,1X,A6)
128   format(' ',A26,1X,I4,1X,A6)

      do iage = 1, lastage-firstage+1
        NMPro(iage) = 0.0  
      enddo  
      do iage = 1, lastage-firstage+1
       do iyear = 1, lastyear-DatYear+1
         NMpro(iage)=NMPro(iage)+(NM(iyear,iage))/
     *        dble(lastyear-DatYear+1)
       enddo
      enddo

c     MATURITY OGIVE

      DatYear = 1
      do while ((DatYear .gt. lastyear) .or. (DatYear .lt. firstyear))
        write(*,*)
        write(*,110) 'Use mean maturity ogive from ',lastyear,
     *   ' back to '
        read(*,*) DatYear
        if (DatYear .lt. firstyear) 
     *   write(*,127) 'ERROR: Earliest data year is',firstyear,'RETRY.'
        if (DatYear .gt. lastyear)
     *    write(*,128) 'ERROR: no data later than',lastyear,'RETRY.'
      enddo ! error-trapping loop
      
      do iage = 1, lastage-firstage+1
        MOPro(iage) = 0.0d0  
        VarMat(iage) = 0.0d0
        Sum = 0d0
        SumSQ = 0d0
        do iyear = DatYear-firstyear+1, lastyear-firstyear+1
          value = DASIN( SQSAFE(dble(MO(iyear,iage))))   
          MOpro(iage)=MOPro(iage)+value
          Sum = Sum + value        
          if (DatYear .ne. lastyear) then
            SumSQ = SumSQ + value*value
          endif
       
       enddo   ! years
       if (DatYear .ne. lastyear) then
         VarMat(iage) = SumSQ - (Sum*Sum)/dble(Lastyear-DatYear+1)
         VarMat(iage) = SQSAFE(VarMat(iage)/dble(Lastyear-DatYear))
       else
         VarMat(iage) = 0d0
       endif
       MOPro(iage) = Sum/dble(Lastyear-Datyear+1)
      enddo    ! ages

c     WEIGHT AT AGE IN THE STOCK 

      DatYear = 1
      do while ((DatYear .gt. lastyear) .or. (DatYear .lt. firstyear))
        write(*,*)
        write(*,120) 'Use mean weight at age in the stock from '
     *  ,lastyear, ' back to '
        read(*,*) DatYear
        if (DatYear .lt. firstyear) 
     *   write(*,127)'ERROR: Earliest data year is',firstyear,'RETRY.'
        if (DatYear .gt. lastyear)
     *    write(*,128)'ERROR: No data later than',lastyear,'RETRY.'
      enddo ! error-trapping loop
      
      do iage = 1, lastage-firstage+1
        SWPro(iage) = 0d0  
        VarSW(iage) = 0d0
       do iyear = datyear-firstyear+1, lastyear-firstyear+1
         SWpro(iage)=SWPro(iage)+(safelog(dble(SW(iyear,iage))))
         if (DatYear .ne. lastyear) then
          VarSW(iage)=VarSW(iage)+
     *       dble(safelog(dble(SW(iyear,iage)))*
     *         safelog(dble(SW(iyear,iage)))) 
         else
           VarSW(iage) = 0d0
         endif
       enddo   ! years
       if (DatYear .ne. lastyear) then
         VarSW(iage)=VarSW(iage)- (SWPro(iage)*SWPro(iage))
     *      /dble(lastyear-Datyear+1)
         VarSW(iage)=sqsafe(VarSW(iage)/dble(lastyear-Datyear))
       else
         VarSW(iage) = 0d0
       endif
       SWPro(iage)=(SWPro(iage)/dble(lastyear-Datyear+1))
      enddo    ! ages


100   format(' ',A33,1X,I4,A9,' --> ') ! \
110   format(' ',A30,1X,I4,A9,' --> ') ! \
120   format(' ',A41,1X,I4,A9,' --> ') ! \

      return
      end



c /////////////////////////////////////////////////////////////////////

      Subroutine ReadVCV(VCV)

c /////////////////////////////////////////////////////////////////////

c
c      Reads the variance-covariance matrix from file ICA.VC to the 
c      matrix VCV.
c
c
      implicit none
      integer ioerr, i, j 
      INCLUDE 'INDAT.INC'
      INCLUDE 'SEPMODEL.INC'
      double precision VCV(maxparm,maxparm)
      character*80 line
      integer dummy

c --------------------- Executable Code -------------------------------

      Open(12, file = 'ICA.VC', status = 'OLD', iostat = ioerr,
     * recl=6000 )
      read (12,10) line
      read(12,*) (dummy , j = 1,Nxparm)


10    format (A80)
      do i = 1, Nxparm
        read(12, *) dummy, (VCV(i, j),  j = 1,Nxparm)
      enddo
      close (12)
      if (ioerr .ne. 0) then
        write(*,*)
        write(*,*) 'I/O Error in accessing file ICA.VC.  Run aborted.'
        write(*,*) 'Error Number = ',ioerr
        stop
      endif
      
      return
      end


c //////////////////////////////////////////////////////////////////////
   
      Subroutine ReadMBAL(MBAL) 

c //////////////////////////////////////////////////////////////////////
      implicit none
      double precision MBAL

c------------------------- EXECUTABLE CODE ------------------------------

      write(*,*)
      write(*,100)' Enter the minimum acceptable spawning stock size'
      read(*,*) MBAL

100   format(' ',A50,' --> ') ! \

      return
      end



c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






c ////////////////////////////////////////////////////////////////////

      Subroutine WriteNewSRR

c ///////////////////////////////////////////////////////////////////
      implicit none
      include 'indat.inc'
      include 'sepmodel.inc'
      include 'srr.inc'
      include 'projc3.inc'
       
      integer i, nosrrdata  
      double precision StockS(maxyear), Recruit(maxyear)
      
      Double precision SRRFUNCT

      NoSrrData = lastyear-firstyear+1

      Call GetSRR( StockS, Recruit, NoSRRdata)

      open(1, file = 'icapro.srr', status='unknown') 
      write(1,*) 'STOCK - RECRUIT RELATION PARAMETERS'

      do i =1,8
        if (dabs(SRRPARM(i)) .gt. 1d-9) then
          write(1,100) i, SRRPARM(i)
        else
          write(1,100) i, 0d0
        endif
      enddo

100   format(' ',I2,3X,E23.12)

      write(1,*) 'Stock and Observed and Expected Recruitments by Year'

      do i = 1,NoSRRdata
        write(1,200) i+firstyear-1,StockS(i),Recruit(i),
     *     SRRFUNCT(SRRPARM,StockS(i))
      enddo

200   format(' ',I4,2X,3(E23.12))

      close(1)

      return
      
      end   ! of WriteNewSRR




c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ////////////////////////////////////////////////////////////////////
c
      Subroutine Hello
c
c ////////////////////////////////////////////////////////////////////


      integer i


      do i=1,5
        write(*,*)
      enddo

      write(*,*) '                          Medium-Term Projections'              
      write(*,*) '                          -----------------------'
      write(*,*) 
      write(*,*) '                            ICPROJ version 1.3 '    
      write(*,*)             
      write(*,*) '                         SOAFD Marine Laboratory'
      write(*,*) '                                  Aberdeen '
      do i=1,3
        write(*,*)
      enddo
      write (*,*)  '                      Dated March 1997 for ICAv1.3 '
      do i=1,3
        write(*,*)
      enddo


      return

      end

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



c ///////////////////////////////////////////////////////////////////////

      Subroutine ReadMaxFMult

c ///////////////////////////////////////////////////////////////////////


      include 'indat.inc'
      include 'projc3.inc'


      MaxFMult = -99.0d0
      
      do while ((MaxFmult .le. 1d0)  .or. (maxFmult .gt. 20d0)) 
        if (MaxFmult .ne. -99.0) then
          write(*,*) 'Error: Allowable range is 1.0 to 20.0. Retry.'
          write(*,*)
        endif
        write(*,100) 'Enter the maximum allowable F-multiplier '
        read(*,*) MaxFMult
      enddo


100   Format (' ',A42,' --> ') ! \


      return
      end

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


c //////////////////////////////////////////////////////////////////////

        Double precision Function SafeLog(X)

c ///////////////////////////////////////////////////////////////////////

        double precision X

        if (X .le. 0) then
          SafeLog = dlog(1d-8)
        else  
          SafeLog = dlog(X)
        endif

        return
        end 
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
