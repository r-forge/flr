c      

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c      UNIT  ICPROJ : Stock projection with variance estimates

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c
c
c
c
c
c/////////////////////////////////////////////////////////////////////
c
       Program NproS ! NPRO2 extended for management simulations
c
c ///////////////////////////////////////////////////////////////////
c
      IMPLICIT NONE
c
c     Forwards projection of ICA estimated populations and parameters
c       This programme uses a routine which does a multifleet stock projection
c       in a conventional manner, starting with the populations in the last 
c       ICA2 fit (in which a stock-recruit relation must have been fitted).
c     Selection pattern and F-multipliers by fleet and year must be provided in a 
c     file to the programme at run-time, named the 'projection file'.
c       The stock is projected forwards with new recruitments drawn from the 
c       fitted stock-recruit relation. Catches by fleet are estimated using the
c       weights at age by fleet in the projection file.
c     The routine generates a single 'core' projection, which would be the  
c     deterministic projection from the fitted parameters. 
c       Variances about the core projection are calcualted by the delta method.
c     This relies on calculating a matrix of the first derivatives of the projection
c     variables (future catches by fleet and stock sizes) with respect to each of
c     the relevant parameters fitted in the ICA2 programme and for which the
c     variance-covariance matrix has been calculated.
c       Variances of the projected catches and stock sizes can be calculated
c       from the variance-covariance matrix and from the matrix of derivatives.
c
c     Given the estimate of future stock size with attached variance and a defined
c     minimum acceptable stock size it is then a simple matter to calcualte the risk of the 
c     stock falling below a certain size under assumptions of normality. 
c      
c
c         Programme Calls  :   Readblock          reads ICA data and parameters
c                              ReadVCV            read variance-covariance matrix
c                              ReadProjFile       read projection file
c                              ChkCatRatio        check catch ratios & round up
c                              SetProjVars        calculate mean Wts, M, ogives etc for projection
c                              Project            does the projection calculation
c                              ReadMBAL           read minimum allowed SSB 
c                              S15ADF             NAG routine for cum. normal distribution
c
c
c           Files created:    Project.pop         Projected populations and catches
c                             ICA.PRO             Projected stock and catch with variance estimates
c                                                 and risk of stock falling below MBAL     
c                             Derivs.tst          matrix of derivatives,     d(Stock or catch estimate in year)
c                                                                            ---------------------------------
c                                                                                d(ICA parameter estimate)
c
c
      INCLUDE 'indat.inc'
      INCLUDE 'sepmodel.inc'
      INCLUDE 'Stats.inc'
      INCLUDE 'SRR.inc'
      INCLUDE 'projc3.inc'        ! the projected populations etc.
      INCLUDE 'MSGPRO1.inc'       ! messages 

c     Local variables

      double precision VCV(maxparm,maxparm)  ! ------------------ Variance-Covariance matrix
      integer iyear, i, jyear, iage, parmno, age
      integer ifleet, index
      integer noPercentiles
      double Precision Percentile(10)
      character*1 dummy
      double precision lastf, MBAL 

      double precision S(maxage) !-------------------------- selection pattern

      INCLUDE 'MSGPRO2.inc'       ! define messages 

c ------------------ EXECUTABLE CODE -------------------------------------

c     Initialise the random number generator
      
      call Hello           !------------------ Display start-up Screen

      NoPercentiles = 10 !max, this is array size
      Language = 2

      call Screen_in_i(HW(1,Language),i,32000,-32000,Language)


      call G05CBF(i)
      
      dummy = 'Z'

c      do while (Scan(dummy, 'iIxX') .eq. 0)
c        write(*,33)'Use population parameters from ICA or XSA (I/X) ?'      
c        read(*,34) dummy
c33       format(' ',A52,' --> ')   ! \
c34       format(A1)                   
c      enddo

c      if (Scan(dummy,'iI') .eq. 1) then

        call readblock       !----------------- Pick up the ICA data and estimated parameters
        call ReadVCV(VCV)    !---------------- Read the Variance-Covariance Matrix 
        if (fitSRR) then
          Nxparm=Nxparm-2
          FitSRR = .false.
        endif

         if (LAG .EQ. -5) THEN
           call Screen_in_i(HW(1,Language),lag,-10,10,Language)
         endif

         call OverWritePop
        


        



      
c      else                 ! read data in Lowestoft format
c        Call ReadPro
c        Call GetSumm
c        Call WriteBlock    ! for debugging
c        Call GetTune(VCV)
c        Call GetFRefRange
c        Call ReadSrrDat
c      endif

      dummy = 'Z'



      
      call ReadProjFile    !---------------- Read the catch ratios and F-Multipliers
      call ChkCatRatio     !---------------- Check the catch ratios, round up 
      call SetProjVars     !---------------- Calculate the mean wts etc. 
      Call ReadMBAL(MBAL)  !---------------- Read in the MBAL      
      Call ReadMaxFMult    !---------------- Read the maximum F-multiplier 
      
c      MaxFmult = 5d0      
      Call FitNewSrr       !---------------- Nonlinear fit of 6-parameter GP SRR 
      Call WriteNewSRR     !---------------- Write out the parameters 
      Call SetMOSW(.false., YtPro) ! -------------- Set up temporary maturity and stock weights with noe error         
      Call Project(Xbest, YtPro, .false.)  ! -------- Do the baseline projection
     
     


c     Copy parameters out of XBEST into local S, F, etc.

      parmno = NySep
      
      If (TwoSel) parmno = parmno+ (lastage-firstage+1-3)

      do age= firstage, lastage-1
        iage = age-firstage+1
        if (age .eq. Refage) then
          S(iage) = 1.0
        else if (age .eq. lastage-1) then
          S(iage) = TermS
        else if ((age .ne. Refage) .and. (age .ne. lastage-1)) then
          parmno = parmno+1
          s(iage) = dexp(Xbest(parmno))+1d-6
        endif
      enddo

      iyear=ytpro

      S(lastage-firstage+1) = S(lastage-firstage)


! ------------------------- Write details of the core projection to disk  

      Open (14, file='Project.Pop',status='unknown',recl=6000)   ! this file holds the projected populations
      
      write(14,*) 'Populations in the Projections'
      write(14,315) (jyear+lastyear-1, jyear=1,iyear)
      do iage =1, lastage-firstage+1
        write(14,316) iage, (Npro(jyear,iage)/1000.0,jyear=1,iyear)
      enddo
315   format (' ',3X,40(5X,I4))
316   format (' ',I2,1X,40(F8.0,1X))      
      write(14,*) 'Fishing Mortality in the Projections'
      do iage=1, lastage-firstage+1
        write(14,317) iage, (FPro(jyear,iage), jyear = 1, iyear)
      enddo
317   format (' ',I2,1X,40(E15.8,1X))

      do ifleet = 1, Nfleet


        do iage = 1, lastage-firstage+1
          write(14,317) iage,  (dexp(Xbest(NySep))*
     *    S(iage)*
     *    CRatio(ifleet,iage)*
     *    FMultPro(ifleet,jyear), jyear = 1, iyear)         
        enddo  
      enddo

      write(14,*)
      write(14,*)'Natural Mortality, Maturity Ogive and Stock Weights'
      do iage = 1, lastage-firstage+1  
        write(14,317) iage, NMPro(iage), MCMO(1,iage), MCSW(1,iage)
      enddo
      write(14,*)
      write(14,*)
      write(14,*) ' S.d. of Maturity Ogive and Stock Weights'
      write(14,*) '      for  root-arcsine and log transforms'
      do iage = 1, lastage-firstage+1  
        write(14,317) iage, VarMat(iage), VarSW(iage)
      enddo
      write(14,*)

      write(14,*) ' Stock Size Trajectory '

      do jyear = 1, YtPro 
        write(14,9000) jyear+lastyear-1, Stock(jyear)
      enddo

9000  format(' ',I4,2X,E23.12)

      write(14,*) 'Fleet Catches by Weight'

      write(14,*) 'Catch Estimates by Fleet'
      do iyear = 1, YtPro
        write(14,9010) iyear+lastyear-1, (FLCatch(ifleet,iyear),
     *     ifleet = 1, NFleet)
      enddo

9010  format(' ',I4, 6(E23.12, 2X))

      write(14,*) 'Fleet Catches by Number'

      do ifleet = 1, Nfleet

        write(14,700) 'Catches by Fleet ',ifleet
700     format (A18,1X,I1)

        LastF = dexp(Xbest(NySep))+1d-6
        
        do iage= 1, lastage-firstage+1
          write(14,9010) iage+firstage-1, 
     *     (NPro(jyear,iage)*
     *     LastF*S(iage)*CRatio(ifleet,iage)*
     *     FMultPro(ifleet,jyear)/
     *     (S(iage)*CRatio(ifleet,iage)*
     *     LastF*FMultPro(ifleet,jyear) + NMPro(iage)) 
     *     *(1d0 - dexp( dble(- NMPro(iage) - LastF*
     *     S(iage)*CRatio(ifleet,iage)*FMultPro(ifleet,jyear)))), 
     *      jyear = 1, YtPro)
        enddo  ! ages

      enddo ! fleets 

      write(14,*) 'Total Catches by Number '

      do iage= 1, lastage-firstage+1
        write(14,9010) iage+firstage-1, 
     *  (NPro(jyear,iage)* FPro(jyear,iage)/
     *  (FPro(jyear,iage)+NMPro(iage)) *
     *  (1d0 - dexp( - NMPro(iage) -FPro(jyear,iage))),
     *    jyear = 1, YtPro)
      enddo  ! iages
      
      close(14)


c
c     Find the percentiles required by the user
c
      Call GetPercentiles(NoPercentiles, Percentile)




c     Core projection written to disk; Now do the variance estimates
c ----------------------------------------------------------------------

c     Set up the reference vector for multivariate random numbers


      Call MCPROJ (VCV, MBAL)

c
c     Now read the *.MC files and create the *.pby files
c

      Call CreatePBYfiles(Nfleet, NoPercentiles, Percentile, 10)



      
      stop 
      end



c ///////////////////////////////////////////////////////////////////////

      double precision Function BootStrapRecruit(SSB,year,RecERR)

c ///////////////////////////////////////////////////////////////////////
c      
c     Generates a recruitment for year 'year'      
c     given that stock size = SSB.  
c      
c        If (RECERR) = true   then a bootstrapped (random resampling with
c          replacement) error is added to the recruitment prediction
c      
c      
c
c
c
      implicit none
      include 'indat.inc'
      include 'projc3.inc'
      include 'srr.inc'
      

      double precision SSB, SRRFunct, XJ(8), Recruit, X
      double precision G05CAF, spawn, resid, CalcSSB, CalcProSSB
      integer pos, year, i, nosrrdata
      logical RecErr

      do i= 1,8
        XJ(i) = SRRParm(i) 
      enddo  

      Recruit = SRRFunct(XJ, SSB)

      if ((dabs(XJ(5)) .gt. 1d-8) .and. (RecERR)) then !-------------------------- Autocorrelated errors
        if (Year -lag .le. lastyear) then   ! take SSB from stock assessment
          Spawn = CalcSSB(year-lag)
        else
          Spawn = CalcProSSB(year-lastyear-lag)
        endif                            
        if (year-1 .le. lastyear) then        ! the variable resid holds the observed recruitment in previous year
          resid = N(year-firstyear,1)
        else
          resid = Npro(year-lastyear-1,1)
        endif

        resid = dlog(resid)-dlog(SRRFunct(XJ, Spawn))
        Recruit = Recruit*dexp(XJ(5)*resid)
      endif

c     Generate uniform random variate between 1 and NoSrrData
      
      NoSrrData = lastyear-firstyear+1


      If (RecERR) then
        pos = IDINT(1d0+  (G05CAF(X)*dble(NoSRRData) ))
        BootStrapRecruit = Recruit*dexp(BSResid(pos))
      else
        BootStrapRecruit = Recruit
      endif

      return
      end
                                
      
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




c /////////////////////////////////////////////////////////////////////

      double precision Function CalcProSSB(iyear) ! Calculate SSB in a projection year

c /////////////////////////////////////////////////////////////////////
      
      include 'indat.inc'
      include 'projc3.inc'
      

      integer iyear, iage
      double precision SSB 

      SSB = 0d0

      
      do iage = 1, lastage-firstage +1
          SSB = SSB + NPro(iyear, iage)* MCMO(iyear,iage)*
     *       MCSW(iyear,iage) * dexp( - FPro(iyear,iage)*PF
     *                 - NMPro(iage)*PM )
      enddo
      
      if (SSB .eq. 0) then   ! some debugging code here 
        write(*,*) 'ERROR : SSB = ZERO in projection year ',
     *   iyear+lastyear
        pause
        write(*,*) 'N, SW, MO, F, NM, PF, PM: '
        do iage = 1, lastage-firstage+1
          write(*,*) iyear, NPro(iyear, iage),SWPro(iage),MOPro(iage),
     *    Fpro(iyear,iage), NMPro(iage), PF, PM
        enddo
      endif
      
      CalcProSSB = SSB

      return
      end


c /////////////////////////////////////////////////////////////////////

       Subroutine MCPROJ( VCV, MBAL ) 

c /////////////////////////////////////////////////////////////////////

c
c      Variance of projections by Monte-Carlo method
c
        implicit none
        include 'indat.inc'
        include 'Sepmodel.inc'
        include 'projc3.inc'
        include 'msgpro1.inc'

        double precision EPS, Z(maxparm), VCV(maxparm,maxparm)
        double precision R(11500)
        double precision G05DDF
        double precision MBAL, MeanF(MaxProYr), Yield(MaxProYr)
        character*77 text(10)

        integer NR, ifail, ItsToRun, Its , iyear, age
        integer ifleet, IC, i
        character*40 filename

        Ifail = 1
        EPS =   0.09/dble(Nxparm)
        NR = 11500
        IC = maxparm

c       Get required number of simulations

        call Screen_in_i(HW(17,Language),ItsToRun,10000,1,Language)


        if (NR .lt. (Nxparm+1)*(Nxparm+2)/2 ) then
          Text(1)=' '
          Text(2)=HW(18,Language)
          Text(3)=' '
          Call Screen_out_a(Text, 10, 3)
          stop
        endif

         
        Open(1,  status='UNKNOWN', file='Stock.mc',recl=6000)
        write(1,*) ItsToRun, NFleet, YtPro-1, MBAL, lastyear+1
        Open(2, status='UNKNOWN', file ='Recruits.mc',recl=6000)
        write(2,*) ItsToRun, NFleet, YtPro-1, MBAL, lastyear+1
        Open(3, status ='unknown', file ='MeanF.mc',recl=6000)
        write(3,*) ItsToRun, NFleet, YtPro-1, MBAL, lastyear+1
        Open(4, status ='unknown', file ='Yield.mc',recl=6000 )
        write(4,*) ItsToRun, NFleet, YtPro-1, MBAL, lastyear+1


        do ifleet = 1, Nfleet
          filename = 'Fmult'//char(ifleet+48)//'.mc'              ! adding 48 converts from ASCII code to a number 1,2,3..
          Open(14+ifleet,status='unknown',file=filename,recl=6000)
          write(14+ifleet,*) ItsToRun, NFleet, YtPro-1,MBAL,lastyear+1
          filename = 'Catch'//char(ifleet+48)//'.mc'
          Open(19+ifleet,status='unknown',file=filename,recl=6000)
          write(19+ifleet,*) ItsToRun,NFleet,YtPro-1,MBAL,lastyear+1
c          filename = 'Discard'//char(ifleet+48)//'.mc'
c          Open(14+ifleet,status='unknown',file=filename)
c          write(14+ifleet,*) ItsToRun,NFleet,YtPro-1,MBAL,lastyear+1
        enddo !   fleet

        
        call G05EAF (Xbest, NxParm, VCV, IC, EPS, R, NR, IFAIL) 


        if (IFAIL .ne. 0) then
          write(Text(1), '(I2)') IFAIL
          Call ConCat(Text(1), HW(19,Language), Text(1))
          Call Screen_out_a(text(1), 10, 1)
        endif


        DO ITS = 1, ItsToRun



        Ifail = 0

        call G05EZF (Z, NxParm, R, NR, Ifail)  ! Variability on Population params
        call SetMOSW( .true., YtPro)



        if (IFAIL .ne. 0) then
          write(Text(1), '(I2)') IFAIL
          Call ConCat(Text(1), HW(20,Language), Text(1))
          Call Screen_out_a(text(1), 10, 1)
        endif


        
        Call GenFMult(ftyear-lastyear+1, YtPro)
        Call Project(Z, YtPro, .true.)     ! YtPro+1


c       Write out the stock by year for this iteration


        write(1, 340) (Stock(iyear), iyear=2,YtPro)



c       Write out the recruitments


        write(2, 340) (Npro(iyear,1), iyear = 2,YtPro)



c       Write out the mean fishing mortality over reference ages

        Do iyear = 2, YtPro
          MeanF(iyear) = 0d0
            do age = LoFage, HiFage
              MeanF(iyear) = MeanF(iyear)+Fpro(iyear, age-firstage+1)
            enddo
            MeanF(iyear) = MeanF(iyear)/dble(HiFage-LoFage+1)
        enddo

        write(3, 340) (MeanF(iyear), iyear=2,Ytpro) 
        
340     format (4X, 25(E23.12))


c       Write out the total yield

        
        Do iyear = 2, YtPro
          Yield(iyear) = 0d0
          do ifleet = 1, Nfleet
            Yield(iyear) = Yield(iyear)+ FLCatch(ifleet,iyear)
          enddo ! fleets
        enddo   ! years

        write(4, 340) (Yield(iyear), iyear=2,YtPro)


c      Write the Fleet Fmults, Yields and discards


       do ifleet = 1,Nfleet
           write(14+ifleet,340)(FmultPro(ifleet,iyear),iyear=2,Ytpro)
           write(19+ifleet,340)(FLcatch(ifleet,iyear),iyear=2,Ytpro)
           write(19+ifleet,340)(Discards(ifleet,iyear),iyear=2,Ytpro)
       enddo



c        write(*,*) ' Iteration ',its

        ENDDO ! Iterations

        Close (1)
        Close (2) 
        Close (3)
        close (4)
        
        do i = 1,Nfleet
          Close (14+ i)   ! F-multiplier files
          Close (19+i)   ! Fleet catch files
c          close (14+i)   ! Discard files
        enddo
        return

        end


c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




c  //////////////////////////////////////////////////////////////

       Subroutine BotchSolve(xyear, LastFStch,LastFtrue)

c //////////////////////////////////////////////////////////////

c Solves from linear approximation and iteration until no catch
c is further than 0.01% from a catch constraint
      implicit none
      include 'indat.inc'
      include 'projc3.inc'
      
      include 'sepmodel.inc'
      include 'MSGPRO1.inc'

      integer Ndata, Nparm
      Double precision Tolerance (maxProFlt), LastFStch,LastFtrue
      double precision LastFref(2)
      character*77 text(10)
      character*10 ytext
      integer ifleet, iage, Xyear, assessment, simulated 
      double precision Z, PartF
      logical NotSolved
      integer UseF(maxProFlt)
      
      assessment = 1
      simulated = 2

      LastFref(assessment) = LastFtrue
      LastFref(simulated) = LastFStch

      CurrYear = xyear

      if ((CurrYear .le. 1) .or. (CurrYear .gt. YtPro)) then
        write(*,*) 'BotchSolve Error: year: ',Curryear
        write(*,*) 'Out of range 2 to ',Ytpro
        stop
      endif

      do ifleet = 1, Nfleet
        Tolerance(ifleet) = 0.1d-4
        FmultPro(ifleet, CurrYear) = 1d0
        if (TargetFM(ifleet,CurrYear) .lt. 0) then ! an F- constraint
          UseF(ifleet) = assessment
        else
          UseF(ifleet) = simulated
        endif
      enddo

      NotSolved = .True.


      do while (NOTSOLVED)

c     Work out the current F and project the population

      do iage = 1, lastage-firstage
         Fpro(CurrYear, iage)=0.0
         do ifleet = 1, Nfleet
            Fpro(CurrYear, iage)= Fpro(CurrYear, iage) 
     *      +LastFref(UseF(ifleet))*Select(iage)*CRatio(ifleet,iage)*
     *        FMultPro(ifleet, CurrYear) 
         enddo   ! fleets
         NPro(CurrYear+1, iage+1) = NPro(CurrYear,iage) *
     *     dexp( - FPro(CurrYear, iage) - NMPro(iage) ) 
      enddo
        
c     The plus-group

      FPro(CurrYear,lastage-firstage+1)=FPro(CurrYear,lastage-firstage)
      Npro(CurrYear+1,lastage-firstage+1)=
     *   NPro(CurrYear+1, lastage-firstage+1) +
     *   NPro(CurrYear, lastage-firstage+1)*
     *   dexp(-FPro(CurrYear, lastage-firstage+1)-
     *        NMPro(lastage-firstage+1))

c  Catches by fleet

      do ifleet = 1, NFleet     
        FLCatch(ifleet, CurrYear) = 0.0d0
        do iage = 1, lastage-firstage+1

          Z = FPro(CurrYear,iage) + NMPro(iage)
          PartF=LastFref(UseF(ifleet))*Select(iage)*
     *     CRatio(ifleet,iage)*
     *            FMultPro(ifleet, CurrYear)*Retention(ifleet,iage)
c          if (iage .ge. lastage-firstage) then
            FLCatch(ifleet, CurrYear) = FLCatch(ifleet, CurrYear)
     *         + NPro(CurrYear, iage) * PartF/Z * (1d0 - dexp(-Z))*
     *           CWPro(ifleet, iage)         
c          else 
c            FLCatch(ifleet, CurrYear) = FLCatch(ifleet, Curryear)
c     *         + (Npro(Curryear,iage) - Npro(CurrYear+1,iage+1))
c     *         * PartF/Z*CWPro(ifleet, iage)
c          endif
         enddo
      enddo

c Discards by Fleet

      do ifleet = 1, NFleet     


        Discards(ifleet, CurrYear) = 0.0
        do iage = 1, lastage-firstage+1
          Z = FPro(CurrYear,iage) + NMPro(iage)
          PartF = LastFref(UseF(ifleet))*Select(iage)*
     *     CRatio(ifleet,iage)*
     *        FMultPro(ifleet, CurrYear)*(1d0-Retention(ifleet,iage))
c          if (iage .ge. lastage-firstage) then
            Discards(ifleet, CurrYear) = Discards(ifleet, CurrYear)
     *         + NPro(CurrYear, iage) * PartF/Z * (1d0 - dexp(-Z))*
     *           DiscWt(ifleet, iage)         
c          else
c          Discards(ifleet, CurrYear) = Discards(ifleet, CurrYear) +
c     *         (NPro(CurrYear,iage) - Npro(CurrYear+1,iage+1))*       
c     *         PartF/Z*DiscWt(ifleet,iage)
c          endif
         enddo
      enddo


c   The residuals 

      NotSolved = .FALSE.

      do ifleet =1, NFleet
        If (FmultPro(ifleet, CurrYear) .ne. MaxFmult) then ! only check those fleets not F-limited
          IF (ConsCat(ifleet,Curryear) .ge. 0d0) then      ! and only those fleets not F-constrained
          If (dabs(FlCatch(ifleet, CurrYear)-ConsCat(ifleet,CurrYear))
     *            /ConsCat(ifleet,CurrYear)
     *       .gt. Tolerance(ifleet) ) NotSolved = .TRUE.
          endif
        endif
      enddo

c Update the  Fmults for the next loop by linear approximation

      do ifleet = 1, NFleet
        If (ConsCat(ifleet,CurrYear) .gt. 0d0 ) then                 ! it's a catch constraint
          FMultPro(ifleet, CurrYear) = FmultPro(ifleet, CurrYear) +
     *    FmultPro(ifleet, CurrYear)/FlCatch(ifleet, CurrYear) *
     *      (ConsCat(ifleet,CurrYear)-FlCAtch(ifleet, CurrYear))
          If (FmultPro(ifleet, CurrYear) .gt. MaxFMult) then             ! limits the F
              FmultPro(ifleet, CurrYear) = MaxFmult
          endif
        else                                                          ! it's an F-multiplier
          if (FMultPro(ifleet, CurrYear) .ne. 
     *            -ConsCat(ifleet,CurrYear)) then                     ! this to make sure the loop is done at least twice
              NotSolved = .TRUE.
              FmultPro(ifleet, CurrYear) = -ConsCat(ifleet,CurrYear)    
          endif
        endif
      enddo


      


      enddo  ! NOT SOLVED

      do ifleet =1,Nfleet
        if (FMultPro(ifleet, CurrYear)  .eq. MaxFmult) then
          write(ytext, '(F8.4)') MaxFmult
          Call Concat(Text(1),HW(21,Language),ytext)
          Call Concat(Text(1),Text(1),HW(22,Language))
          write(ytext, '(I3)') ifleet
          Call Concat(Text(1),Text(1),ytext)
          Call Concat(Text(1),Text(1),HW(23,Language))
          write(ytext, '(I4)') CurrYear+firstyear-1
          Call Concat(Text(1),Text(1),ytext)
          call screen_out_a(Text, 10, 1)

        endif
      enddo



      return
      end

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


c ////////////////////////////////////////////////////////////////////////

      Subroutine Project(X, iyear, RecERR) ! with X as far as iyear

c ///////////////////////////////////////////////////////////////////////
c
c     Do a single projection from the X vector of S, N and with 
c       options set in projection file, for succeeding iyears
c       with constant catch by fleet (ConsCatch) or F_multiplier
c
c     
c
c
c
c
      implicit none
      include 'indat.inc'
      include 'projc3.inc'
      include 'SRR.inc'
      include 'SepModel.inc'

      integer iyear, iage, parmno, year, i, jyear, ifleet, age, index 
      double precision X(maxparm)
      double precision LastFStch, LastFReal 
      double precision S(maxage)
      double precision EstdRecruit, PartF, Z, SSB
      double precision  CalcProSSB, BootStrapREcruit ! Functions
      double precision CAlcRecruit, CalcSSB 
      logical UseRec1, UseRec2, RecErr

c----------------------------------  Get the parameters out of X

      
      parmno = NySep


      LastFStch = dexp(X(parmno))+1d-6                      !  : last F in pseudo-data
      LastFreal = F(lastyear-firstyear+1,Refage-firstage+1) ! last F at reference age in the assessment

c---------------------------------     The selection pattern  

      If (TwoSel) parmno = parmno+ (lastage-firstage+1-3)

      do age= firstage, lastage-1
        iage = age-firstage+1
        if (age .eq. Refage) then
          S(iage) = 1.0
        else if (age .eq. lastage-1) then
          S(iage) = TermS
        else if ((age .ne. Refage) .and. (age .ne. lastage-1)) then
          parmno = parmno+1
          s(iage) = dexp(X(parmno))+1d-6
c          write(*,*) 'Debug : S ',parmno,age,S(iage)
        endif
      enddo

      S(lastage-firstage+1) = S(lastage-firstage)


      do iage = 1, lastage-firstage+1
        Select(iage) = S(iage) ! for the LSFUN1
      enddo



c----------------------------------     Starting Populations in lastyear

      do age = firstage, lastage -1
        iage = age-firstage+1
        parmno = parmno +1
        Npro(1,iage) = dexp(X(parmno)) + 1d0 
      enddo  

c ---------------------------------  Project to 1 Jan on lastyear+1
      
      FPro(1,1) = LastFreal*S(1)
      do age = firstage+1, lastage 
        iage = age-firstage+1
        FPro(1, iage) = LastFReal*S(iage)
        Npro(2,iage) = NPro(1,iage-1)*dexp(-FPro(1,iage-1)-
     *                 NMPro(iage-1))
      enddo  

      FPro(1,lastage-firstage+1) = LastFReal*S(lastage-firstage)

c------------------------------------     The plus-groups in lastyear+1

      Npro(1, lastage-firstage+1) = 
     * (N(lastyear-firstyear+1,lastage-firstage+1))          

      Npro(2,lastage-firstage+1)=NPro(1,lastage-firstage+1)*
     *   dexp(-FPro(1,lastage-firstage+1)-NMPro(lastage-firstage+1))
     *   + Npro(1,lastage-firstage)*
     *   dexp(-FPro(1,lastage-firstage)-NMPro(lastage-firstage))

c----------------------------------     Estimates of numbers at last true age in previous years are 
c                                       skipped over in parameter list 

      do year = lastyear-NySep+1, lastyear-1
        parmno=parmno+1
      enddo

c    Decide whether to use the recruitment in lastyear

      UseRec1 = .false.
      do index = 1, nageix
        if ((fage(index).eq.firstage).and.(lyear(index).ge.lastyear))
     *  then
        if (Aindex(index,lastyear-fyear(index)+1,1) .gt. 0d0) 
     *    UseRec1 = .true.
        endif
      enddo


      UseRec2 = .false.
      do index = 1, nageix
        if ((fage(index).eq.firstage).and.(lyear(index).eq.lastyear+1)
     *    .and. (Aindex(index,lastyear+1-fyear(index)+1,1) .gt. 0d0))    
     *     UseRec2 = .true.
      enddo

      If (UseRec2) then
        parmno = Parmno+1
        EstdRecruit = dexp(X(Parmno))  ! the incoming recruitment in lastyear+1
c        write(*,*) 'Parmno , recruits', parmno, EstdRecruit  ! debug
      endif

c---------------------     Not interested in the catchabilities etc.  here: 


c---------------------     All parameters needed are now copied out of the X vector


c     First project from lastyear to lastyear +1
c     This needs to be handled differently from other years as there may be an estimate
c     of recruitment from a survey made in a year after the last year of catch data,
c     which is the first year for the projection (YtPro=2).
c
c
c     Note that lastyear = last year for which there are catch data
c               YtPro    = Number of years to project, which
c                          starts at lastyear (=last year of catch at age data)
c                so that N(lastyear) = NPro(1)

c      year = lastyear
      
c      do iage = 1, lastage-firstage  
c
c        Npro(1,iage+1)=N(lastyear-firstyear+1,iage)*
c     *   dexp(-FPro(1,iage)-
c     *   NM(lastyear-firstyear+1,iage))
c      enddo

c ------------------------------------------     do the plus-group

      FPro(1, lastage-firstage+1) = 
     *   FPro(1, lastage-firstage)

       NPro(1,lastage-firstage+1) = 
     *  N(lastyear-firstyear+1,lastage-firstage+1)*
     *  dexp(-FPro(1,lastage-firstage+1)- 
     *        NM(lastyear-firstyear+1,lastage-firstage+1)) 
     * + N(lastyear-firstyear+1,lastage-firstage)*
     *  dexp(-FPro(1,lastage-firstage)- 
     *        NM(lastyear-firstyear+1,lastage-firstage)) 
     
      Stock(1) = CalcProSSB(1)

c----------------------------------------------     Projections for the succeeding years

      do jyear = 1, iyear  ! +1                     ------ years to do the projections
        
c---------------------------- Recruitment

        if ( (jyear - lag) .le. 0) then
          SSB = CalcSSB(lastyear+jyear-lag) !---------- SSB from the ICA analysis
        else
          SSB = CalcProSSB(jyear-lag)                 !------ SSB from the projections
        endif

c        UseRec1 = .true.        !----------- Fixed during meeting
c        Userec2 =.true.
        if (jyear .eq. 1) then 
           if (UseRec1) then
             Npro(jyear,1) = N(Lastyear-firstyear+1,1)
           else
             Npro(jyear,1)=BootStrapRecruit(SSB,lastyear+jyear-1,
     *                                                         RecERR)
           endif
        else if (jyear .eq. 2) then
           if (UseRec2) then
              Npro(jyear,1) = EstdRecruit
           else
              Npro(jyear,1) = BootStrapRecruit(SSB, lastyear+jyear-1, 
     *                                                         RecERR)
           endif
        else if (jyear .gt. 2) then
          Npro(jyear,1) = BootStrapRecruit(SSB,lastyear+jyear-1,RecERR)
        endif

        if (jyear .gt. 1) then
          Call BotchSolve(jyear, LastFStch, LastFreal)
        endif


c----------------------------- Stock   

        Stock(jyear) = CalcProSSB(jyear) 

      enddo
      
      
      return 
      end



c /////////////////////////////////////////////////////////////////////
c
      Subroutine FitNewSRR
c
c ////////////////////////////////////////////////////////////////////
      implicit none
      include 'indat.inc'
      include 'sepmodel.inc'
      include 'srr.inc'
      include 'projc3.inc'
      include 'msgpro1.inc'


 
      character*1 Curve, Auto
      character*3 ytext
      character*77 text(10)

      double precision AX(8), Alow(8), Ahigh(8), WX(500), SSQ 
      integer iw(12), i, nparm, ibound, liw, lw, ifail,nosrrdata
      double precision StockSize(maxyear), RecruitNos(maxyear)

      NoSrrData= lastyear-firstyear+1








      Ifail = 1

      do while ( (Ifail .ne. 0) .and. (Ifail .ne. 5)) ! ie keep repeating until the minimisation is successful

      If (Ifail .ne. 99) then
        Text(2)=HW(3,Language)
        Text(3)=' '
        Text(1)=Text(3)
        Call Screen_OUT_a(Text,10,3)
      endif

      Curve = 'X'
      do i=5,11
        Text(i-4)=HW(i,language)
      enddo

c     READ THE TYPE OF SRR MODEL

      Call Screen_out_a(Text,10,7)
      Call Screen_in_a(HW(12,Language),Curve,KW(1,Language),Language)

c     CHOOSE YEAR RANGE FOR SRR MODEL

      lychosen = -1
      fychosen = 1
      do while (lychosen .le. fychosen)
       Call Screen_in_i(HW(13,Language),fychosen,lastyear-3,
     *                                             firstyear,Language)
       Call Screen_in_i(HW(14,Language),lychosen,lastyear+1,
     *                                           firstyear+1,Language)
        if (lychosen .le. fychosen) then
          Text(1)=' '
          Text(2)=HW(15,Language)
          Text(3)=' '
          call screen_out_a(Text, 10,3)
        endif
      enddo

c     ASK IF ERRORS TO BE AUTOCORRELATED

      Call SCreen_in_a(HW(16,Language),Auto,KW(2,Language),Language)

c     Set the constraints accordingly      

      do i = 1,8  ! initially all unconstrained
        Alow(i) = 0d0
        Ahigh(i) = 1d20 
      enddo


      if (Scan(curve,'Ss') .ne. 0) then  ! Shepherd
        Alow(2) = 0d0
        Ahigh(2) = 0d0
        Alow(6) = 0d0
        Ahigh(6) = 0d0
      else if (Scan(curve,'Bb') .ne. 0) then ! BevHolt
        Alow(2) = 0d0
        Ahigh(2) = 0d0
        Alow(4) = 1d0
        Ahigh(4) = 1d0
        Alow(6) = 0d0
        Ahigh(6) = 0d0
      else if (Scan(curve,'Rr') .ne. 0) then ! Ricker
       Alow(3) = 1d0
       Ahigh(3) = 1d0
       Alow(4) = 0d0
       Ahigh(4) = 0d0
       Ahigh(6) =0d0
       Alow(6) = 0d0
      else if (Scan(curve,'NnOo') .ne. 0) then ! Mean only
       Alow(1) = 0d0
       Ahigh(1) = 0d0
       Alow(2) = 0d0
       Ahigh(2) = 0d0
       Alow(3) =  1d0
       Ahigh(3) = 1d0
       Ahigh(4) = 0d0
       Ahigh(4) = 0d0
      endif

      If (Scan(Auto, 'Ii') .ne. 0) then ! no autocorrelation
       Alow(5) = 0d0
       Ahigh(5) = 0d0
      else
       Alow(5) = -1d0
       Ahigh(5) = 1d0
      endif

c     get the vectors of Rec and SSB to be used

      Call GetSRR(StockSize, RecruitNos, NoSRRdata)

c     Put in some starting values for AX

      AX(1) = 0d0
      do i = 1, NoSrrData
        AX(1) = AX(1) + (RecruitNos(i)/StockSize(i)) 
      enddo  

      AX(1) = AX(1)/dble(NoSrrData)   ! mean recruit per spawning stock

      AX(6) = 0d0
      AX(3) =0d0
      do i = 1, NoSrrData
        AX(6) = AX(6) + dlog(RecruitNos(i))   ! mean recruits
        AX(3) = AX(3) + StockSize(i)    ! mean SSB
      enddo

      AX(6) = dexp(AX(6)/dble(NoSrrData))  
      AX(3) = AX(3)/dble(NOSrrData)
      

      AX(2) = 1d-6                     ! Ricker exp.
                        
      AX(5) = 0d0                     ! Autocorrelation
      AX(4) = 1d0                       

      do i =1,6 ! put the constraints on the starting values
        if (AX(i) .lt. Alow(i)) AX(i) = Alow(i)
        if (AX(i) .gt. Ahigh(i)) AX(i) = Ahigh(i)
      enddo


c LOG TRANSFORM THE PARAMS 

      do i =1,6
       if (i .ne. 5) then
        AX(i) = dlog(AX(i) + 1d-8)
        Alow(i) = dlog(Alow(i) + 1d-8)
        Ahigh(i) = dlog(Ahigh(i) + 1d-8)
       endif

       if (Alow(i) .eq. Ahigh(i)) Ahigh(i) = Alow(i)+ 1d-9

      enddo


c     Calculate the slope for Ockham model

c     First find the lowest SSB observation
      
      AX(8) = 1d20
      do i = 1, NoSrrData
        if (StockSize(i) .lt. AX(8)) AX(8)=StockSize(i)
      enddo  

      AX(8) = dlog(AX(8))       ! AX(8) is log lowest observed SSB

      if (Scan(curve,'oO') .ne. 0) then
         AX(7) = 0d0
         do i =1,NoSrrData
           AX(7) = AX(7) + (dlog(RecruitNos(i)) - AX(6))*
     *         (dlog(RecruitNos(i)) - AX(6))
         enddo
         AX(7) = SQRT( AX(7)/ dble(NoSrrData) )
      else
        AX(7) = 99d0  ! marker value: not using Ockham model
      endif
      Ahigh(7) = AX(7) + 1d-9
      Alow(7) = AX(7) - 1d-9
      Ahigh(8) = AX(8) + 1d-9
      Alow(8) = AX(8) - 1d-9

      

c     Do the minimisation if necessary


      Ifail = -1

      Nparm = 8
      Ibound = 0
      LIW = 12
      LW = 130


       if (Scan(curve,'NnOo') .eq. 0) then ! ie not if only mean or Ockham
        Call E04JAF(Nparm,Ibound,Alow,Ahigh,AX,SSQ,IW,LIW,WX,LW,Ifail)

        write(ytext, '(I3)') Ifail
        Call Concat(Text(1),HW(24,Language),ytext)
        Call Screen_out_a(text,10,1)
       else  
        Ifail = 0 
       endif 
       
       enddo

       Call Funct2(Nparm,AX) ! puts the residual vector in common memory

       
       do i =1,8
         SRRParm(i) = AX(i)  ! puts the parameters in common
       enddo


       return
       end ! of routine Fit New SRR


c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


c //////////////////////////////////////////////////////////////////////
c
      Subroutine FUNCT2(NP,XC)        
c      
c //////////////////////////////////////////////////////////////////////

       ! As funct1 except that it returns a residual vector in BSRESID 

      implicit none
      include 'indat.inc'
      include 'srr.inc'
      include 'projc3.inc'

      integer NP,NoSRRdata
      double precision XC(8), FC(maxyear), resid
      double precision StockSize(maxyear), RecruitNos(maxyear)
      double precision predRect
      double precision SRRFUNCT
      integer i      
      
      NoSrrdata = lastyear-firstyear+1

      Call GetSRR(StockSize, RecruitNos,NoSRRdata)

      do i = 1, NoSrrData
        PredRect = SRRFunct(XC, StockSize(i))
         if ((i .gt. 1) .and. (dabs(XC(5)) .gt. 1d-8)) then
           resid = RecruitNos(i-1)/SRRFunct(XC,StockSize(i-1))
           resid = dlog(resid)
           PredRect = Predrect*dexp(XC(5)*resid )
         endif
         resid = (RecruitNos(i)/PredRect)
         resid = dlog(resid)
         BSResid(i) = resid
      enddo

        
      return
      end

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c //////////////////////////////////////////////////////////////////////
c
      Subroutine FUNCT1(NP,XC,FC)        
c      
c //////////////////////////////////////////////////////////////////////

      implicit none
      include 'indat.inc'
      include 'srr.inc'
      include 'projc3.inc'

      integer NP, NoSRRData
      double precision XC(8), FC, resid
      double precision StockSize(maxyear), RecruitNos(maxyear)
      double precision predRect
      double precision SRRFUNCT
      integer  i      
      
      NoSrrdata = lastyear-firstyear+1

      Call GetSRR(StockSize, RecruitNos,NoSRRdata)

      FC = 0d0

      do i = 1, NoSrrData
        PredRect = SRRFunct(XC, StockSize(i))
         if ((i .gt. 1) .and. (dabs(XC(5)) .gt. 1d-8)) then
           resid = RecruitNos(i-1)/SRRFunct(XC,StockSize(i-1))
           resid = dlog(resid)
           PredRect = Predrect*dexp(XC(5)*resid) 
         endif
         resid = (RecruitNos(i)/PredRect)
         resid = dlog(resid)
         FC = FC+ (resid*resid)
      enddo

        
      return
      end

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



c ////////////////////////////////////////////////////////////////////

      Double Precision Function SRRFUNCT(XJ,SSB)

c////////////////////////////////////////////////////////////////////
c  
c    Returns predicted Recruitment for given SSB
c
      double precision X(8), XJ(8), SSB, recr, G05DEF, AVrec, Slope
      integer i

c    
c       X(6) is mean of log recruitments
c       X(7) is standard deviation of log recruitments
c       X(8) is lowest observed SSB 
c
c
      do i = 1,4
        X(i) = dexp(XJ(i))- 1d-8
      enddo
        if (XJ(7) .gt. 90d0) then ! not ockham model fitted
        if (dexp(XJ(6)) .lt. 1d0) then
          recr = (X(1)*SSB)
          recr = recr/(1d0+ SSB/X(3))**X(4)* dexp(-X(2)*SSB)
        else
          recr = dexp(XJ(6))
        endif
      else  ! Ockham model
c        write (*,*) 'SRR : OCKHAM '
        if (SSB .gt. dexp(XJ(8)) ) then
          recr = dexp(XJ(6))
        else
          AvRec = G05DEF( XJ(6), XJ(7) )
          slope = AvRec/ dexp(XJ(8))
          recr = SSB*slope
        endif
      endif

      

      srrfunct = recr+ 1d-6 ! so that recruits are never zero
      

      return
      end

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



c ///////////////////////////////////////////////////////////////////////

       Double precision function SQSAFE(X)

c ///////////////////////////////////////////////////////////////////////

      double precision X, zero

      zero = -1d-9


      if (X .le. zero) then
        write(*,*) 'Error attempting to take square root of ',X
        SQSAFE = 0d0
      else
        SQSAFE = DSQRT(dabs(X))
      endif

      return

      end



c //////////////////////////////////////////////////////////////////////

      Subroutine SetMOSW ( doMC, endyear )

c /////////////////////////////////////////////////////////////////////
      implicit none
      include 'indat.inc'
      include 'projc3.inc'
      

      integer iyear, iage, endyear
      logical doMC 

      double precision G05DEF, G05DDF   ! NAG random number generators
      double precision value


      If ( .not. doMC) then
      
      do iyear = 1, endyear
        do iage = 1, lastage-firstage+1
          value = DSIN(MOPro(iage))
          MCMO(iyear,iage) = value*value
          MCSW(iyear,iage) = dexp(SWPro(iage))
        enddo
      enddo

      else

      do iyear = 1, endyear
        do iage = 1, lastage-firstage+1
          value = G05DDF(MoPro(iage), Varmat(iage))  ! normal random variate
          value = DSIN(value)
          MCMO(iyear,iage) = value*value
          MCSW(iyear,iage) = G05DEF(SWPro(iage), VarSW(iage)) ! lognormal variate
        enddo
      enddo

      endif

      return
      end






c //////////////////////////////////////////////////////////////////////////

        Subroutine GenFMult(startsim, endsim) ! Generate stochastic F constraint matrix
                                          ! using the data in TargetFM and CVTarg
                                          ! note the startsim and endsim referenced
                                          ! so that lastyear = 1

c /////////////////////////////////////////////////////////////////////////
        
        implicit none
        include 'indat.inc'
        include 'Projc3.inc' 

        integer iyear, ifleet, startsim, endsim
        double precision expectation, G05DEF

        do ifleet = 1,Nfleet
          do iyear = startsim, endsim
            expectation = dlog( dabs(dble(TargetFM(ifleet,iyear)))) +
     *      ( (CVTarg(ifleet,iyear)*CVTarg(ifleet,iyear))/2d0 ) 
            ConsCat(ifleet, iyear) = 
     *         G05DEF(expectation,dble(CVTarg(ifleet,iyear)))
            if (TargetFM(Ifleet,iyear) .lt. 0d0) then
              ConsCat(ifleet,iyear) = -ConsCat(ifleet,iyear)
            endif
          enddo ! years
        enddo  ! fleets

        return
        
        end ! of subroutine GenFmult

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


c ///////////////////////////////////////////////////////////////////////        
        
       Subroutine  OverWritePop

c /////////////////////////////////////////////////////////////////////
c    
c       Allows the user to over-write certain populations
c       parameters and parameter estiamtes
c
c
        implicit none
        include 'indat.inc'
        include 'sepmodel.inc'

        integer i, year, age
        double precision input_N, NEW_VALUE
        logical error

        character*1 dummy
        dummy = ' '

        do while (Scan(dummy, 'yYnN') .eq. 0)
          write (*,
     *   '( ''  Change any of the populations (Y/N) ? --> '')') ! \
          read (*,'(A)') dummy 
          if (Scan(dummy, 'yYnN') .eq. 0) then
            write(*,*) ' ERROR: RETRY'
          endif
        enddo

        if (Scan(dummy, 'nN') .eq. 0) then 
           write(*,*) ' New populations: -1,-1,-1 to finish'
           year = 1
           error = .false.
           do while (year .gt. 0)
             write(*,*)' Enter year, age, new population'
             read (*,*) year, age, input_N 
             if (year .gt. 0) then
               if ((year .gt. lastyear+1).or.(year.lt.firstyear)) then
                 write (*,*) 'YEAR ERROR '
                 error = .true.
               endif  ! year ok
               if ((age .gt. lastage) .or.(age.lt. firstage)) then
                 write (*,*) 'AGE ERROR '
                 error = .true.
               endif  ! age of
               if (.not. error .and. (year .gt. 0)  ) then
                 N(year-firstyear+1,age-firstage+1) = input_N
               endif  ! use the value
             endif  ! year ok
           enddo  ! read next value
           
           
           write(*,*) '  New parameters: -1, -1 to finish'
           i = 10
           write(*,*) '  Parameter number, new value [not log] '
           do while (i .gt. 0)
             read(*,*) i, new_value
             if (i .ge. 0) then
               Xbest(i) = dlog(new_value)
             endif ! a non-zero i value
           enddo  ! next parameter

        endif  ! chosen to overwrite
        return
        end

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



c //////////////////////////////////////////////////////////////////////

       Subroutine GetPercentiles(NoPercentiles, Percentile)

c //////////////////////////////////////////////////////////////////////
c
c  Routine sets up number of percentiles and the percentiles to
c     be plotted 
c
c
      implicit none
      integer NoPercentiles, i
      double precision percentile(NoPercentiles)
      character*1 response
      integer tscan
      include 'msgpro1.inc'

      Call Screen_in_a(HW(25,Language),response,KW(3,Language),
     * Language)

      if (Tscan(response, KW(4,Language)) .ne. 0) then ! use default values
        NoPercentiles = 5
        Percentile(1) =5d0
        Percentile(2) =25d0
        Percentile(3) = 50d0
        Percentile(4) =75d0
        Percentile(5) = 95d0
      else
        Call Screen_in_i(HW(26,Language),NoPercentiles,10,
     *     3,Language)
        do i = 1, NoPercentiles
          Call Screen_in_r(HW(27,Language),Percentile(i),99d0,1d0,
     *       Language)
        enddo
      endif
c
c    sort, in case where not entered in ascending order
c

      Call Shell(NoPercentiles,Percentile)


      return

      end



c //////////////////////////////////////////////////////////////////////

      Subroutine CreatePBYfiles(NFleets, NP, P, SP)

c///////////////////////////////////////////////////////////////////////

      character*30 filename1,filename2,filename3,filename4,
     *  filename5, Title1, Title2, Title3
c
c    NP - no of percentiles
c     P - percentiles
c    SP - size of percentile vector
c
      integer ifleet, Nfleets, SP, NP
      double precision P(SP)


      Call ReadAndSortStock('stock.mc','stock.pby','Stock',
     *                                    .true., NP, P, SP)
      Call ReadAndSortStock('recruits.mc','recruits.pby',
     *                        'Recruits',.false., NP, P, SP)
      Call ReadAndSortStock('MeanF.mc','MeanF.pby',
     *                           'MeanF',.false., NP, P, SP)
      Call ReadAndSortStock('Yield.mc','Yield.pby',
     *                           'Yield',.false., NP, P, SP)

      do ifleet= 1, Nfleets
        filename1='Fmult'//char(48+ifleet)//'.mc'
        filename2='Fmult'//char(48+ifleet)//'.pby'
        Title1 = 'F-multiplier for fleet '//char(48+ifleet)
        filename3='Catch'//char(48+ifleet)//'.mc'
        filename4='Catch'//char(48+ifleet)//'.pby'
        filename5='Discard'//char(48+ifleet)//'.pby'
        Title2='Catches by Fleet '//char(48+ifleet)
        Title3='Discards by Fleet '//char(48+ifleet)
        Call ReadAndSortStock(filename1,filename2,Title1, .false.,
     *   NP,P,SP)
        Call ReadAndSortCatch(filename3,filename4,Title2, 
     *      filename5, NP, P, SP)
      enddo

      return
      end

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++








c ///////////////////////////////////////////////////////////////////////
  
      SUBROUTINE SHELL(N, ARR)

c ////////////////////////////////////////////////////////////////////////
c
c  From page 229 of Numerical Recipes
c
c
      implicit none
      integer M,N, NN,K,L,I,J, LogNB2
      Double precision ALN2I, Tiny
      Double precision Arr(N), T


      Parameter (ALN2I = 1./0.69314718, TINY=1d-5)
      
      LognB2=IDINT(Dlog(Dble(N))*ALN2I + Tiny)
      M=N
      Do NN=1, LognB2
        M =M /2
        K=N-M
        Do j=1,k
          i=j
3         continue
          l=i+m
          if (ARR(L) .lt. Arr(i)) then
            T=Arr(i)
            Arr(i)=Arr(l)
            Arr(l)=t
            i=i-m
            if (i .ge. 1) goto 3
          endif
        enddo
      enddo
      return
      end

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




c //////////////////////////////////////////////////////////////////////

      Subroutine ReadAndSortStock( Infile, Outfile, Title, Calc_RISK,
     *   Nopercentiles, Percentile, PSIze)

c //////////////////////////////////////////////////////////////////////
c
c     Reads an .MC file, sorts iterations and creates .PBY file
c
       implicit none 
       include 'indat.inc'
       include 'projc3.inc'
       character*(*) Infile, OutFile, Title
       logical Calc_RISK
       integer NoPercentiles, PSize
       Double Precision Percentile(PSize), MBAL

       integer StartYear,  NoFleets, IterationsRun
       integer itn, iyr, j, year

       real*4 Data(MaxProYr, 2000)
       real*4 Mean(MaxProYr), MBALProb(MaxProYr)
       double precision Vector(2000)
       real*4 fractile(MaxProYr,NoPercentiles)


       Open(11, File=Infile, Status='old',recl=6000)
       Open(12, File=Outfile, Status='unknown',recl=6000)


       read(11, *) IterationsRun,NoFleets, YtPro, MBAL, StartYear
       write(*,*) Infile, IterationsRun
       do itn=1,IterationsRun

         read(11,*) (Data(iyr,itn), iyr=1,YtPro)

       enddo
       close(11)

c
c      do the sorting, year by year
c
       do iyr = 1, YtPro
         do itn=1,IterationsRun
           Vector(itn) = Data(iyr,itn)
         enddo
         Call SHELL(IterationsRun, Vector)

         if (Calc_Risk) then ! find the proportion above & below MBAL by year
           itn= 1
           do while ((Vector(itn).lt. MBAL).and.
     *                                   (itn.lt.IterationsRun))
             itn=itn+1
             MBALPROB(iyr) = float(itn)/float(iterationsRun)
           enddo
         endif


         do j =1, NoPercentiles
           Fractile(iyr,j) =
     *      Vector(1 + INT(DBLE(IterationsRun)*Percentile(j)/100d0))
         enddo
       enddo



       ! the expectation of distribution


       do iyr= 1, YtPro
         Mean(iyr) = 0d0
         do itn = 1, IterationsRun
           Mean(iyr) = Mean(iyr)+Data(iyr,itn)
         enddo
         Mean(iyr) = Mean(iyr)/Dble(IterationsRun)
       enddo

       write(12,'(A)') Title
       write(12, '(A)') 'Percentile'
       write(12,'(1X,I6,5X,I6,5X,I6)') startyear, startyear+YtPro-1,
     *  nopercentiles
       write(12, 100) (year, year=startyear, startyear+YtPro-1)
100    format(16X,120(5X,I5,5X))
       do j =1,NoPercentiles
         write(12,200) Int(Percentile(j)),(Fractile(iyr,j),
     *      iyr=1,YtPro-1)
       enddo
200    format(' ', 3X,I3,9X,120(1X,E14.6))
       write(12,300) (Mean(iyr), iyr=1,YtPro-1)
300    format(' Mean           ',120(1X,E14.6))
       if (Calc_Risk) then
         write(12,'(A)') 'P (SSB < MBAL) '
         do year = startyear, startyear+YtPro-1
           write(12,400) year, MBalProb( year-startyear+1)
         enddo
       endif
400    format(5X,I5,5X, E15.6)


       close(11)


       return
       end ! of ReadAndSortStock

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


c //////////////////////////////////////////////////////////////////////

      Subroutine ReadAndSortCatch( Infile, Outfile1,Title,Outfile2,
     *   Nopercentiles, Percentile, PSIze)

c //////////////////////////////////////////////////////////////////////
c
c     Reads an .MC file, sorts iterations and creates .PBY file
c      for catch and discards
c
       implicit none 
       include 'indat.inc'
       include 'projc3.inc'
       character*(*) Infile, OutFile1, Outfile2, Title
       logical Calc_RISK
       integer NoPercentiles, PSize
       Double Precision Percentile(PSize), MBAL

       integer StartYear,  NoFleets, IterationsRun
       integer itn, iyr, j, year

       real*4 Catch(MaxProYr, 2000)
       real*4 discarded(MaxProYr, 2000)
       real*4 MeanCatch(MaxProYr), MeanDiscard(MaxProYr),
     *  MBALProb(MaxProYr)
       double precision Vector(2000)
       real*4 FractileCatch(MaxProYr,NoPercentiles)
       real*4 FractileDiscard(MaxProYr,NoPercentiles)

       Open(11, File=Infile, Status='old',recl=6000)
       Open(12, File=Outfile1, Status='unknown',recl=6000)
       Open(13, File=Outfile2, Status='unknown',recl=6000)

       read(11, *) IterationsRun,NoFleets, YtPro, MBAL, StartYear

       do itn=1,IterationsRun
         read(11,*) (Catch(iyr,itn), iyr=1,YtPro)
         read(11,*) (discarded(iyr,itn), iyr=1,YtPro)
       enddo
       close(11)

c
c      do the sorting, year by year
c
       do iyr = 1, YtPro
         do itn=1,IterationsRun
           Vector(itn) = Catch(iyr,itn)
         enddo


         Call SHELL(IterationsRun, Vector)
         do j =1, NoPercentiles
           FractileCatch(iyr,j) =
     *      Vector(1 + INT(DBLE(IterationsRun)*Percentile(j)/100d0))
         enddo
       enddo

       do iyr = 1, YtPro
         do itn=1,IterationsRun
           Vector(itn) = discarded(iyr,itn)
         enddo
         Call SHELL(IterationsRun, Vector)
         do j =1, NoPercentiles
           FractileDiscard(iyr,j) =
     *      Vector(1 + INT(DBLE(IterationsRun)*Percentile(j)/100d0))
         enddo
       enddo


       ! the expectation of distribution


       do iyr= 1, YtPro
         MeanCatch(iyr) = 0d0
         do itn = 1, IterationsRun
           MeanCatch(iyr) = MeanCatch(iyr)+Catch(iyr,itn)
         enddo
         MeanCatch(iyr) = MeanCatch(iyr)/Dble(IterationsRun)
       enddo

       do iyr= 1, YtPro
         MeanDiscard(iyr) = 0d0
         do itn = 1, IterationsRun
           MeanDiscard(iyr) = MeanDiscard(iyr)+Discarded(iyr,itn)
         enddo
         MeanDiscard(iyr) = MeanDiscard(iyr)/Dble(IterationsRun)
       enddo

       write(12,'(A)') Title
       write(12, '(A)') 'Percentile'
       write(12,'(1X,I6,5X,I6,5X,I6)') startyear, startyear+YtPro-1,
     *  nopercentiles
       write(12, 100) (year, year=startyear, startyear+YtPro-1)
100    format(15X,120(5X,I5,5X))
       do j =1,NoPercentiles
         write(12,200) int(Percentile(j)),(FractileCatch(iyr,j),
     *    iyr=1,YtPro-1)
       enddo
200    format(' ',3X,I3,9X,120(1X,E14.6))
       write(12,300) (MeanCatch(iyr), iyr=1,YtPro-1)
300    format(' Mean           ',120(1X,E14.6))

       close(12)

       write(13,'(A)') Title
       write(13, '(A)') 'Percentile'
       write(13,'(1X,I6,5X,I6,5X,I6)') startyear, startyear+YtPro-1,
     *  nopercentiles
       write(13, 100) (year, year=startyear, startyear+YtPro-1)
       do j =1,NoPercentiles
         write(13,200) int(Percentile(j)),(FractileDiscard(iyr,j),
     *    iyr=1,YtPro-1)
       enddo
       write(13,300) (MeanDiscard(iyr), iyr=1,YtPro-1)

       close(13)


       return
       end ! of ReadAndSortStock

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++









