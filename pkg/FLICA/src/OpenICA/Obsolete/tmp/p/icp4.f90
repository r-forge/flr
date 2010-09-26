!      WINAPP 9000000,9000000                                                                                                       
                                                                                                                                   
!                                                                                                                                  
                                                                                                                                   
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
                                                                                                                                   
!      UNIT  ICPROJ : Stock projection with variance estimates                                                                     
                                                                                                                                   
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
!/////////////////////////////////////////////////////////////////////                                                             
!                                                                                                                                  
       Program NproS ! NPRO2 extended for management simulations                                                                   
!                                                                                                                                  
! ///////////////////////////////////////////////////////////////////                                                              
!                                                                                                                                  
                                                                                                                                   
      IMPLICIT NONE                                                                                                                
!                                                                                                                                  
!     Forwards projection of ICA estimated populations and parameters                                                              
!       This programme uses a routine which does a multifleet stock projection                                                     
!       in a conventional manner, starting with the populations in the last                                                        
!       ICA2 fit (in which a stock-recruit relation must have been fitted).                                                        
!     Selection pattern and F-multipliers by fleet and year must be provided in a                                                  
!     file to the programme at run-time, named the 'projection file'.                                                              
!       The stock is projected forwards with new recruitments drawn from the                                                       
!       fitted stock-recruit relation. Catches by fleet are estimated using the                                                    
!       weights at age by fleet in the projection file.                                                                            
!     The routine generates a single 'core' projection, which would be the                                                         
!     deterministic projection from the fitted parameters.                                                                         
!       Variances about the core projection are calcualted by the delta method.                                                    
!     This relies on calculating a matrix of the first derivatives of the projection                                               
!     variables (future catches by fleet and stock sizes) with respect to each of                                                  
!     the relevant parameters fitted in the ICA2 programme and for which the                                                       
!     variance-covariance matrix has been calculated.                                                                              
!       Variances of the projected catches and stock sizes can be calculated                                                       
!       from the variance-covariance matrix and from the matrix of derivatives.                                                    
!                                                                                                                                  
!     Given the estimate of future stock size with attached variance and a defined                                                 
!     minimum acceptable stock size it is then a simple matter to calcualte the risk of the                                        
!     stock falling below a certain size under assumptions of normality.                                                           
!                                                                                                                                  
!                                                                                                                                  
!         Programme Calls  :   Readblock          reads ICA data and parameters                                                    
!                              ReadVCV            read variance-covariance matrix                                                  
!                              ReadProjFile       read projection file                                                             
!                              ChkCatRatio        check catch ratios & round up                                                    
!                              SetProjVars        calculate mean Wts, M, ogives etc for projection                                 
!                              Project            does the projection calculation                                                  
!                              ReadMBAL           read minimum allowed SSB                                                         
!                              S15ADF             NAG routine for cum. normal distribution                                         
!                                                                                                                                  
!                                                                                                                                  
!           Files created:    Project.pop         Projected populations and catches                                                
!                             ICA.PRO             Projected stock and catch with variance estimates                                
!                                                 and risk of stock falling below MBAL                                             
!                             Derivs.tst          matrix of derivatives,     d(Stock or catch estimate in year)                    
!                                                                            ---------------------------------                     
!                                                                                d(ICA parameter estimate)                         
!                                                                                                                                  
!                                                                                                                                  
      INCLUDE 'INDAT.INC'                                                                                                          
      INCLUDE 'SEPMODEL.INC'                                                                                                       
      INCLUDE 'STATS.INC'                                                                                                          
      INCLUDE 'SRR.INC'                                                                                                            
      INCLUDE 'PROJC3.INC'        ! the projected populations etc.                                                                 
      include 'MESSAGE1.INC'                                                                                                       



!     Local variables                                                                                                              

      character*1 auto, curve
      integer psize                                                                                                                
      parameter (psize=10)                                                                                                         
      double precision VCV(maxparm,maxparm)  ! ------------------ Variance-Covariance matrix                                       
      integer iyear, i, jyear, iage, parmno, age                                                                                   
      integer ifleet                                                                                                               
      integer noPercentiles                                                                                                        
      double Precision Percentile(psize)                                                                                           
      character*1 dummy                                                                                                            
      character*5 ast                                                                                                              
      double precision lastf, MBAL                                                                                                 
                                                                                                                                   
      double precision S(maxage) !-------------------------- selection pattern                                                     
                                                                                                                                   
      integer tscan                                                                                                                
      external tscan                                                                                                               
                                                                                                                                   
      double precision autoscale, noscaling                                                                                        
      character*6 ytext                                                                                                            
      character*77 text(5)                                                                                                         
      double precision Data(maxProyr, Maxage)

                                                                                                                                   
      INCLUDE 'Message2.inc'       ! define messages                                                                               
                                                                                                                                   
! ------------------ EXECUTABLE CODE -------------------------------------                                                         
                                                                                                                                   
      RecalculatePopulations = .true. ! always, in this prog *&*&*

!      write(*,*) 'Scale Recruitments by : '
!      read(*,*) RecruitScaling

      RecruitScaling=1d0

      call readblock       !----------------- Pick up the ICA data and estimated parameters                                      
      call ReadVCV(VCV)    !---------------- Read the Variance-Covariance Matrix                                                 
!      call TableOut(1)
!      stop

        if (fitSRR) then
          NxParmUsedbyICA=NXparm
          Nxparm=Nxparm-2
          FitSRR = .false.
          NxData=NxData-(lastyear-firstyear+1-lag)
        else                                             ! *&*& else condition added
          NxParmUsedbyICA=NXparm                         !
        endif

      Call SOutputWindow('ICP 1.4 w','ICP.LOG')
                                                                                                                                   
!     Initialise the random number generator                                                                                       
                                                                                                                                   
      call Hello           !------------------ Display start-up Screen                                                             
                                                                                                                                   
      NoPercentiles = 10 !max, this is array size                                                                                  
                                                                                                                                   
      i =120                                                                                                                       
                                                                                                                                   
      call Screen_in_i(HW(1,Language),i,32000,-32000,Language)                                                                     
                                                                                                                                   
                                                                                                                                   
      call G05CBF(i)                                                                                                               
                                                                                                                                   
      dummy = 'Z'                                                                                                                  
                                                                                                                                   
!      do while (tScan(dummy, 'iIxX') .eq. 0)                                                                                      
!        write(*,33)'Use population parameters from ICA or XSA (I/X) ?'                                                            
!        read(*,34) dummy                                                                                                          
!33       format(' ',A52,' --> ')   ! \                                                                                            
!34       format(A1)                                                                                                               
!      enddo                                                                                                                       
                                                                                                                                   
!      if (tScan(dummy,'iI') .eq. 1) then                                                                                          
                                                                                                                                   



                                                                                                                                   
!         write(*,*) 'LAG',lag                                                                                                     
                                                                                                                                   
         if (LAG .EQ. -5) THEN                                                                                                     
           lag = 0                                                                                                                 
           call Screen_in_i(HW(2,Language),lag,10,-10,Language)                                                                    
         endif                                                                                                                     
                                                                                                                                   
         call OverWritePop                                                                                                         
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
      dummy = 'Z'                                                                                                                  
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
      call ReadProjFile    !---------------- Read the catch ratios and F-Multipliers                                               
      call ChkCatRatio     !---------------- Check the catch ratios, round up                                                      
      call SetProjVars     !---------------- Calculate the mean wts etc.                                                           
      Call ReadMBAL(MBAL)  !---------------- Read in the MBAL                                                                      
      Call ReadMaxFMult    !---------------- Read the maximum F-multiplier                                                         
                                                                                                                                   
      Call GetSRRChoice(Curve, Auto)                                                                                                
      Call FitNewSrr(Curve, Auto)       !---------------- Nonlinear fit of 6-parameter GP SRR                                      
      Call WriteNewSRR     !---------------- Write out the parameters                                                              
      Call SetMOSW(.false., YtPro) ! -------------- Set up temporary maturity and stock weights with noe error                     
      Call SetREcUsage                                                                                                             
      Call Project(Xbest, YtPro, .false.)  ! -------- Do the baseline projection                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!     Copy parameters out of XBEST into local S, F, etc.                                                                           
                                                                                                                                   
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
                                                                                                                                   
      autoscale = 100d0 ! force automatic scaling                                                                                  
      noscaling = 0d0                                                                                                              
      ast= HP(40,Language)  ! labelling for age column                                                                             
                                                                                                                                   
!   Write POPULATIONS                                                                                                              
                                                                                                                                   
      do jyear=1,ytpro                                                                                                             
        do iage=1, lastage-firstage+1                                                                                              
          Data(jyear,iage)=Npro(jyear,iage)                                                                                        
        enddo                                                                                                                      
      enddo                                                                                                                        
                                                                                                                                   
      Call WriteTable(HW(40,Language),maxproyr,maxage,Data,lastyear,lastyear+YtPro-1,firstage,lastage,8,132,14,ast, &              
      missing,autoscale)                                                                                                           
                                                                                                                                   
                                                                                                                                   
!   Write FISHING MORTALITY                                                                                                        
                                                                                                                                   
      do jyear=1,ytpro                                                                                                             
        do iage=1, lastage-firstage+1                                                                                              
          Data(jyear,iage)=Fpro(jyear,iage)                                                                                        
        enddo                                                                                                                      
      enddo                                                                                                                        
                                                                                                                                   
      Call WriteTable(HW(41,Language),maxproyr,maxage,Data,lastyear,lastyear+YtPro-1,firstage,lastage,8,132,14,ast, &              
      missing,noscaling)                                                                                                           
                                                                                                                                   
!                                                                                                                                  
!   Write the PARTIAL Fs by FLEET                                                                                                  
!                                                                                                                                  
                                                                                                                                   
      do ifleet = 1, Nfleet                                                                                                        
        write(14,*)                                                                                                                
        write(14,*)                                                                                                                
        write(ytext, '(I4)') ifleet                                                                                                
        CAll ConCat(Text(1),HW(29,Language),ytext)                                                                                 
        do jyear=1, YtPro                                                                                                          
          do iage=1, lastage-firstage+1                                                                                            
            Data(jyear,iage)=  dexp(Xbest(NySep))*                       &                                                         
            S(iage)*                                                      &                                                        
            CRatio(ifleet,iage)*                                          &                                                        
            FMultPro(ifleet,jyear)                                                                                                 
          enddo                                                                                                                    
        enddo                                                                                                                      
                                                                                                                                   
        Call WriteTable(text(1),maxproyr,maxage,Data,lastyear,lastyear+YtPro-1,firstage,lastage,8,132,14,ast,missing,noscaling)    
                                                                                                                                   
      enddo  ! fleets                                                                                                              
                                                                                                                                   
      write(14,*)                                                                                                                  
      write(14,*) HW(30,Language)                                                                                                  
      do iage = 1, lastage-firstage+1                                                                                              
        write(14,317) iage, NMPro(iage), MCMO(1,iage), MCSW(1,iage)                                                                
      enddo                                                                                                                        
                                                                                                                                   
317   format(' ',I2, 1X, 40(E15.8,1X))                                                                                             
      write(14,*)                                                                                                                  
      write(14,*)                                                                                                                  
      write(14,*) HW(31,Language)                                                                                                  
      write(14,*) HW(32,Language)                                                                                                  
      do iage = 1, lastage-firstage+1                                                                                              
        write(14,317) iage, VarMat(iage), VarSW(iage)                                                                              
      enddo                                                                                                                        
      write(14,*)                                                                                                                  
                                                                                                                                   
      write(14,*) HW(33,Language)                                                                                                  
                                                                                                                                   
      do jyear = 1, YtPro                                                                                                          
        write(14,9000) jyear+lastyear-1, Stock(jyear)                                                                              
      enddo                                                                                                                        
                                                                                                                                   
9000  format(' ',I4,2X,E23.12)                                                                                                     
                                                                                                                                   
      write(14,*) HW(34,Language) ! 'Fleet Catches by Weight'                                                                      
                                                                                                                                   
      write(14,*) HW(35,Language) ! 'Catch Estimates by Fleet'                                                                     
      do iyear = 1, YtPro                                                                                                          
        write(14,9010) iyear+lastyear-1, (FLCatch(ifleet,iyear),        &                                                          
           ifleet = 1, NFleet)                                                                                                     
      enddo                                                                                                                        
9010  format(' ',I4, 6(E23.12, 2X))                                                                                                
                                                                                                                                   
                                                                                                                                   
      write(14,*) HW(42,1) ! 'Fleet Catches by Number'                                                                             
                                                                                                                                   
      do ifleet = 1, Nfleet                                                                                                        
        write(ytext,'(I4)') ifleet                                                                                                 
        Call ConCat(Text(1), HW(35,Language), ytext)                                                                               
                                                                                                                                   
        LastF = dexp(Xbest(NySep))+1d-6                                                                                            
                                                                                                                                   
        do jyear=1,Ytpro                                                                                                           
          do iage= 1, lastage-firstage+1                                                                                           
            Data(jyear,iage) =                            &                                                                        
             NPro(jyear,iage)*                                           &                                                         
             LastF*S(iage)*CRatio(ifleet,iage)*                           &                                                        
             FMultPro(ifleet,jyear)/                                      &                                                        
             (S(iage)*CRatio(ifleet,iage)*                                &                                                        
             LastF*FMultPro(ifleet,jyear) + NMPro(iage))                  &                                                        
             *(1d0 - dexp( dble(- NMPro(iage) - LastF*                    &                                                        
             S(iage)*CRatio(ifleet,iage)*FMultPro(ifleet,jyear))))                                                                 
          enddo  ! ages                                                                                                            
        enddo ! years                                                                                                              
       Call WriteTable(text(1),maxproyr,maxage,Data,lastyear,lastyear+YtPro-1,firstage,lastage,8,132,14,ast,missing,autoscale)     
      enddo ! fleets                                                                                                               
                                                                                                                                   
!      write(14,*) 'Total Catches by Number '                                                                                      
                                                                                                                                   
      do jyear=1,YtPro                                                                                                             
        do iage= 1, lastage-firstage+1                                                                                             
        Data(jyear,iage) =    &                                                                                                    
        NPro(jyear,iage)* FPro(jyear,iage)/                            &                                                           
        (FPro(jyear,iage)+NMPro(iage)) *                                &                                                          
        (1d0 - dexp( - NMPro(iage) -FPro(jyear,iage)))                                                                             
        enddo ! ages                                                                                                               
      enddo  ! jyears                                                                                                              
                                                                                                                                   
      Call WriteTable(HW(36,Language),maxproyr,maxage,Data,lastyear,lastyear+YtPro-1,firstage,lastage,8,132,14,ast,  &             
      missing,autoscale)                                                                                                           
                                                                                                                                   
      close(14)                                                                                                                    
                                                                                                                                   
                                                                                                                                   
!                                                                                                                                  
!     Find the percentiles required by the user                                                                                    
!                                                                                                                                  
      NoPercentiles=5                                                                                                              
      Call GetPercentiles(NoPercentiles, Percentile, psize)                                                                        
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!     Core projection written to disk; Now do the variance estimates                                                               
! ----------------------------------------------------------------------                                                           
                                                                                                                                   
!     Set up the reference vector for multivariate random numbers                                                                  
                                                                                                                                   
                                                                                                                                   
      Call MCPROJ (VCV, MBAL, Curve, Auto)   
                                                                                                                                   
!                                                                                                                                  
!     Now read the *.MC files and create the *.pby files                                                                           
!                                                                                                                                  
                                                                                                                                   
      Call CreatePBYfilesFP(Nfleet, NoPercentiles, Percentile, 10)                                                                 
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!      Text(1) = HW(64,Language)                                                                                                    
!      call Screen_out_a(Text,5,1)                                                                                           

      stop
                                                                                                                                   
      end                                                                                                                          
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
! ///////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                   
      double precision Function BootStrapRecruit(SSB,year,RecERR)                                                                  
                                                                                                                                   
! ///////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                  
!     Generates a recruitment for year 'year'                                                                                      
!     given that stock size = SSB.                                                                                                 
!                                                                                                                                  
!        If (RECERR) = true   then a bootstrapped (random resampling with                                                          
!          replacement) error is added to the recruitment prediction                                                               
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
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
                                                                                                                                   
!     Generate uniform random variate between 1 and NoSrrData                                                                      
                                                                                                                                   
      NoSrrData = lastyear-firstyear+1                                                                                             
                                                                                                                                   
                                                                                                                                   
      If (RecERR) then                                                                                                             
        pos = IDINT(1d0+  (G05CAF(X)*dble(NoSRRData) ))                                                                            
        BootStrapRecruit = Recruit*dexp(BSResid(pos))*RecruitScaling 
      else                                                                                                                         
        BootStrapRecruit = Recruit*RecruitScaling  
      endif                                                                                                                        
                                                                                                                                   
      return                                                                                                                       
      end                                                                                                                          
                                                                                                                                   
                                                                                                                                   
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
! /////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                   
      double precision Function CalcProSSB(iyear) ! Calculate SSB in a projection year                                             
                                                                                                                                   
! /////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                   
      include 'indat.inc'                                                                                                          
      include 'projc3.inc'                                                                                                         
                                                                                                                                   
                                                                                                                                   
      integer iyear, iage                                                                                                          
      double precision SSB                                                                                                         
                                                                                                                                   
      SSB = 0d0                                                                                                                    
                                                                                                                                   
                                                                                                                                   
      do iage = 1, lastage-firstage +1                                                                                             
          SSB = SSB + NPro(iyear, iage)* MCMO(iyear,iage)*              &                                                          
             MCSW(iyear,iage) * dexp( - FPro(iyear,iage)*PF             &                                                          
                       - NMPro(iage)*PM )                                                                                          
      enddo                                                                                                                        
                                                                                                                                   
      if (SSB .eq. 0) then   ! some debugging code here                                                                            
        write(*,*) 'ERROR : SSB = ZERO in projection year ',            &                                                          
         iyear+lastyear                                                                                                            
        pause                                                                                                                      
        write(*,*) 'N, SW, MO, F, NM, PF, PM: '                                                                                    
        do iage = 1, lastage-firstage+1                                                                                            
          write(*,*) iyear, NPro(iyear, iage),SWPro(iage),MOPro(iage),  &                                                          
          Fpro(iyear,iage), NMPro(iage), PF, PM                                                                                    
        enddo                                                                                                                      
      endif                                                                                                                        
                                                                                                                                   
      CalcProSSB = SSB                                                                                                             
                                                                                                                                   
      return                                                                                                                       
      end                                                                                                                          
                                                                                                                                   
                                                                                                                                   
! /////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                   
       Subroutine MCPROJ( VCV, MBAL, Curve, Auto)                                                                                  
                                                                                                                                   
! /////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                   
!                                                                                                                                  
!      Variance of projections by Monte-Carlo method                                                                               
!                                                                                                                                  
        implicit none                                                                                                              
        include 'indat.inc'                                                                                                        
        include 'Sepmodel.inc'                                                                                                     
        include 'projc3.inc'                                                                                                       
        include 'message1.inc'
        include 'srr.inc'
                                                                                                                                   
        double precision EPS, Z(maxparm), VCV(maxparm,maxparm)                                                                     
        double precision R(11500),FC(maxdata)                                                                                      
        double precision MBAL, MeanF(MaxProYr), Yield(MaxProYr)                                                                    
        character*77 text(10)                                                                                                      
        character*1 curve, auto

        character*1100 line
                                                                                                                                   
        integer NR, ifail, ItsToRun, Its , iyear, age                                                                              
        integer ifleet, IC, i, istate
        character*40 filename                                                                                                      
        character*1 Newits                                                                                                         
        logical UseMCfile                                                                                                          
        integer tscan
        external tscan

                                                                                                                                   
        Ifail = 1                                                                                                                  
        EPS =   0.09/dble(Nxparm)                                                                                                  
        NR = 11500                                                                                                                 
        IC = maxparm                                                                                                               
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!       Decide whether to use iterations in MC file                                                                                
                                                                                                                                   
        NewIts =KO(1,Language)  
        Call Screen_in_a(HW(62,Language),NewIts,KO(1,Language),Language)                                                           
                                                                                                                                   
        If (TScan(Newits, KY(1,Language)) .ne. 0) then ! Read ICA.MC file for sample data

          UseMCFile = .true.                                                                                                       
                                                                                                                                   
!          Open the MCMC file here                                                                                                 
!          Read to the end                                                                                                         
!          Count the number of iterations                                                                                          
!          rewind the file                                                                                                         

           
           Open(30, file=ica_mc,STATUS='OLD', ERR=120)                                                                             
                                                                                                                                   
           ItsToRun=0
           write(*,*) 'P to  read : ',NxParmUsedByICA
           pause
119        read(30,*, END=121, ERR=120, IOSTAT=istate) (Z(i), i=1,NxParmUsedByICA)
             write(*,*) 'Iteration : ',ItstoRun,i, Istate
             ItsToRun=ItsToRun+1
           GoTo 119                                                                                                                
120        Continue ! error in file access
           write(*,*) 'IOSTAT: ',Istate
           Text(1)=HW(63,Language)                                                                                               
           Call Screen_out_a(Text,10,1)                                                                                          
           stop                                                                                                                  

121        Continue
           rewind(30)
                                                                                                                                   
        else ! use  VCV sampling method                                                                                            
          UseMCFile = .false.                                                                                                      
                                                                                                                                   
!         Get required number of simulations                                                                                         

          ItsToRun=200
          call Screen_in_i(HW(17,Language),ItsToRun,10000,1,Language)                                                              
                                                                                                                                   
          if (NR .lt. (Nxparm+1)*(Nxparm+2)/2 ) then                                                                               
            Text(1)=' '                                                                                                            
            Text(2)=HW(18,Language)                                                                                                
            Text(3)=' '                                                                                                            
            Call Screen_out_a(Text, 10, 3)                                                                                         
            stop                                                                                                                   
          endif                                                                                                                    
          IFAIL = 1
          call G05EAF (Xbest, NxParm, VCV, IC, EPS, R, NR, IFAIL)
          if (IFAIL .ne. 0) then                                                                                                   
            write(Text(1), '(I2)') IFAIL                                                                                           
            Call ConCat(Text(1), HW(19,Language), Text(1))                                                                         
            Call Screen_out_a(text(1), 10, 1)                                                                                      
          endif                                                                                                                    
        endif      ! Using VCV sampling                                                                                                                
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
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
!          filename = 'Discard'//char(ifleet+48)//'.mc'                                                                            
!          Open(14+ifleet,status='unknown',file=filename)                                                                          
!          write(14+ifleet,*) ItsToRun,NFleet,YtPro-1,MBAL,lastyear+1                                                              
        enddo !   fleet                                                                                                            
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
        DO ITS = 1, ItsToRun
                                                                                                                                   
        If (UseMCFile) then                                                                                                        
                                                                                                                                   
          read(30,*) (Z(i), i=1,NxparmUsedByICA)    
                                                                                                                                
                                                                                                                                  
        else                                                                                                                       
          Ifail = 0                                                                                                                
          call G05EZF (Z, NxParm, R, NR, Ifail)  ! Variability on Population params                                                
          if (IFAIL .ne. 0) then                                                                                                   
            write(Text(1), '(I2)') IFAIL                                                                                           
            Call ConCat(Text(1), HW(20,Language), Text(1))                                                                         
            Call Screen_out_a(text(1), 10, 1)                                                                                      
          endif                                                                                                                    
                                                                                                                                   
        endif                                                                                                                      
                                                                                                                                   
        call SetMOSW( .true., YtPro)                                                                                               
        full = .true.
        Call LSFUN1 (nxdata,nxparm,Z,FC)     ! *&*&* changed from NXPARM to NXPARMUSEDBYICA ; 11/8/1998                                                                                      
        Call FitNewSRR(curve, auto)                                                                                                
                                                                                                                                   
        Call GenFMult(ftyear-lastyear+1, YtPro)
        Call Project(Z, YtPro, .true.)     ! YtPro+1                                                                               
                                                                                                                                   
                                                                                                                                   
!       Write out the stock by year for this iteration                                                                             
                                                                                                                                   
                                                                                                                                   
        write(1, 340) (Stock(iyear), iyear=2,YtPro)                                                                                
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!       Write out the recruitments                                                                                                 
                                                                                                                                   
                                                                                                                                   
        write(2, 340) (Npro(iyear,1), iyear = 2,YtPro)                                                                             
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!       Write out the mean fishing mortality over reference ages                                                                   
                                                                                                                                   
        Do iyear = 2, YtPro                                                                                                        
          MeanF(iyear) = 0d0                                                                                                       
            do age = LoFage, HiFage                                                                                                
              MeanF(iyear) = MeanF(iyear)+Fpro(iyear, age-firstage+1)                                                              
            enddo                                                                                                                  
            MeanF(iyear) = MeanF(iyear)/dble(HiFage-LoFage+1)                                                                      
        enddo                                                                                                                      
                                                                                                                                   
        write(3, 340) (MeanF(iyear), iyear=2,Ytpro)                                                                                
                                                                                                                                   
340     format (4X, 25(E23.12))                                                                                                    
                                                                                                                                   
                                                                                                                                   
!       Write out the total yield                                                                                                  
                                                                                                                                   
                                                                                                                                   
        Do iyear = 2, YtPro                                                                                                        
          Yield(iyear) = 0d0                                                                                                       
          do ifleet = 1, Nfleet                                                                                                    
            Yield(iyear) = Yield(iyear)+ FLCatch(ifleet,iyear)                                                                     
          enddo ! fleets                                                                                                           
        enddo   ! years                                                                                                            
                                                                                                                                   
        write(4, 340) (Yield(iyear), iyear=2,YtPro)                                                                                
                                                                                                                                   
                                                                                                                                   
!      Write the Fleet Fmults, Yields and discards                                                                                 
                                                                                                                                   
                                                                                                                                   
       do ifleet = 1,Nfleet                                                                                                        
           write(14+ifleet,340)(FmultPro(ifleet,iyear),iyear=2,Ytpro)                                                              
           write(19+ifleet,340)(FLcatch(ifleet,iyear),iyear=2,Ytpro)                                                               
           write(19+ifleet,340)(Discards(ifleet,iyear),iyear=2,Ytpro)                                                              
       enddo                                                                                                                       
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!        write(*,*) ' Iteration ',its                                                                                              
                                                                                                                                   
        ENDDO ! Iterations                                                                                                         
                                                                                                                                   
        Close (1)                                                                                                                  
        Close (2)                                                                                                                  
        Close (3)                                                                                                                  
        close (4)                                                                                                                  
                                                                                                                                   
        do i = 1,Nfleet                                                                                                            
          Close (14+ i)   ! F-multiplier files                                                                                     
          Close (19+i)   ! Fleet catch files                                                                                       
!          close (14+i)   ! Discard files                                                                                          
        enddo                                                                                                                      
                                                                                                                                   
        close (30)                                                                                                                 
                                                                                                                                   
        return                                                                                                                     
                                                                                                                                   
        end                                                                                                                        
                                                                                                                                   
                                                                                                                                   
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                     
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!  //////////////////////////////////////////////////////////////                                                                  
                                                                                                                                   
       Subroutine BotchSolve(xyear, LastFStch,LastFtrue)                                                                           
                                                                                                                                   
! //////////////////////////////////////////////////////////////                                                                   
                                                                                                                                   
! Solves from linear approximation and iteration until no catch                                                                    
! is further than 0.01% from a catch constraint                                                                                    
      implicit none                                                                                                                
      include 'indat.inc'                                                                                                          
      include 'projc3.inc'                                                                                                         
                                                                                                                                   
      include 'sepmodel.inc'                                                                                                       
      include 'Message1.inc'                                                                                                       
                                                                                                                                   
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
                                                                                                                                   
!     Work out the current F and project the population                                                                            
                                                                                                                                   
      do iage = 1, lastage-firstage                                                                                                
         Fpro(CurrYear, iage)=0.0                                                                                                  
         do ifleet = 1, Nfleet                                                                                                     
            Fpro(CurrYear, iage)= Fpro(CurrYear, iage)                  &                                                          
            +LastFref(UseF(ifleet))*Select(iage)*CRatio(ifleet,iage)*   &                                                          
              FMultPro(ifleet, CurrYear)                                                                                           
         enddo   ! fleets                                                                                                          
         NPro(CurrYear+1, iage+1) = NPro(CurrYear,iage) *               &                                                          
           dexp( - FPro(CurrYear, iage) - NMPro(iage) )                                                                            
      enddo                                                                                                                        
                                                                                                                                   
!     The plus-group                                                                                                               
                                                                                                                                   
      FPro(CurrYear,lastage-firstage+1)=FPro(CurrYear,lastage-firstage)                                                            
      Npro(CurrYear+1,lastage-firstage+1)=                              &                                                          
         NPro(CurrYear+1, lastage-firstage+1) +                         &                                                          
         NPro(CurrYear, lastage-firstage+1)*                            &                                                          
         dexp(-FPro(CurrYear, lastage-firstage+1)-                      &                                                          
              NMPro(lastage-firstage+1))                                                                                           
                                                                                                                                   
!  Catches by fleet                                                                                                                
                                                                                                                                   
      do ifleet = 1, NFleet                                                                                                        
        FLCatch(ifleet, CurrYear) = 0.0d0                                                                                          
        do iage = 1, lastage-firstage+1                                                                                            
                                                                                                                                   
          Z = FPro(CurrYear,iage) + NMPro(iage)                                                                                    
          PartF=LastFref(UseF(ifleet))*Select(iage)*                    &                                                          
           CRatio(ifleet,iage)*                                         &                                                          
                  FMultPro(ifleet, CurrYear)*Retention(ifleet,iage)                                                                
!          if (iage .ge. lastage-firstage) then                                                                                    
            FLCatch(ifleet, CurrYear) = FLCatch(ifleet, CurrYear)       &                                                          
               + NPro(CurrYear, iage) * PartF/Z * (1d0 - dexp(-Z))*     &                                                          
                 CWPro(ifleet, iage)                                                                                               
!          else                                                                                                                    
!            FLCatch(ifleet, CurrYear) = FLCatch(ifleet, Curryear)                                                                 
!     *         + (Npro(Curryear,iage) - Npro(CurrYear+1,iage+1))                                                                  
!     *         * PartF/Z*CWPro(ifleet, iage)                                                                                      
!          endif                                                                                                                   
         enddo                                                                                                                     
      enddo                                                                                                                        
                                                                                                                                   
! Discards by Fleet                                                                                                                
                                                                                                                                   
      do ifleet = 1, NFleet                                                                                                        
                                                                                                                                   
                                                                                                                                   
        Discards(ifleet, CurrYear) = 0.0                                                                                           
        do iage = 1, lastage-firstage+1                                                                                            
          Z = FPro(CurrYear,iage) + NMPro(iage)                                                                                    
          PartF = LastFref(UseF(ifleet))*Select(iage)*                  &                                                          
           CRatio(ifleet,iage)*                                         &                                                          
              FMultPro(ifleet, CurrYear)*(1d0-Retention(ifleet,iage))                                                              
!          if (iage .ge. lastage-firstage) then                                                                                    
            Discards(ifleet, CurrYear) = Discards(ifleet, CurrYear)     &                                                          
               + NPro(CurrYear, iage) * PartF/Z * (1d0 - dexp(-Z))*     &                                                          
                 DiscWt(ifleet, iage)                                                                                              
!          else                                                                                                                    
!          Discards(ifleet, CurrYear) = Discards(ifleet, CurrYear) +                                                               
!     *         (NPro(CurrYear,iage) - Npro(CurrYear+1,iage+1))*                                                                   
!     *         PartF/Z*DiscWt(ifleet,iage)                                                                                        
!          endif                                                                                                                   
         enddo                                                                                                                     
      enddo                                                                                                                        
                                                                                                                                   
                                                                                                                                   
!   The residuals                                                                                                                  
                                                                                                                                   
      NotSolved = .FALSE.                                                                                                          
                                                                                                                                   
      do ifleet =1, NFleet                                                                                                         
        If (FmultPro(ifleet, CurrYear) .ne. MaxFmult) then ! only check those fleets not F-limited                                 
          IF (ConsCat(ifleet,Curryear) .ge. 0d0) then      ! and only those fleets not F-constrained                               
          If (dabs(FlCatch(ifleet, CurrYear)-ConsCat(ifleet,CurrYear))  &                                                          
                  /ConsCat(ifleet,CurrYear)                             &                                                          
             .gt. Tolerance(ifleet) ) NotSolved = .TRUE.                                                                           
          endif                                                                                                                    
        endif                                                                                                                      
      enddo                                                                                                                        
                                                                                                                                   
! Update the  Fmults for the next loop by linear approximation                                                                     
                                                                                                                                   
      do ifleet = 1, NFleet                                                                                                        
        If (ConsCat(ifleet,CurrYear) .gt. 0d0 ) then                 ! it's a catch constraint                                     
          FMultPro(ifleet, CurrYear) = FmultPro(ifleet, CurrYear) +     &                                                          
          FmultPro(ifleet, CurrYear)/FlCatch(ifleet, CurrYear) *        &                                                          
            (ConsCat(ifleet,CurrYear)-FlCAtch(ifleet, CurrYear))                                                                   
          If (FmultPro(ifleet, CurrYear) .gt. MaxFMult) then             ! limits the F                                            
              FmultPro(ifleet, CurrYear) = MaxFmult                                                                                
          endif                                                                                                                    
        else                                                          ! it's an F-multiplier                                       
          if (FMultPro(ifleet, CurrYear) .ne.                           &                                                          
                  -ConsCat(ifleet,CurrYear)) then                     ! this to make sure the loop is done at least twice          
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
                                                                                                                                   
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                
                                                                                                                                   
                                                                                                                                   
! ////////////////////////////////////////////////////////////////////////                                                         
                                                                                                                                   
      Subroutine Project(X, iyear, RecERR) ! with X as far as iyear                                                                
                                                                                                                                   
! ///////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                  
!     Do a single projection from the X vector of S, N and with                                                                    
!       options set in projection file, for succeeding iyears                                                                      
!       with constant catch by fleet (ConsCatch) or F_multiplier                                                                   
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
!                                                                                                                                  
      implicit none                                                                                                                
      include 'indat.inc'                                                                                                          
      include 'projc3.inc'                                                                                                         
      include 'SRR.inc'                                                                                                            
      include 'SepModel.inc'                                                                                                       
                                                                                                                                   
      integer iyear, iage, parmno, year, jyear,  age, index                                                                        
      double precision X(maxparm)                                                                                                  
      double precision LastFStch, LastFReal                                                                                        
      double precision S(maxage)                                                                                                   
      double precision EstdRecruit, SSB                                                                                            
      double precision  CalcProSSB, BootStrapREcruit ! Functions                                                                   
      double precision CalcSSB                                                                                                     
      logical  RecErr                                                                                                              
                                                                                                                                   
!----------------------------------  Get the parameters out of X                                                                   
                                                                                                                                   
                                                                                                                                   
      parmno = NySep                                                                                                               
                                                                                                                                   
                                                                                                                                   
      LastFStch = dexp(X(parmno))+1d-6                      !  : last F in pseudo-data                                             
      LastFreal = F(lastyear-firstyear+1,Refage-firstage+1) ! last F at reference age in the assessment                            
                                                                                                                                   
!---------------------------------     The selection pattern                                                                       
                                                                                                                                   
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
!          write(*,*) 'Debug : S ',parmno,age,S(iage)                                                                              
        endif                                                                                                                      
      enddo                                                                                                                        
                                                                                                                                   
      S(lastage-firstage+1) = S(lastage-firstage)                                                                                  
                                                                                                                                   
                                                                                                                                   
      do iage = 1, lastage-firstage+1                                                                                              
        Select(iage) = S(iage) ! for the LSFUN1                                                                                    
      enddo                                                                                                                        
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!----------------------------------     Starting Populations in lastyear                                                           
                                                                                                                                   
      do age = firstage, lastage -1                                                                                                
        iage = age-firstage+1                                                                                                      
        parmno = parmno +1                                                                                                         
        Npro(1,iage) = dexp(X(parmno)) + 1d0                                                                                       
      enddo                                                                                                                        
                                                                                                                                   
! ---------------------------------  Project to 1 Jan on lastyear+1                                                                
                                                                                                                                   
      FPro(1,1) = LastFreal*S(1)                                                                                                   
      do age = firstage+1, lastage                                                                                                 
        iage = age-firstage+1                                                                                                      
        FPro(1, iage) = LastFReal*S(iage)                                                                                          
        Npro(2,iage) = NPro(1,iage-1)*dexp(-FPro(1,iage-1)-             &                                                          
                       NMPro(iage-1))                                                                                              
      enddo                                                                                                                        
                                                                                                                                   
      FPro(1,lastage-firstage+1) = LastFReal*S(lastage-firstage)                                                                   
                                                                                                                                   
!------------------------------------     The plus-groups in lastyear+1                                                            
                                                                                                                                   
      Npro(1, lastage-firstage+1) =                                     &                                                          
       (N(lastyear-firstyear+1,lastage-firstage+1))                                                                                
                                                                                                                                   
      Npro(2,lastage-firstage+1)=NPro(1,lastage-firstage+1)*            &                                                          
         dexp(-FPro(1,lastage-firstage+1)-NMPro(lastage-firstage+1))    &                                                          
         + Npro(1,lastage-firstage)*                                    &                                                          
         dexp(-FPro(1,lastage-firstage)-NMPro(lastage-firstage))                                                                   
                                                                                                                                   
!----------------------------------     Estimates of numbers at last true age in previous years are                                
!                                       skipped over in parameter list                                                             
                                                                                                                                   
      do year = lastyear-NySep+1, lastyear-1                                                                                       
        parmno=parmno+1                                                                                                            
      enddo                                                                                                                        
                                                                                                                                   
!    Decide whether to use the recruitment in lastyear                                                                             
                                                                                                                                   
!      UseRec1 = .false.                                                                                                            
!      do index = 1, nageix                                                                                                         
!        if ((fage(index).eq.firstage).and.(lyear(index).ge.lastyear))   &                                                          
!        then                                                                                                                       
!        if (Aindex(index,lastyear-fyear(index)+1,1) .gt. 0d0)           &                                                          
!          UseRec1 = .true.                                                                                                         
!        endif                                                                                                                      
!      enddo                                                                                                                        
                                                                                                                                   
                                                                                                                                   
!      UseRec2 = .false.                                                                                                            
!      do index = 1, nageix                                                                                                         
!        if ((fage(index).eq.firstage).and.(lyear(index).eq.lastyear+1)  &                                                          
!          .and. (Aindex(index,lastyear+1-fyear(index)+1,1) .gt. 0d0))   &                                                          
!           UseRec2 = .true.                                                                                                        
!      enddo                                                                                                                        
                                                                                                                                   
      If (UseRec2) then                                                                                                            
        parmno = Parmno+1                                                                                                          
        EstdRecruit = dexp(X(Parmno))  ! the incoming recruitment in lastyear+1                                                    
!        write(*,*) 'Parmno , recruits', parmno, EstdRecruit  ! debug                                                              
      endif                                                                                                                        
                                                                                                                                   
!---------------------     Not interested in the catchabilities etc.  here:                                                        
                                                                                                                                   
                                                                                                                                   
!---------------------     All parameters needed are now copied out of the X vector                                                
                                                                                                                                   
                                                                                                                                   
!     First project from lastyear to lastyear +1                                                                                   
!     This needs to be handled differently from other years as there may be an estimate                                            
!     of recruitment from a survey made in a year after the last year of catch data,                                               
!     which is the first year for the projection (YtPro=2).                                                                        
!                                                                                                                                  
!                                                                                                                                  
!     Note that lastyear = last year for which there are catch data                                                                
!               YtPro    = Number of years to project, which                                                                       
!                          starts at lastyear (=last year of catch at age data)                                                    
!                so that N(lastyear) = NPro(1)                                                                                     
                                                                                                                                   
! ------------------------------------------     do the plus-group                                                                 
                                                                                                                                   
      FPro(1, lastage-firstage+1) =                                     &                                                          
         FPro(1, lastage-firstage)                                                                                                 
                                                                                                                                   
       NPro(1,lastage-firstage+1) =                                     &                                                          
        N(lastyear-firstyear+1,lastage-firstage+1)*                     &                                                          
        dexp(-FPro(1,lastage-firstage+1)-                               &                                                          
              NM(lastyear-firstyear+1,lastage-firstage+1))              &                                                          
       + N(lastyear-firstyear+1,lastage-firstage)*                      &                                                          
        dexp(-FPro(1,lastage-firstage)-                                 &                                                          
              NM(lastyear-firstyear+1,lastage-firstage))                                                                           
                                                                                                                                   
      Stock(1) = CalcProSSB(1)                                                                                                     
                                                                                                                                   
!----------------------------------------------     Projections for the succeeding years                                           
                                                                                                                                   
      do jyear = 1, iyear  ! +1                     ------ years to do the projections                                             
                                                                                                                                   
!---------------------------- Recruitment                                                                                          
                                                                                                                                   
        if ( (jyear - lag) .le. 0) then                                                                                            
          SSB = CalcSSB(lastyear+jyear-lag) !---------- SSB from the ICA analysis                                                  
        else                                                                                                                       
          SSB = CalcProSSB(jyear-lag)                 !------ SSB from the projections                                             
        endif                                                                                                                      
                                                                                                                                   
!        UseRec1 = .true.        !----------- Fixed during meeting                                                                 
!        Userec2 =.true.                                                                                                           
                                                                                                                                   
!        write(*,*) jyear, UseRec1 
        if (jyear .eq. 1) then                                                                                                     
           if (UseRec1) then                                                                                                       
             Npro(jyear,1) = N(Lastyear-firstyear+1,1)                                                                             
           else                                                                                                                    
             Npro(jyear,1)=BootStrapRecruit(SSB,lastyear+jyear-1,       &                                                          
                                                               RecERR)                                                             
           endif
           Npro(jyear+1,2)=Npro(jyear,1) *dexp((-NM(lastyear-firstyear+1,1) &
             -F(lastyear-firstyear+1,1)))

        else if (jyear .eq. 2) then                                                                                                
           if (UseRec2) then                                                                                                       
              Npro(jyear,1) = EstdRecruit                                                                                          
           else                                                                                                                    
              Npro(jyear,1) = BootStrapRecruit(SSB, lastyear+jyear-1,   &                                                          
                                                               RecERR)                                                             
           endif                                                                                                                   
        else if (jyear .gt. 2) then                                                                                                
          Npro(jyear,1) = BootStrapRecruit(SSB,lastyear+jyear-1,RecERR)                                                            
        endif                                                                                                                      
                                                                                                                                   
        if (jyear .gt. 1) then                                                                                                     
          Call BotchSolve(jyear, LastFStch, LastFreal)                                                                             
        endif                                                                                                                      
                                                                                                                                   
                                                                                                                                   
!----------------------------- Stock                                                                                               
                                                                                                                                   
        Stock(jyear) = CalcProSSB(jyear)                                                                                           
                                                                                                                                   
      enddo                                                                                                                        
                                                                                                                                   
                                                                                                                                   
      return                                                                                                                       
      end                                                                                                                          
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
! /////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                  
      Subroutine GetSRRChoice(Curve, Auto)                                                                                         
!                                                                                                                                  
! ////////////////////////////////////////////////////////////////////                                                             
      implicit none                                                                                                                
      include 'indat.inc'                                                                                                          
      include 'sepmodel.inc'                                                                                                       
      include 'srr.inc'                                                                                                            
      include 'projc3.inc'                                                                                                         
      include 'message1.inc'                                                                                                       
                                                                                                                                   
      integer tscan                                                                                                                
      external tscan                                                                                                               
                                                                                                                                   
      character*1 Curve, Auto                                                                                                      
      character*3 ytext                                                                                                            
      character*77 text(10)                                                                                                        
                                                                                                                                   
      double precision AX(8), Alow(8), Ahigh(8), WX(500), SSQ                                                                      
      integer iw(12), i, nparm, ibound, liw, lw, ifail,nosrrdata                                                                   
      double precision StockSize(maxyear), RecruitNos(maxyear)                                                                     
                                                                                                                                   
      NoSrrData= lastyear-firstyear+1                                                                                              
                                                                                                                                   
                                                                                                                                   
      Ifail = 99 ! to start                                                                                                        
                                                                                                                                   
!      do while ( (Ifail .ne. 0) .and. (Ifail .ne. 5)) ! ie keep repeating until the minimisation is successful                     
                                                                                                                                   
      If (Ifail .ne. 99) then                                                                                                      
        Text(2)=HW(3,Language)                                                                                                     
        Text(3)=' '                                                                                                                
        Text(1)=Text(3)                                                                                                            
        Call Screen_OUT_a(Text,10,3)                                                                                               
      endif                                                                                                                        
      Ifail =1                                                                                                                     
                                                                                                                                   
      Curve = ' '                                                                                                                  
      do i=5,11                                                                                                                    
        Text(i-4)=HW(i,language)                                                                                                   
      enddo                                                                                                                        
                                                                                                                                   
!     READ THE TYPE OF SRR MODEL                                                                                                   
                                                                                                                                   
      Call Screen_out_a(Text,10,7)                                                                                                 
      Call Screen_in_a(HW(12,Language),Curve,KW(1,Language),Language)                                                              
                                                                                                                                   
!     CHOOSE YEAR RANGE FOR SRR MODEL                                                                                              
                                                                                                                                   
      lychosen = -1                                                                                                                
      fychosen = 1                                                                                                                 
      do while (lychosen .le. fychosen)                                                                                            
       fychosen=firstyear                                                                                                          
       lychosen=lastyear                                                                                                           
       Call Screen_in_i(HW(13,Language),fychosen,lastyear-3,            &                                                          
                                                   firstyear,Language)                                                             
       Call Screen_in_i(HW(14,Language),lychosen,lastyear+1,            &                                                          
                                                 firstyear+1,Language)                                                             
        if (lychosen .le. fychosen) then                                                                                           
          Text(1)=' '                                                                                                              
          Text(2)=HW(15,Language)                                                                                                  
          Text(3)=' '                                                                                                              
          call screen_out_a(Text, 10,3)                                                                                            
        endif                                                                                                                      
      enddo                                                                                                                        
                                                                                                                                   
!     ASK IF ERRORS TO BE AUTOCORRELATED                                                                                           
                                                                                                                                   
      Auto=' '                                                                                                                     
                                                                                                                                   
      Call SCreen_in_a(HW(16,Language),Auto,KW(2,Language),Language)                                                               
                                                                                                                                   
                                                                                                                                   
!      enddo  
                                                                                                                                   
      end ! of subroutine GetSRRChoice                                                                                             
                                                                                                                                   
                                                                                                                                   
! ////////////////////////////////////////////////////////////////////////                                                         
                                                                                                                                   
                                                                                                                                   
      Subroutine FitNewSRR(Curve, Auto)                                                                                            
                                                                                                                                   
! ////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                  
!                                                                                                                                  
                                                                                                                                   
                                                                                                                                   
      implicit none                                                                                                                
      include 'indat.inc'                                                                                                          
      include 'sepmodel.inc'                                                                                                       
      include 'srr.inc'                                                                                                            
      include 'projc3.inc'                                                                                                         
      include 'message1.inc'                                                                                                       
                                                                                                                                   
      integer tscan                                                                                                                
      external tscan                                                                                                               
                                                                                                                                   
      character*1 Curve, Auto                                                                                                      
      character*77 text(10)

      double precision AX(8), Alow(8), Ahigh(8), WX(500), SSQ                                                                      
      integer iw(12), i, nparm, ibound, liw, lw, ifail,nosrrdata                                                                   
      double precision StockSize(maxyear), RecruitNos(maxyear)                                                                     
                                                                                                                                   
      NoSrrData= lastyear-firstyear+1                                                                                              
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!     Set the constraints accordingly                                                                                              
                                                                                                                                   
      do i = 1,8  ! initially all unconstrained                                                                                    
        Alow(i) = 0d0                                                                                                              
        AX(i) = 0d0                                                                                                                
        Ahigh(i) = 1d20                                                                                                            
      enddo                                                                                                                        
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
      if (tScan(curve,'Ss') .ne. 0) then  ! Shepherd                                                                               
        Alow(2) = 0d0                                                                                                              
        Ahigh(2) = 0d0                                                                                                             
        Alow(6) = 0d0                                                                                                              
        Ahigh(6) = 0d0                                                                                                             
      else if (tScan(curve,'Bb') .ne. 0) then ! BevHolt                                                                            
        Alow(2) = 0d0                                                                                                              
        Ahigh(2) = 0d0                                                                                                             
        Alow(4) = 1d0                                                                                                              
        Ahigh(4) = 1d0                                                                                                             
        Alow(6) = 0d0                                                                                                              
        Ahigh(6) = 0d0                                                                                                             
      else if (tScan(curve,'Rr') .ne. 0) then ! Ricker                                                                             
       Alow(3) = 1d0                                                                                                               
       Ahigh(3) = 1d0                                                                                                              
       Alow(4) = 0d0                                                                                                               
       Ahigh(4) = 0d0                                                                                                              
       Ahigh(6) =0d0                                                                                                               
       Alow(6) = 0d0                                                                                                               
      else if (tScan(curve,'NnOo') .ne. 0) then ! Mean only                                                                        
       Alow(1) = 0d0                                                                                                               
       Ahigh(1) = 0d0                                                                                                              
       Alow(2) = 0d0                                                                                                               
       Ahigh(2) = 0d0                                                                                                              
       Alow(3) =  1d0                                                                                                              
       Ahigh(3) = 1d0                                                                                                              
       Ahigh(4) = 0d0                                                                                                              
       Ahigh(4) = 0d0                                                                                                              
      endif                                                                                                                        
                                                                                                                                   
      If (tScan(Auto, 'Ii') .ne. 0) then ! no autocorrelation                                                                      
       Alow(5) = 0d0                                                                                                               
       Ahigh(5) = 0d0                                                                                                              
      else                                                                                                                         
       Alow(5) = -1d0                                                                                                              
       Ahigh(5) = 1d0                                                                                                              
      endif                                                                                                                        
                                                                                                                                   
!     get the vectors of Rec and SSB to be used                                                                                    
                                                                                                                                   
      Call GetSRR(StockSize, RecruitNos, NoSRRdata)                                                                                
                                                                                                                                   
!     Put in some starting values for AX                                                                                           
                                                                                                                                   
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
                                                                                                                                   
                                                                                                                                   
! LOG TRANSFORM THE PARAMS                                                                                                         
                                                                                                                                   
      do i =1,6                                                                                                                    
       if (i .ne. 5) then                                                                                                          
        AX(i) = dlog(AX(i) + 1d-8)                                                                                                 
        Alow(i) = dlog(Alow(i) + 1d-8)                                                                                             
        Ahigh(i) = dlog(Ahigh(i) + 1d-8)                                                                                           
       endif                                                                                                                       
                                                                                                                                   
       if (Alow(i) .eq. Ahigh(i)) Ahigh(i) = Alow(i)+ 1d-9                                                                         
                                                                                                                                   
      enddo                                                                                                                        
                                                                                                                                   
                                                                                                                                   
!     Calculate the slope for Ockham model                                                                                         
                                                                                                                                   
!     First find the lowest SSB observation                                                                                        
                                                                                                                                   
      AX(8) = 1d20                                                                                                                 
      do i = 1, NoSrrData                                                                                                          
        if (StockSize(i) .lt. AX(8)) AX(8)=StockSize(i)                                                                            
      enddo                                                                                                                        
                                                                                                                                   
      AX(8) = dlog(AX(8))       ! AX(8) is log lowest observed SSB                                                                 
                                                                                                                                   
      if (tScan(curve,'oO') .ne. 0) then                                                                                           
         AX(7) = 0d0                                                                                                               
         do i =1,NoSrrData                                                                                                         
           AX(7) = AX(7) + (dlog(RecruitNos(i)) - AX(6))*               &                                                          
               (dlog(RecruitNos(i)) - AX(6))                                                                                       
         enddo                                                                                                                     
         AX(7) = SQRT( AX(7)/ dble(NoSrrData) )                                                                                    
      else                                                                                                                         
        AX(7) = 99d0  ! marker value: not using Ockham model                                                                       
      endif                                                                                                                        
      Ahigh(7) = AX(7) + 1d-9                                                                                                      
      Alow(7) = AX(7) - 1d-9                                                                                                       
      Ahigh(8) = AX(8) + 1d-9                                                                                                      
      Alow(8) = AX(8) - 1d-9                                                                                                       
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
!     Do the minimisation if necessary                                                                                             
                                                                                                                                   
                                                                                                                                   
      Ifail = 1                                                                                                                   
                                                                                                                                   
      Nparm = 8                                                                                                                    
      Ibound = 0                                                                                                                   
      LIW = 12                                                                                                                     
      LW = 130                                                                                                                     
                                                                                                                                   
                                                                                                                                   
       if (tScan(curve,'NnOo') .eq. 0) then ! ie not if only mean or Ockham                                                        
        Call E04JAF(Nparm,Ibound,Alow,Ahigh,AX,SSQ,IW,LIW,WX,LW,Ifail)
        if ((Ifail .ne. 0) .and. (ifail .ne.5)) then

          

! **fix at home           write(Text(1), '( '' SRR Fit:', (I3)'   ) Ifail
          write(*,*) 'SRR fail ',Ifail

          Call Concat(Text(1),HW(24,Language),Text(1)) 
          Call Screen_out_a(text,10,1)
         endif
        else                                                                                                                        
          Ifail = 0  
       endif                                                                                                                       
                                                                                                                                   
                                                                                                                                   
       Call Funct2(Nparm,AX) ! puts the residual vector in common memory                                                           
                                                                                                                                   
                                                                                                                                   
       do i =1,8                                                                                                                   
         SRRParm(i) = AX(i)  ! puts the parameters in common                                                                       
       enddo                                                                                                                       
                                                                                                                                   
                                                                                                                                   
       return                                                                                                                      
       end ! of routine Fit New SRR                                                                                                
                                                                                                                                   
                                                                                                                                   
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                             
                                                                                                                                   
                                                                                                                                   
! //////////////////////////////////////////////////////////////////////                                                           
!                                                                                                                                  
      Subroutine FUNCT2(NP,XC)                                                                                                     
!                                                                                                                                  
! //////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                   
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
                                                                                                                                   
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                   
! //////////////////////////////////////////////////////////////////////                                                           
!                                                                                                                                  
      Subroutine FUNCT1(NP,XC,FC)                                                                                                  
!                                                                                                                                  
! //////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                   
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
                                                                                                                                   
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
! ////////////////////////////////////////////////////////////////////                                                             
                                                                                                                                   
      Double Precision Function SRRFUNCT(XJ,SSB)                                                                                   
                                                                                                                                   
!////////////////////////////////////////////////////////////////////                                                              
!                                                                                                                                  
!    Returns predicted Recruitment for given SSB                                                                                   
!                                                                                                                                  
      double precision X(8), XJ(8), SSB, recr, G05DEF, AVrec, Slope                                                                
      integer i                                                                                                                    
                                                                                                                                   
!                                                                                                                                  
!       X(6) is mean of log recruitments                                                                                           
!       X(7) is standard deviation of log recruitments                                                                             
!       X(8) is lowest observed SSB                                                                                                
!                                                                                                                                  
!                                                                                                                                  
      do i = 1,4                                                                                                                   
        X(i) = dexp(XJ(i))- 1d-8                                                                                                   
      enddo                                                                                                                        
        if (XJ(7) .gt. 90d0) then ! not ockham model fitted                                                                        
        if (dexp(XJ(6)) .lt. 1d0) then                                                                                             
          recr = (X(1)*SSB)                                                                                                        
          recr = recr/(1d0+ (SSB/X(3))**X(4) ) * dexp(-X(2)*SSB)  
        else                                                                                                                       
          recr = dexp(XJ(6))                                                                                                       
        endif                                                                                                                      
      else  ! Ockham model                                                                                                         
!        write (*,*) 'SRR : OCKHAM '                                                                                               
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
                                                                                                                                   
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
! ///////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                   
       Double precision function SQSAFE(X)                                                                                         
                                                                                                                                   
! ///////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                   
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
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
! //////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                   
      Subroutine SetMOSW ( doMC, endyear )                                                                                         
                                                                                                                                   
! /////////////////////////////////////////////////////////////////////                                                            
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
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
! //////////////////////////////////////////////////////////////////////////                                                       
                                                                                                                                   
        Subroutine GenFMult(startsim, endsim) ! Generate stochastic F constraint matrix                                            
                                          ! using the data in TargetFM and CVTarg                                                  
                                          ! note the startsim and endsim referenced                                                
                                          ! so that lastyear = 1                                                                   
                                                                                                                                   
! /////////////////////////////////////////////////////////////////////////                                                        
                                                                                                                                   
        implicit none                                                                                                              
        include 'indat.inc'                                                                                                        
        include 'Projc3.inc'                                                                                                       
                                                                                                                                   
        integer iyear, ifleet, startsim, endsim                                                                                    
        double precision expectation, G05DEF                                                                                       
                                                                                                                                   
        do ifleet = 1,Nfleet                                                                                                       
          do iyear = startsim, endsim                                                                                              
            expectation = dlog( dabs(dble(TargetFM(ifleet,iyear)))) +   &                                                          
            ( (CVTarg(ifleet,iyear)*CVTarg(ifleet,iyear))/2d0 )                                                                    
            ConsCat(ifleet, iyear) =                                    &                                                          
               G05DEF(expectation,dble(CVTarg(ifleet,iyear)))                                                                      
            if (TargetFM(Ifleet,iyear) .lt. 0d0) then                                                                              
              ConsCat(ifleet,iyear) = -ConsCat(ifleet,iyear)                                                                       
            endif                                                                                                                  
          enddo ! years                                                                                                            
        enddo  ! fleets                                                                                                            
                                                                                                                                   
        return                                                                                                                     
                                                                                                                                   
        end ! of subroutine GenFmult                                                                                               
                                                                                                                                   
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                         
                                                                                                                                   
                                                                                                                                   
! ///////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                   
       Subroutine  OverWritePop                                                                                                    
                                                                                                                                   
! /////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                  
!       Allows the user to over-write certain populations                                                                          
!       parameters and parameter estiamtes                                                                                         
!                                                                                                                                  
!                                                                                                                                  
        implicit none                                                                                                              
        include 'indat.inc'                                                                                                        
        include 'sepmodel.inc'                                                                                                     
        include 'message1.inc'                                                                                                     
                                                                                                                                   
        integer i, year, age                                                                                                       
        double precision input_N, NEW_VALUE, Nmin,Nmax                                                                             
        logical error                                                                                                              
                                                                                                                                   
        integer tscan                                                                                                              
        external tscan                                                                                                             
                                                                                                                                   
        character*1 dummy                                                                                                          
        dummy = ' '                                                                                                                
                                                                                                                                   
                                                                                                                                   
        call Screen_in_a(HW(37,Language), dummy,KO(1,Language),1)                                                                  
                                                                                                                                   
        if (tScan(dummy, KY(1,Language)) .ne. 0) then                                                                              
          year=lastyear                                                                                                            
          do while (year .ne. -1)                                                                                                  
            Nmin=0d0                                                                                                               
            Nmax=1d25                                                                                                              
            year=lastyear                                                                                                          
            age=firstage                                                                                                           
            Input_N=0d0                                                                                                            
            call Screen_in_IR( HW(38,1), year,age,input_N,lastyear+1,firstyear,lastage,firstage,Nmax,NMin,Language)                
            Nmin=-1d30                                                                                                             
            Nmax= 1d30                                                                                                             
          enddo                                                                                                                    
          i=1                                                                                                                      
          do while (i.ne.-1)
            new_value=Nmin
            call Screen_in_IR2( HW(39,1), i,new_value,nxparm,0,Nmax,NMin,Language)                                                 
            if (i.ne.-1) Xbest(i) = dlog(new_value)                                                                                
          enddo  ! next parameter                                                                                                  
        endif  ! chosen to overwrite                                                                                               
        return                                                                                                                     
        end                                                                                                                        
                                                                                                                                   
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                   
                                                                                                                                   
                                                                                                                                   
! //////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                   
       Subroutine SetREcUsage                                                                                                      
                                                                                                                                   
! ///////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                  
!  Decide whether to use ICA or stock-recruit model estimates                                                                      
!                                                                                                                                  
       implicit none                                                                                                               
       include 'indat.inc'                                                                                                         
       include 'projc3.inc'                                                                                                        
       include 'message1.inc'                                                                                                      
       character*77 text                                                                                                           
       character*5 ytext                                                                                                           
       integer tscan                                                                                                               
       external tscan                                                                                                              
       character*1 dummy                                                                                                           
                                                                                                                                   
       UseRec1= .false.                                                                                                            
       UseRec2 =.false.                                                                                                            
                                                                                                                                   
       write(ytext, '(I4)') lastyear                                                                                               
       Call ConCat(Text,HW(59,Language),ytext)
       dummy='i'
       call Screen_in_a(Text,dummy,HW(60,Language),Language)                                                                       
                                                                                                                                  
       if (Tscan(dummy, HW(61,Language)) .ne. 0) UseRec1=.true.                                                                    
                                                                                                                                   
       write(ytext, '(I4)') lastyear+1                                                                                             
       Call ConCat(Text,HW(59,Language),ytext)                                                                                     
       dummy='i'
       call Screen_in_a(Text,dummy,HW(60,Language),Language)
                                                                                                                                   
       if (Tscan(dummy, HW(61,Language)) .ne. 0) UseRec2=.true.                                                                    
                                                                                                                                   
       return                                                                                                                      
       end                                                                                                                         
                                                                                                                                   
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
