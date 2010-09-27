! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      Subroutine ICA2(Params,VCV) !  Fit the full model by least-squares

! ///////////////////////////////////////////////////////////////////////                                                           
!                                                                                                                                   
!       Written by K.R. Patterson,                                                                                                  
!                  Marine Laboratory,                                                                                               
!                  P.O. Box 101,                                                                                                    
!                  Aberdeen,                                                                                                        
!                  Scotland,                                                                                                        
!                  UK                                                                                                               
!                  Fax : 01224 295511                                                                                               
!                  Tel : 01224 295507                                                                                               
!                  email : pattersonkr@marlab.ac.uk                                                                                 
!                                                                                                                                   
!                  Version 1.3b  dated December 1997                                                                                
!                  Version 1.4x date July 2007                                                                                                                 
!      Modified by M. Kienzle - Marine Laboratory - Aberdeen - Scotland
!                  e-mail: marco.kienzle@gmail.com
!                                                                                                                                   
! /////////////////////// MODIFICATIONS /////////////////////////////////                                                           
! MK - Jan-May 2005: replace calls to the NAG library by calls to PORT lib
! MK - July 2007: the DN2F minimization function from PORT library has never produced the 
!                 exact same results than e04fyf from the NAG library. Therefore it was replaced
!                 by the lmdif1 subroutine from the MINPACK library.
! MK - End of 2007: finally NAG was replaced by MINUIT from cernlib
! /////////////////////// TO DO /////////////////////////////////////////                                                           
!   create a routine that replace the Report for displaying the diagnostic of the fit                                                                                                                                
! ///////////////////////////////////////////////////////////////////////                                                                                                                                   
!          Program Description:                                                                                                     
!          ====================                                                                                                     
!                                                                                                                                   
!          The program implements a Deriso-Gudmundsson type integrated catch-at-age analysis.                                       
!          It relies upon a prior run of ICA1 for data reading, choice of options                                                   
!          and finding an approximate minimum from which to begin the search. Data and                                              
!          parameters are read from a file ICA.TMP which is written by ICA1.                                                        
!                                                                                                                                   
! OBSOLETE          The solution is found in three concentric loops , as below (the third loop is within                                     
! OBSOLETE                             NAG routine, which solves for F,S and N by modified Newton method):                                  
!          The solution is found in three concentric loops , as below (the third loop is within                                     
!                              MINPACK routine, which solves for F,S and N by a modified levenberg-marquardt algorithm method)

!                      Set weights all = 1, (or set by user)                                                                        
!                                 Recalculate until F changes by less than specified amount                                         
!                                 .                                                                                                 
!                                 .    Recalculate until SSQ changes by less than 0.05%                                             
!                                 .    .   fit separable model using NAG routine                                                    
!                                 .    .   update the conventional VPA using populations & selection pattern from separable model   
!                                 .    Iterate                                                                                      
!                                 .                                                                                                 
!                                 Calculate new weights                                                                             
!                                 Iterate                                                                                           
!                      write outputs                                                                                                
!                      stop                                                                                                         
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!     Program calls:      Hello                  displays start-up screen                                                           
!                         ReadBlock              reads contents of ICA.TMP into common blocks                                       
!                         CalcStats              calculates variances etc.                                                          
                                                                                                                                    
!                         DisplayWts             display the survey index weights on screen                                         
!                         LSFUN1                 objective function, calculates residual vector for                                 
!                                                  given parameter vector. Also, if writeout = true then                            
!                                                  the residuals are written to a file ICA.RES. If Full = true                      
!                                                  then a conventional VPA is calculated for all the years of data.                 
! OBSOLETE                         E04FDF                 NAG routine to minimise sums of squares                                            
! OBSOLETE                         E04YCF                 NAG routine to approximate variance-covariance matrix                              
!                         DN2F                   PORT routine to solve non-linear least square problem
!                                                and calculate the variance-covariance matrix                                                                                                           
! OBSOLETE BUT NOT REPLACED                       Report                 display NAG diagnostics of fit                                                     
!                         WriteVCV               write variance-covariance matrix to file (Modified by MK)                                           
!                         TableOut               Write output and data transfer file                                                
!                         ShrinkF                calculates shrunk VPA, writes to file ICA.SHR                                      
!                                                                                                                                   
!      Use MSWIN32
!      Use CLRWIN
	

      implicit none

      Include "INDAT.INC"                                                                                                           
      Include "SEPMODEL.INC"                                                                                                        
      Include "STATS.INC"                                                                                                           
      Include "SRR.INC"                                                                                                             
      Include "LABELS.INC"                                                                                                          
      Include "MESSAGE1.INC"                                                                                                       

! Externalal SUBROUTINES
     
     external OBJECTIVEFCN, covar

!      DEFINE global variables
       COMMON/global_int/nodats,noparms;
                                                                                                                 
!     Local variables                                                                                                               
                                                                                                                                    
! added for NAG call E04FYF in place of old E04FDF (01 Dec. 2004)
!
      integer IUSER(1)
      real    USER(1)

! end of addidtion

      character*5 ytext                                                                                                             
      character*9 ntext                                                                                                             
      character*76 Text(5)                                                                                                          
      double precision Resids(maxdata)                                      ! Residuals                                             
      integer  i, ii, j, ik, iage   
      character*1 dummy                                                                                                             
      integer age, index                                                                                                            
!      double precision params(maxparm)                                      ! Fitted parameters, as passed to E04 routines         
double precision params(46)
      double precision ssq, SSQOld, endsq                                   ! ssq for anova table                        
      integer liw, lw, ifail                                   ! Workspace for E0f routines  
!integer iw(maxdata)                           
      integer  nv, ns, job, ifaily                                          ! used in e04YCF call                                       
      double precision cj(maxparm)                                          ! not used; reqd for e04YCF syntax                     
      double precision Wk                                               &                                                           
       ((7*maxparm+maxparm*maxparm+2*maxparm*maxdata+                   &                                                           
              3*maxdata + maxparm*(maxparm-1)/2))                                                                                   
      double precision Wk2(maxparm)                                         ! E04YCF workspace                                     
      double precision VCV(maxparm, maxparm)                            
!      real*4  Corr(maxparm,maxparm)                                    ! Parameter correlation matrix                   
      double precision  Var1                                            ! Temporary variables used in calculating parameter correlat
      integer test, nodats, noparms                                     ! no of data, no of parameters                              
      logical firstrun                                                  ! skips E04 calls in first reweighting loop: weights recalcu
      integer RWOpt, it                                                 ! Reweighting option, iteration number                      
      double precision  IxCor(maxsurvey), LastF, FmTol                  ! Index Weights working variable, Internal correlati
      integer iyr,imon,iday, ihr, imin, isec, ith                       ! System time and date                                      
      real Time                                                         ! Time as real; includes day of week in case of overnight ru
                                                                                                                                    
      double precision mincv, MaxWeight,MinTol,MaxTol                   ! Minimum cv of mean for shrinkage                          
      logical shrink                                                    ! whether or not to shrink                                  
      integer shrinkyrs                                                 ! number of years to shrink to                              
      integer tscan                                                     ! The tscan function replaces Microsoft 'SCAN'         

! DEFINING THE WORKSPACE VARIABLE THE DN2F MINIMIZATION FUNCTION (PORT library)

      INTEGER LIV,LV, UI(1) 	       
      PARAMETER (LV = 105+maxparm*(maxdata+2*maxparm+17)+2*maxdata)
	  PARAMETER (LIV = 82+maxparm)  
	  INTEGER IV(LIV)
	  DOUBLE PRECISION V(LV)
	  REAL UR(1)


! DEFINING THE WORKSPACE VARIABLE FOR LMDIF1 - MINPACK MINIMIZATION FUNCTION
INTEGER lwa

DOUBLE PRECISION fvec(nxdata), wa(nxdata * nxparm + 5 * nxparm + nxdata)
INTEGER info, iw(nxparm), iwa(nxparm)
DOUBLE PRECISION tol
PARAMETER (tol=1.0D-14)

! DEFINING THE WORKSPACE FOR MINUIT
integer ierflg
integer ARGLIS(4), zero
double precision step

ierflg=0
step=0.01
zero=0
!      DIMENSION NPRM(2),VSTRT(2),STP(2),ARGLIS(4),ALIM(2),BLIM(2)
!      CHARACTER*10 PNAM(2)

!______________________________EXECUTABLE CODE OF ICA2______________________                                                        
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ------- DYNAMIC MEMORY ALLOCATION --------------------------------                                                                
                                                                                                                                    
      write(*,*) 'Start of ICA2'
      nodats = nxdata
      noparms = nxparm          

! Initialize lwa is a positive integer, a parameter of lmdif1 algorithm from the MINPACK library
      lwa= nxdata * nxparm + 5 * nxparm + nxdata



!DEBUG      write(*,*) 'nodats is', nodats
!DEBUG      write(*,*) 'noparms is', noparms

                                                                                                                             


                                                                                                                                    
                                                          ! allocate is Microsoft FORTRAN dialect                                   
      test=0                                                                                                                        
      if (test .gt. 0) then ! there's a memory problem                                                                              
        Text(1)= HM(1,Language)                                                                                                     
        Call Screen_out_a(Text,5,1)                                                                                                 
        stop                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
      RWOpt = 0                                           ! reweighting option set to null value                                    
      UseSep = .true.                                     ! always fit a separable model                                            
                                                                                                                                    
      dummy=KO(3,Language)  
      Call Screen_in_a(HM(2,Language),dummy,KO(3,Language),Language)                                                                
!      write(*,*) dummy, KY(3,Language), Tscan(dummy, KY(3,Language))                                                               
      if (Tscan(dummy, KY(3,Language)) .eq. 0) then                                                                                 
        RWOpt = 1                                                                                                                   
        Maxweight=1d0
        Call Screen_in_r(HM(48,Language),MaxWeight,10d0,0.5d0,          &
            Language)                                                                                                               
                                                                                                                                    
      else                                                                                                                          
        RWOpt = 2                                                                                                                   
                                                                                                                                    
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      if (RWOpt .eq. 1) then !-------------------------- set starting weights all = 1                                               
        do index = 1, nssbix ! ------------------------- but will be changed iteratively                                            
           Blambda(index) = 1d0                                                                                                     
        enddo                                                                                                                       
        do index = 1, Nageix                                                                                                        
          do age= fage(index), lage(index)                                                                                          
            Alambda(index, age-fage(index)+1) = 1d0                                                                                 
          enddo                                                                                                                     
        IxCor(index) = 0.5d0                                                                                                        
        enddo                                                                                                                       
        if (FitSRR) then                                                                                                            
          SRRLambda = 1d0                                                                                                           
        endif                                                                                                                       
      endif ! Reweighting option = 1                                                                                                
                                                                                                                                    
                                                                                                                                    
      if (RWOpt .eq. 2) then ! ------------------------ Weights to be read from screen and fixed                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
        do index = 1, nssbix                                                                                                        
          Call Concat(Text(1),HM(3,Language),BsurvLab(index))
          Blambda(index)=1d0
          CAll Screen_in_r(Text(1),Blambda(index),10d0,1d-8,Language)                                                               
        enddo                                                                                                                       
        do index =1, Nageix                                                                                                         
         do age=fage(index),lage(index)                                                                                             
          Call Concat(Text(1),HM(3,Language),AsurvLab(index))                                                                       
          Call Concat(Text(1),Text(1),HM(4,Language))                                                                               
          Call IntToChar(age,ytext(1:4),4)                                                                                          
          Call Concat(Text(1),Text(1),ytext(1:4))                                                                                   
          Alambda(index,age-fage(index)+1)=1d0
          CAll Screen_in_r(Text(1),Alambda(index,age-fage(index)+1)     &
           ,10d0,1d-8,Language)                                                                                                     
          enddo ! ages                                                                                                              
        enddo ! indices                                                                                                             
                                                                                                                                    
        if (FitSRR) then ! --------------------------- this is not strictly correct, never mind
          SRRLambda=0.1d0
          CAll Screen_in_r(HM(5,Language),SRRlambda,10d0,1d-8,Language)                                                             
        endif                                                                                                                       
                                                                                                                                    
      if (nageix .gt. 0) then                                                                                                       
        do ik =1,4                                                                                                                  
          Text(ik)=HM(ik+5,Language)                                                                                                
        enddo                                                                                                                       
                                                                                                                                    
        Call Screen_out_a(Text,5,4)                                                                                                 
                                                                                                                                    
                                                                                                                                    
        do index = 1,nageix                                                                                                         
           Call ConCat(Text(1),HM(10,Language),ASurvlab(index))
           IxCOr(index)=1d0
           Call Screen_in_r(Text(1),IxCor(index),1d0,0d0,Language)                                                                 
           IxCor(index) = 1. - IxCor(index)                                                                                         
        enddo  ! indices                                                                                                            
      endif    ! any age-structured indices                                                                                         
                                                                                                                                    
                                                                                                                                    
      endif ! RWOpt = 2  , weights to be fixed by hand.                                                                             
                                                                                                                                    
                                                                                                                                    
        !------------------------ Take square roots of the weights, for computational efficiency                                    
                                                                                                                                    
      do index=1,nssbix                                                                                                             
        Blambda(index) = dsqrt(Blambda(index))                                                                                      
      enddo  ! indices of SSB                                                                                                       
                                                                                                                                    
      do index = 1,nageix                                                                                                           
          do iage=1,lage(index)-fage(index)+1                                                                                       
            Alambda(index,iage)=dsqrt(Alambda(index,iage) *             &                                                           
            (IxCor(index)*dble(lage(index)-fage(index))+1.) /           &                                                           
                         dble(lage(index)-fage(index)+1))                                                                           
          enddo  ! ages                                                                                                             
      enddo     ! years                                                                                                             
                                                                                                                                    
      If (FitSRR) SRRLambda = dsqrt(SRRLambda)                                                                                      
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
9050  format (A1)                                                                                                                   
9075  format (' ', A37, ' --> ')                                                                                                    
                                                                                                                                    
                                                                                                                                    
         ! -----------------------  Whether or not shrink, and associated options                                                   
                                                                                                                                    
      shrink = .false.                                                                                                              
      dummy = 'N'                                                                                                                   
                                                                                                                                    
      CAll Screen_in_a(HM(11,Language),dummy,KO(1,Language),Language)                                                               

      shrinkyrs = 5
      MinCV=0.2d0
      if (tscan(dummy, KY(1,Language)) .ne. 0) then                                                                                 
        Call Screen_in_i(HM(12,Language),shrinkyrs,                     &                                                           
             lastyear-firstyear+1,2,Language)                                                                                       
        Call Screen_in_r(HM(13,Language),MinCV,1d0,0d0,Language)                                                                    
        shrink = .true.                                                                                                             
      endif  ! have chosen to shrink                                                                                                
                                                                                                                                    
                                                                                                                                    
      IF (RWOpt .ne. 2) then
        MinTol=1d-8
        MaxTol=0.1d0
        FMTol = 0.05d0 
        Text(1)=HM(14,Language)                                                                                                     
        Text(2)=HM(15,Language)                                                                                                     
        Call Screen_out_a(Text,5,2)                                                                                                 
        Call Screen_in_r(HM(16,1),FmTol,MaxTol,MinTol,Language)  
      ELSE                                                                                                                          
        FMtol=1d-9                                                                                                                  
      ENDIF                                                                                                                         
                                                                                                                                    
                                                                                                                                    
! ---------------------------------- Weighting options now completed,                                                               
! ---------------------------------- next some information for the user.                                                            
                                                                                                                                    
                                                                                                                                    
      firstrun = .true.                                                                                                             
      writeout = .false.                                                                                                            
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      LastF = 10d0            ! --------------- F in previous iteration set to out-of range value at start;                         
                              ! --------------- the inverse-weighting iterations cease when this changes by less than               
                              ! --------------- a specified proportion                                                              
      it = 0                  ! this variable is to record the number of inv. wt.  VPA iterations                                   
                                                                                                                                    
                                                                                                                                    
      Text(1)=HM(17,Language)                                                                                                       
      Call Screen_out_a(Text,5,1)                                                                                                   

      do i = 1, noparms                                              ! Update the parameter list                                    
        Params(i) = (Xbest(i))                                                                                                    
      enddo                                                                                                                     


                                                                                                                                    
! --------------------------------------------------------------------                                                              
!         ANALYSIS RESTARTS FROM HERE ON REWEIGHTING                                                                                
! --------------------------------------------------------------------                                                              
                                                                                                                                    
      DO WHILE ((dabs(dexp(Params(NySep))-LastF)/LastF .gt. FmTol)  & ! Continue as long as the F is changing by more than the       
        .and. ( .not. (( it .ge. 1) .and. (RWOpt .eq. 2)) ) )        ! specified amount, unless no inverse-variance reweighting is  
                                                                     ! specified. In this latter case, do one loop only.            
      if (.not. firstrun) LastF = dexp(Params(NySep))                ! update the LastF from previous run                           
      it = it + 1                                                    ! update the number of iv wt iterations                        
                                                                                                                                    
      Call DisplayWts                                                ! show weights on the screen                                   
                                                                                                                                    
                                                                                                                                    
      do i = 1, noparms                                              ! Update the parameter list                                    
        Params(i) = (Xbest(i))                                                                                                      
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ----------------------------------------------------------------------                                                            
! ------------------------------------- CALCULATE STARTING RESIDUALS                                                                
! ----------------------------------------------------------------------                                                            
                                                                                                                                    

!------ SETTING UP THE PARAMETER FOR THE DN2F MINIMIZATION FUNCTION

!          UI(1) = 50
!	  IV(1) = 0 ! RUN THE OPTIMIZATION WITH DEFAULT VALUES

!------ CONTROLS FOR THE MINIZATION ALGORITHM

!	   CALL DIVSET(1, IV,LIV,LV,V)

!------	PRINTING CONTROLS

!	   IV(19) = 0
!	   IV(14) = 1
!	   IV(21) = 0 ! Turn all printing off: comment to have the covariance matrix and the diagnostic array printed

!------ FURTHER SETTINGS

!	   V(38)  = 1.0
!	   IV(16) = 0

!	   IV(18) = 10 ! MAX NUMBER OF ITERATION, DEFAULT=150

!------   Request the Hessian to be the product of the Jacobian (this matter for the var/cov matrix)
!	   IV(15) = 3


!  ------------------------- FOLLOWING LOOP IS THE ACTUAL MODEL FITTING FOR GIVEN WEIGHTS                                           
	write(*,*) 'I am just before call LSFUN1 for 1st time'
!	M.K.: swap the order of the first 2 arguments to enable MINUIT TO WORK
!        Call LSFUN1(nodats, Noparms, Params, Resids)                                                                                
        Call LSFUN1(Noparms, nodats, Resids, Params)                                                                                
        endsq = 0d0                                                                                                                 
        do i = 1, nodats                                                                                                            
          endsq = endsq+Resids(i)*Resids(i)                                                                                         
        enddo                                                                                                                       
!!!!!!!!!!!!!!!!!!!! ATTEMPT TO USE MINPACK
        SSQOld = 1d0                                      ! ----------arbitrary to make sure it starts                              
        SSQ = endsq                                       ! ----------SSQ from starting position estimated by prior fit             
        do while ( dabs(SSQ-SSQOld)/SSQOld .gt. 0.0005d0) ! --------- Keep going until the SSQ changes by less than 0.05%           
!
          write(*,*) 'MINPACK Minimising SSQ ----> ', SSQ         ! --------- for debugging                                                 
          SSQOld = SSQ                                                                                                              
!          ifail = 1                                                  !------------------------ needed to keep going; see NAG routine
          Full = .false.
!write(*,*) nodats, noparms, SSQ, params(1), params(2) ! DEBUG
!DEBUG          write(*,*) nodats, noparms, SSQ, liw, lw, ifail            !------------------------ don't recalculate the full VPA in eac

!          call e04fdf(nodats, noparms,params,SSQ,iw,liw,wk,lw,ifail) !------- NAG routine Pre mark 19 : fit the separable model            !          call e04fyf(nodats, noparms,LSFUN,params,SSQ,wk,lw,IUSER,USER,ifail) !------- NAG routine mark 19: fit the separable model    

!          DN2F COMPUTE THE VAR/COV MATRIX AND STORES THE RESULTS IN ARRAY V
!           call DN2F(nodats,noparms,params,LSFUN,IV,LIV,LV,V,UI,UR,INUTILE) !-------- PORT minimization routine: fit the separable model             
!          write(*,*) 'SSQ --- > ', 2*V(10) ! DN2F minimises 0.5 * LSFUN ^ 2
!	  SSQ = 2*V(10)                    ! DN2F minimises 0.5 * LSFUN ^ 2

info=1
!	write(*,*) 'Calling lmdif1'
!call lmdif1(LSFUN1, noparms, nodats, params, fvec, tol, info, iw, wa, lwa)
!         Full = .true.                                              !------------------------ but recalculate the full VPA afterwar

!          DN2F COMPUTE THE VAR/COV MATRIX AND STORES THE RESULTS IN ARRAY V
!           call DN2F(nodats,noparms,params,LSFUN,IV,LIV,LV,V,UI,UR,INUTILE) !-------- PORT minimization routine: fit the separable model           

!DEBUG	write(*,*) 'Parameters', Params

!          write(*,*) 'CALLING LSFUN1 ..' ! To debug	
!	write(*,*) 'I am just before call LSFUN1 for 2nd time'

!	M.K.: swap the order of the first 2 arguments to enable MINUIT TO WORK
!          Call LSFUN1(nodats, Noparms, Params, Resids)        
          Call LSFUN1(Noparms, nodats, Resids, Params)        

! RECALCULATE THE SSQ

       endsq = 0d0                                                                                                                 


        do i = 1, nodats                                                                                                            
          endsq = endsq+Resids(i)*Resids(i)                                                                                         
        enddo                                                                                     
! Old and new SSQ
 
            SSQOld = SSQ
            SSQ = endsq

!DEBUG write(*,*) 'MINPACK Old SSQ is ', SSQOld
!DEBUG write(*,*) 'MINPACK new SSQ is ', SSQ
!DEBUG write(*,*) 'MINPACK Condition ', dabs(SSQ-SSQOld)/SSQOld


        enddo                                                                                                                       
!write(*,*) 'WARNING: LMDIF return INFO =', info


!!!!!! AN ATTEMPT TO USE PORT
!
!        SSQOld = 1d0                                      ! ----------arbitrary to make sure it starts                              
!        SSQ = endsq                                       ! ----------SSQ from starting position estimated by prior fit             
!        do while ( dabs(SSQ-SSQOld)/SSQOld .gt. 0.0005d0) ! --------- Keep going until the SSQ changes by less than 0.05%           

!          write(*,*) 'Minimising SSQ ----> ', SSQ         ! --------- for debugging                                                 
!          SSQOld = SSQ                                                                                                              
!          ifail = 1                                                  !------------------------ needed to keep going; see NAG routine
!          Full = .false.
!write(*,*) nodats, noparms, SSQ, params(1), params(2) ! DEBUG
!DEBUG          write(*,*) nodats, noparms, SSQ, liw, lw, ifail            !------------------------ don't recalculate the full VPA in eac

!          call e04fdf(nodats, noparms,params,SSQ,iw,liw,wk,lw,ifail) !------- NAG routine Pre mark 19 : fit the separable model            !          call e04fyf(nodats, noparms,LSFUN,params,SSQ,wk,lw,IUSER,USER,ifail) !------- NAG routine mark 19: fit the separable model    

!          DN2F COMPUTE THE VAR/COV MATRIX AND STORES THE RESULTS IN ARRAY V
!           call DN2F(nodats,noparms,params,LSFUN,IV,LIV,LV,V,UI,UR,INUTILE) !-------- PORT minimization routine: fit the separable model             
!          write(*,*) 'PORT SSQ --- > ', 2*V(10) ! DN2F minimises 0.5 * LSFUN ^ 2
!	  SSQ = 2*V(10)                    ! DN2F minimises 0.5 * LSFUN ^ 2
!
!          Full = .true.                                              !------------------------ but recalculate the full VPA afterwar
!
!	write(*,*) 'Parameters', Params

!          write(*,*) 'CALLING LSFUN1 ..' ! To debug	
!          Call LSFUN1(nodats, Noparms, Params, Resids)        

! RECALCULATE THE SSQ

!       endsq = 0d0                                                                                                                 
!
!
!       do i = 1, nodats                                                                                                            
!          endsq = endsq+Resids(i)*Resids(i)                                                                                         
!       enddo                                                                                     
! Old and new SSQ
! 
!            SSQOld = SSQ
!            SSQ = endsq
!
!write(*,*) 'Old SSQ is ', SSQOld
!write(*,*) 'new SSQ is ', SSQ
!write(*,*) 'Condition ', dabs(SSQ-SSQOld)/SSQOld

!        enddo                                                                                                                       
!
!           write(*,*) 'PORT Minimisation routine finished with code', IV(1)
!	   CALL Report(IV(1)) ! Report any failure of the minimization algorithm
        


!!!!! ATTEMPT TO USE MINUIT

!     Initialise MINUIT, define I/O unit numbers
      CALL MNINIT(5,6,7)

!     Define the parameters: no limits are used as advised in the reference manual
!      ZERO=0.
!      ONE=1.
write(*,*) 'The number of parameters to pass to MINUIT is ', Noparms
      DO 11 I= 1, Noparms
!write(*,*) 'Parameter number',I, ' is ', params(I)
         CALL MNPARM(I,'Par',params(I),step,params(I) - 1000,params(I) + 1000,IERFLG)
         IF (IERFLG .NE. 0) THEN
            WRITE (6,'(A,I)') 'UNABLE TO DEFINE PARAMETER NO.',I
            STOP
         ENDIF

 11   CONTINUE     

write(*,*) 'The number of parameters to passed to MINUIT is ', Noparms
!     Set the title
      CALL MNSETI('Fitting ICA')

!     Request FCN to read in data (IFLAG=1)
      ARGLIS(1) = 1.
      CALL MNEXCM(OBJECTIVEFCN, 'CALL FCN', ARGLIS ,1,IERFLG,0)


write(*,*) 'BEFORE SETTING THE PRINT LEVEL'
!
!     SET THE PRINT LEVEL FROM MINUIT
!
      ARGLIS(1) = 1.
      CALL MNEXCM(OBJECTIVEFCN, 'SET PRINT' , ARGLIS, 1, IERFLG,0)
!
!	Estimation of the Hessian matrix
!

      CALL MNEXCM(OBJECTIVEFCN, 'hes' , zero, 0, IERFLG,0)


!
!     MINIMIZATION OF THE FUNCTION BY METHOD MINOS (DO NOT REQUIERE THE FIRST DERIVATIVE OF THE FUNCTION)
!

      CALL MNEXCM(OBJECTIVEFCN, 'MIGRAD' , ARGLIS, 0, IERFLG,0)

!      THE COMMAND BELOW CREATES A DISCREPANCY IN THE NUMBER OF PARAMETERS!      CALL MNEXCM(OBJECTIVEFCN, 'MINOS' , ARGLIS, 0, IERFLG,0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! kienzlem: 10/05/2005: THIS IS OBSOLETE BECAUSE DN2F COMPUTES THE VAR/COV MATRIX TOGETHER WITH THE PARAMETER ESTIMATES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODEL HAS BEEN FITTED: NOW THE VAR-COVAR MATRIX ESTIMATE                                                                          
!                      See NAG Documentation to explain this                                                                        
!                                                                                                                                   
!                                                                                                                                   
!c estimate the uncertainties in parameters
!       call fiterr(LSFUN1, nodats, noparms, lenfvc, mvarys, fvect,
!    $      ftemp, fjac, alpha, iprint, nerstp, xvarys,
!     $      delta, correl, ier, ibadx)
! 
!      write(*,*) 'Computing covariance matrix. Please wait'                                                                        

!!!!!!! COMPUTING THE CO-VARIANCE MATRIX WITH MINPACK
!!!!! IT SEEMS THAT YOU CANNOT COMPUTE FOR MORE THAN 8 PARAMETERS 

! NOT ENOUGH FOR ICA

!      write(*,*) 'The number of data is ', nxdata               
!      write(*,*) 'The number of paramters is ', nxparm               
!      write(*,*) 'The dimension of wa is ', lwa               


!      call covar(n,wa,m,iwa,tol,wa(4*n+1))
!      call covar(9,wa,nxparm,iwa,tol,wa(4*nxdata+1))
                                                                          !       write(*,*) 'Computing covariance matrix is over'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!       NS = 6*noparms + 2*nodats + nodats*noparms + 1 +                 &                                                           
!             max0(1, (noparms*(noparms-1))/2)                                                                                       
!        nv = ns + noparms                                                                                                           
!                                                                                                                                    
!       ifaily = 1                                                                                                                   
!       job = -1                                                                                                                     
!                                                                                                                                    
!       call e04ycf(job, nodats, noparms, ssq, wk(ns), wk(nv), noparms,  &                                                           
!       cj, wk2, ifaily)                                                                                                             
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                                                                                                    
                                                                                                                                    
! --------------------------------------- display the NAG diagnostic message                                                        
                                                                                                                                    
!OBSOLETE      Call Report(Ifail, Ifaily)                                                                                                    
                                                                                                                                    
                                                                                                                                    
      do i = 1, noparms       ! --------------------- copy parameters into common block                                             
        Xbest(i) = Params(i)
     enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      If (FitSRR) then                                                                                                              
        a = dexp(XBest(Noparms-1))                                                                                                  
        b = dexp(Xbest(Noparms))                                                                                                    
        Call WriteSRRFile                                                                                                           
      endif                                                                                                                         
                                                                                                                                    
      firstrun = .false.                                                                                                            
                                                                                                                                    
!------------ ANOVA TABLE CALCULATIONS --------------------------------                                                             
                                                                                                                                    
!   ... Calculation of new inv.-variance weights ...                                                                                
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      If (RWOpt .eq. 1)  CAll UpdateWts(MaxWeight)                                                                                  
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! -----------  Display some diagnostics on screen after each model fit                                                              
                                                                                                                                    
                                                                                                                                    
      Call IntToChar(lastyear,ytext(1:4),4)                                                                                         
      CAll ConCat(text(1),HM(18,Language),ytext(1:4))                                                                               
      CAll ConCat(text(1),Text(1),HM(19,Language))                                                                                  
      Call IntToChar(Refage,ytext(1:3),3)                                                                                           
      CAll ConCat(text(1),Text(1),ytext(1:3))                                                                                       
      call ConCat(text(1),Text(1),HM(20,Language))                                                                                  
      write(ntext, 100) dexp(Params(NySep))                                                                                         
100   format( F8.6 )                                                                                                                
      call Concat(text(1),Text(1),ntext)                                                                                            
      call Concat(text(1),Text(1),HM(21,Language))                                                                                  
      call intToChar(it, ytext(1:2),2)                                                                                              
      call Concat(text(1),Text(1),ytext(1:2))                                                                                       
      call screen_out_a(Text,5,1)                                                                                                   
                                                                                                                                    
                                                                                                                                    
      Call CalcStats(.true.)    ! Calculate the weighted variances etc.                                                             
                                                                                                                                    
                                                                                                                                    
      !----------------------- Note that the wts on the catches at age remain unchanged:                                            
      !                        survey weights are updated RELATIVE to the weights                                                   
      !                        on the catch-at-age observations                                                                     
                                                                                                                                    
                                                                                                                                    
      ENDDO   ! the reweighting iteration loop                                                                                      
                                                                                                                                    

                                                                                                                                    
!-------------------- Model HAS BEEN FITTED AND VCV ESTIMATED                                                   
                                                                                                                                    
      Call WriteVCV(V, LV, IV(26) , noparms)  !-------------- write var-covar matrix to file
!OBSOLETE      Call WriteVCV(Wk, lw, nv, noparms)  !-------------- write var-covar matrix to file                                            
                                                                                                                                    
                                                                                                                                    
! TRANSFORM THE 1D ARRAY V INTO A 2D ARRAY VCV
! DN2F returns a triangle matrix 
! with variances in position 1, 3, 6, 10, 15, etc...                                                                                                                                    

      do i = 1, noparms                                                                                                             
       do j=1, noparms
	   if(i >= j) then 
		VCV(i,j) = V(IV(26) + (i-1)*i/2 + (j-1))
       else
	    VCV(i,j) = V(IV(26) + (j-1)*j/2 + (i-1))
		endif
         enddo                                                                                                                     
      enddo                                                                                                                         

!-------------------------------------------------------- CALCULATE THE PARAMETER S.D.s                                             
!      do i = 1, noparms       ! --------------------- see NAG routine documentation for the way in which the VCV matrix             
!          Var1 = Wk(nv+(i-1)+((i-1)*noparms))   !---- stored in the Wk workspace vector                                             

	ii = IV(26) - 1
	DO 10 i = 1, Noparms
	ii = ii + i
    Var1 = V(ii)

!TEST	WRITE (*, *) 'VAR ', V(ii)
          if (Var1 .gt. 0d0) then                                                                                                   
            Var1 = dsqrt(Var1)                                                                                                      
          else                                                                                                                      
            Var1 = 0d0                                                                                                              
           write(*,*) 'Negative variance error in ICA2.'                                                                            
          endif                                                                                                                     
          XHigh(i) = Xbest(i) + Var1                                                                                                
          XLow(i)  = Xbest(i) - Var1                                                                                                 

10	CONTINUE			
!      enddo                                                                                                                         
                                                                                                                                    
!      write(*,*) 'CVs calculated OK'                                                                                               
                                                                                                                                    
																																	                                                                                                                                    
      !------------------------- To reduce the number of arithmetical operations in                                                 
      !                          the minimisation, the weights are stored as the square roots of                                    
      !                          the weights. They must be detransformed before printing.                                           
                                                                                                                                    
                                                                                                                                    
      do index=1,nssbix                                                                                                             
        Blambda(index) = Blambda(index)*Blambda(index)                                                                              
      enddo                                                                                                                         
      do index = 1,nageix                                                                                                           
        do iage=1,lage(index)-fage(index)+1                                                                                         
          Alambda(index,iage)=Alambda(index,iage)*Alambda(index,iage)                                                               
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
        If (FitSRR) SRRLambda = SRRLambda*SRRLambda                                                                                 
                                                                                                                                    
                                                                                                                                    
        Call TableOut(1)       !---------------- this does the final printing out                                                   
                                                                                                                                    
                                                                                                                                    
      if (shrink) then                                                                                                              
                                                                                                                                    
        call ShrinkF(Wk,lw,nv,mincv,shrinkyrs,ica_shr)    !--------------------- shrink estimates using WK var-covar matrix,        
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      if (test .ne. 0) then                                                                                                         
        Text(1)=HM(22,Language)                                                                                                     
        call screen_out_a(Text,5,1)                                                                                                 
        stop                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
!     ---------------------------------------------------------- Calculate and print out the execution time                         
                                                                                                                                    
                                                                                                                                    
      Call Writeblock  ! Write out the  contents of the common blocks to                                                            
                       !  disk                                                                                                      
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!9060  format(' ',A17, 1X, A40, 1X, '--> ',\)                                                                                       
!9065  format(' ',A17, 1X, A30, A8, I2, ' --> ',\)                                                                                  
9070  format(F8.3)                                                                                                                  
9010  format (A1)                                                                                                                   
9015  format (' ',A17,      F12.4,F12.4,1X,   I3,1X,F12.4,1X,F8.5,F12.4)                                                            
!             name        fssq   ssq        df    var     ivar   lambda                                                             
9016  format (' ',A14,1X,I1,1X,F12.7,F12.7,1X,I3,1X,F12.4,1X,F8.5,F12.4)                                                            
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      end ! of routine ICA2                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!///////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine DisplayWts                                                                                                         
                                                                                                                                    
!/////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
      Include "INDAT.INC"                                                                                                           
      Include "SEPMODEL.INC"                                                                                                        
      Include "SRR.INC"                                                                                                             
      Include "LABELS.INC"                                                                                                          
      Include "MESSAGE1.INC"                                                                                                        
      integer TextSize
      parameter(TextSize=100)

      character*77 text(TextSize)
      character*250 line                                                                                                            
                                                                                                                                    
                                                                                                                                    
      integer index,age,iage, i                                                                                                     
                                                                                                                                    
!     Write the weights to the screen                                                                                               
                                                                                                                                    
      i =0                                                                                                                          
      if (nssbix .gt. 0) then                                                                                                       
        i=i+1                                                                                                                       
        Text(i) = HM(46,Language)                                                                                                   
      endif                                                                                                                         
      i=i+1                                                                                                                         
      write(Text(i),109)(Blambda(index)*Blambda(index),index=1,nssbix)                                                              
      if (nageix .gt. 0) then                                                                                                       
        i=i+1                                                                                                                       
        Text(i) = HM(23,Language)                                                                                                   
        do index = 1, nageix                                                                                                        
         i=i+1                                                                                                                      
         Text(i)= AsurvLab(index)                                                                                                   
         i=i+1                                                                                                                      
         write(Line ,111) HM(24,Language),                              &                                                           
                      (age,age=fage(index),lage(index))                                                                             
         Text(i)= Line                                                                                                              
         i=i+1                                                                                                                      
         write(Line,112) HM(25,Language),                               &                                                           
               (Alambda(index,iage)*Alambda(index,iage)                 &                                                           
          ,iage=1,lage(index)-fage(index)+1)                                                                                        
         Text(i)=Line                                                                                                               
        enddo                                                                                                                       
      endif                                                                                                                         
                                                                                                                                    
      if (FitSRR) then                                                                                                              
        i=i+1                                                                                                                       
         write(Text(i),113) HM(26,Language), SRRLambda*                 &                                                           
          SRRLambda                                                                                                                 
      endif                                                                                                                         
                                                                                                                                    
      Call Screen_out_a(Text, textSize, i)   
                                                                                                                                    
      return                                                                                                                        
                                                                                                                                    
                                                                                                                                    
109   format(' ',10(1X,F5.3))                                                                                                       
110   format(' ',A11,1X,I2)                                                                                                         
111   format(' ',A10,3X,30(I2,4X))                                                                                                  
112   format(' ',A10,30(F5.3,1X))                                                                                                   
113   format(' ',A30,1X,F5.3)                                                                                                       
                                                                                                                                    
      end  ! of routine displaywts                                                                                                  
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                             
 
! /////////////////////////////////////////////////////////////////////                                                             
                                                                                                                                    
      Subroutine WriteVCV(V, LV, ref, noparms)                                                                                      
! ARGUMENT
! V: var/cov triangle matrix from DN2F
! LV: length of the array V
! ref: the reference position in V indicating the start of the var and covariance values
! noparms: number of parameters                                                                                                                                   
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
      include "INDAT.INC"                                                                                                           
      integer ref, noparms                                                                                                       

      double precision V(LV)                                                                                                                                    
      double precision Corr(maxparm,maxparm), Cov, VCV(noparms,noparms)
                                                                                                                                    
      integer filech,i, j                                                                                                           
      double precision Var1, Var2                                                                                                   

! TRANSFORM THE 1D ARRAY V INTO A 2D ARRAY VCV
! DN2F returns a triangle matrix 
! with variances in position 1, 3, 6, 10, 15, etc...                                                                                                                                    

      do i = 1, noparms                                                                                                             
       do j=1, noparms
	   if(i >= j) then 
		VCV(i,j) = V(ref + (i-1)*i/2 + (j-1))
       else
	    VCV(i,j) = V(ref + (j-1)*j/2 + (i-1))
		endif
         enddo                                                                                                                     
      enddo                                                                                                                         

!DEBUG write(*,*) 'After transforming triangle to square'

      filech = 8                                                                                                                    
      open(filech, file = ica_vc, status='unknown')                                                                                 
                                                                                                                                    
      write(filech, *) 'Variance-Covariance Matrix'                                                                                 
                                                                                                                                    
      write(filech, 9080) (i, i= 1, noparms)                                                                                        
                                                                                                                                    
      do i=1, noparms                                                                                                               
        write(filech,9100) i, (VCV(i,j), j=1, noparms)                                                                             
      enddo                                                                                                                         
                                                                                                                                    
      write(filech, *) ' '                                                                                                          
                                                                                                                                    
      do i = 1, noparms                                                                                                             
       do j=1, noparms                                                                                                              
          Var1 = VCV(i,i)
		  Var2 = VCV(j,j)
           Cov = VCV(i,j)                                                                                       
          if ((Var1 .eq. 0d0) .or. (Var2 .eq. 0d0)                      &                                                           
       .or. (Cov .eq. 0d0) ) then                                                                                                   
           Corr(i,j) = 0.0                                                                                                          
          else                                                                                                                      
           Corr(i,j) = sngl(Cov/dsqrt(Var1*Var2))                                                                                   
          endif                                                                                                                     
         enddo                                                                                                                      
      enddo                                                                                                                         
                                                                                                                                    
      write(filech, *) 'Parameter Correlation Matrix'                                                                               
                                                                                                                                    
      write(filech, 9080) (i, i= 1, noparms)                                                                                        
                                                                                                                                    
      do i=1, noparms                                                                                                               
        write(filech,9100) i, (Corr(i,j), j=1, noparms)                                                                             
      enddo                                                                                                                         
                                                                                                                                    
      write(filech, *)                                                                                                              
                                                                                                                                    
      close(filech)                                                                                                                 

                                                                                                                                    
9080  format(4X,150(I10,1X))                                                                                                        
9090  format(I3,2X,40(F5.3,1X))                                                                                                     
9100  format(I3,1X,150(F10.6,1X))                                                                                                   
                                                                                                                                    
      return                                                                                                                        
                                                                                                                                    
      end ! of subroutine WriteVCV                                                                                                  
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                       
                                                                                                                                                
                                                                                                                                    
!OBSOLETE /////////////////////////////////////////////////////////////////////                                                             
!OBSOLETE                                                                                                                                    
!OBSOLETE      Subroutine WriteVCV(Wk, lw, nv, noparms)                                                                                      
!OBSOLETE                                                                                                                                    
!OBSOLETE ////////////////////////////////////////////////////////////////////                                                              
!OBSOLETE                                                                                                                                    
!OBSOLETE      include "INDAT.INC"                                                                                                           
!OBSOLETE      integer lw, noparms, nv                                                                                                       
!OBSOLETE                                                                                                                                    
!OBSOLETE      double precision Wk(lw), Corr(maxparm,maxparm), Cov, VCV(maxparm,maxparm)
!OBSOLETE                                                                                                                                    
!OBSOLETE      integer filech,i, j                                                                                                           
!OBSOLETE      double precision Var1, Var2                                                                                                   
!OBSOLETE                                                                                                                                    
!OBSOLETE      filech = 8                                                                                                                    
!OBSOLETE      open(filech, file = ica_vc, status='unknown')                                                                                 
!OBSOLETE                                                                                                                                    
!OBSOLETE      write(filech, *) 'Variance-Covariance Matrix'                                                                                 
!OBSOLETE                                                                                                                                    
!OBSOLETE      write(filech, 9080) (i, i= 1, noparms)                                                                                        
!OBSOLETE                                                                                                                                    
!OBSOLETE      do i= 0, (noparms*noparms)-noparms, noparms                                                                                   
!OBSOLETE        write(filech,*) i+1, (Wk(nv+i+j), j=0,noparms-1)                                                                            
!OBSOLETE      enddo                                                                                                                         
                                                                                                                                    
!OBSOLETE      write(filech, *) ' '                                                                                                          
                                                                                                                                    
                                                                                                                                    
!OBSOLETE      do i = 1, noparms                                                                                                             
!OBSOLETE       do j=1, noparms                                                                                                              
!OBSOLETE          Var1 = Wk(nv+(i-1)+((i-1)*noparms))                                                                                       
!OBSOLETE          Var2 = Wk(nv+(j-1)+((j-1)*noparms))                                                                                       
!OBSOLETE          Cov  = Wk(nv+(j-1)+((i-1)*noparms))                                                                                       
!OBSOLETE          if ((Var1 .eq. 0d0) .or. (Var2 .eq. 0d0)                      &                                                           
!OBSOLETE       .or. (Cov .eq. 0d0) ) then                                                                                                   
!OBSOLETE           Corr(i,j) = 0.0                                                                                                          
!OBSOLETE          else                                                                                                                      
!OBSOLETE           Corr(i,j) = sngl(Cov/dsqrt(Var1*Var2))                                                                                   
!OBSOLETE          endif                                                                                                                     
!OBSOLETE         enddo                                                                                                                      
!OBSOLETE      enddo                                                                                                                         
!OBSOLETE                                                                                                                                    
!OBSOLETE                                                                                                                                    
!OBSOLETE                                                                                                                                    
!OBSOLETE      write(filech, *) 'Parameter Correlation Matrix'                                                                               
!OBSOLETE                                                                                                                                    
!OBSOLETE      write(filech, 9080) (i, i= 1, noparms)                                                                                        
!OBSOLETE                                                                                                                                    
!OBSOLETE      do i=1, noparms                                                                                                               
!OBSOLETE        write(filech,9100) i, (Corr(i,j), j=1, noparms)                                                                             
!OBSOLETE      enddo                                                                                                                         
!OBSOLETE                                                                                                                                    
!OBSOLETE      write(filech, *)                                                                                                              
!OBSOLETE                                                                                                                                    
!OBSOLETE                                                                                                                                    
!OBSOLETE                                                                                                                                    
!OBSOLETE      close(filech)                                                                                                                 
!OBSOLETE
!OBSOLETE                                                                                                                                    
!OBSOLETE9080  format(4X,150(I10,1X))                                                                                                        
!OBSOLETE9090  format(I3,2X,40(F5.3,1X))                                                                                                     
!OBSOLETE9100  format(I3,1X,150(F10.6,1X))                                                                                                   
!OBSOLETE                                                                                                                                    
!OBSOLETE      return                                                                                                                        
!OBSOLETE                                                                                                                                    
!OBSOLETE      end ! of subroutine WriteVCV                                                                                                  
!OBSOLETE                                                                                                                                    
!OBSOLETE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                       
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
!OBSOLETE       Subroutine Report (Ifail, Ifaily)                                                                                            
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////                                                               
!                                                                                                                                   
!      This routine simply displays on the screen the error code from the                                                           
!      E04 NAG routines together with the corresponding messages.                                                                   
!                                                                                                                                   
!                                                                                                                                   
!OBSOLETE      Integer Ifail, Ifaily                                                                                                         
!OBSOLETE      character*5 ytext                                                                                                             
!OBSOLETE      character*78 text(1)                                                                                                          
!OBSOLETE      include 'MESSAGE1.INC'                                                                                                        
!OBSOLETE                                                                                                                                    
!OBSOLETE      If ((Ifail .eq. 0) .or. (Ifail .eq. 5)) then                                                                                  
!OBSOLETE        ! do nothing ; could use HM(27) if required                                                                                 
!OBSOLETE      else                                                                                                                          
!OBSOLETE        call IntToChar(Ifail, ytext, 1)                                                                                             
!OBSOLETE        call Concat(Text,HM(28,Language),' IFail = ')                                                                               
!OBSOLETE        call Concat(Text,Text,ytext)                                                                                                
!OBSOLETE        call Screen_out_a(Text(1),1,1)                                                                                              
!OBSOLETE        stop                                                                                                                        
!OBSOLETE      endif                                                                                                                         
!OBSOLETE                                                                                                                                    
!OBSOLETE      If (Ifaily .eq. 0)  then                                                                                                      
!OBSOLETE        ! do nothing ; could use HM(29) if required                                                                                 
!OBSOLETE      else if (Ifaily .eq. 3) then                                                                                                  
!OBSOLETE        call Screen_out_a(HM(30,Language),1,1)                                                                                      
!OBSOLETE      else                                                                                                                          
!OBSOLETE        call Screen_out_a(HM(31,Language),1,1)                                                                                      
!OBSOLETE      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
!OBSOLETE      return                                                                                                                        
!OBSOLETE      end                                                                                                                           
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                              
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
       Subroutine Report (value)                                                                                            
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////                                                               
!                                                                                                                                     
!	PURPOSE: inform the user about the outcome of the minimization procedure
!                The messages are taken from 
!				AT&T Bell Laboratories
!				Computing Science Technical Report No 153
!				Usage Summary for Selected Optimization Routines  
!				David M. Gay 
!				October 1990

	INTEGER value;
        include "MESSAGE1.INC"  
        character*5 ytext                                                                                                             
        character*78 text(1)        


      If ((value >= 3) .and. (value <= 6)) then                                                                                  
        ! do nothing ; could use HM(27) if required                                                                                 
      else                                                                                                                          
        call IntToChar(value, ytext, 2)                                                                                             
        call Concat(Text,HM(28,Language),' Minimization returned value  ')
        call Concat(Text,Text,ytext)
        call Screen_out_a(Text(1),1,1)                                                                                              
        stop                                                                                                                        
      endif                                                                                                                         
                                 

	return                                                                                                                        
     	end                                                                                                                           
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                              
                                                                                                                                      
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine UpdateWts(MaxValue)                                                                                                
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      include "INDAT.INC"                                                                                                           
      include "STATS.INC"                                                                                                           
      include "SEPMODEL.INC"                                                                                                        
      include "SRR.INC"                                                                                                             
      integer index, iage                                                                                                           
      logical weighted                                                                                                              
      double precision MaxValue                                                                                                     
                                                                                                                                    
      weighted = .false.                                                                                                            
      Call CalcStats( weighted )                                                                                                    
                                                                                                                                    
!     The SSB surveys                                                                                                               
      do index = 1, nssbix                                                                                                          
        Blambda(index) = DMIN1(Maxvalue,  CVar/BVar(index))                                                                         
        Blambda(index)=DSQRT(Blambda(index))                                                                                        
      enddo                                                                                                                         
                                                                                                                                    
!     The aged indices                                                                                                              
      do index =1,nageix                                                                                                            
        do iage=1,lage(index)-fage(index)+1                                                                                         
          Alambda(index, iage) = DMIN1(MaxValue, CVar/Avar(index,iage))                                                             
          Alambda(index, iage) = DSQRT(Alambda(index, iage))                                                                        
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
!     The stock-recruit relation                                                                                                    

      If (FitSRR) then
        SRRLambda = DMIN1(MaxValue, CVar/SRRVar) 
        SRRLambda = DSQRT(SRRLambda)
      endif
                                                                                                                                    
                                                                                                                                    
      return                                                                                                                        
      end  ! of routine UpdateWts                                                                                                   
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          

      SUBROUTINE LSFUN(M,N,XC,NF,FVECC,LTY,TY,UF) 
      INTEGER M, N, LTY, NF
      DOUBLE PRECISION XC(N)
      DOUBLE PRECISION FVECC(M), TY(LTY,2) 
      EXTERNAL UF
	write(*,*) 'I AM IN THE SUBROUTINE LSFUN'	

!	M.K.: swap the order of the first 2 arguments to enable MINUIT TO WORK
!       CALL LSFUN1(M,N,XC,FVECC) 
      CALL LSFUN1(N,M,FVECC, XC) 

      RETURN 
      END


!!!!!!!!!! MINUIT OBJECTIVE FUNCTION

      SUBROUTINE OBJECTIVEFCN(NPAR,GIN,F,X,IFLAG,FUTIL)

!      DEFINE global variables
       COMMON/global_int/nodats,noparms;

      INTEGER NPAR, IFLAG, FUTIL
      DOUBLE PRECISION X(NPAR), F

      DOUBLE PRECISION GIN(nodats)
      EXTERNAL UF
!write(*,*) 'I AM IN THE SUBROUTINE OBJECTIVEFCN'
!write(*,*) 'the value of M is ', M
!write(*,*) 'the value of nodats is ', nodats

!write(*,*) 'the value of NPAR is ', npar
!write(*,*) 'the value of the parameters is ', X
!write(*,*) 'the value of the residuals ', F

!SUBROUTINE FCN(NPAR,GRAD,FVAL,XVAL,IFLAG,FUTIL)
!	M.K.: swap the order of the first 2 arguments to enable MINUIT TO WORK
!      CALL LSFUN1(M,NPAR,X,F) 
      CALL LSFUN1(NPAR,nodats,GIN,X) 

! Compute the Sum of Sqare to return to MINUIT
     F = 0d0                                                                                                                   
      do j=1,nodats                                                                                                                  
        F = F+(GIN(j) * GIN(j))                                                                                                   
      enddo                                                               
!write(*,*) 'The value of the SSQ at the end of OBJECTIVEFCN ', F 
!write(*,*) 'The value of the residuals in OBJECTIVEFCN ', GIN
!write(*,*) 'I AM OUT OF THE SUBROUTINE OBJECTIVEFCN'


      RETURN 
      END
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ INUTILE is a dummy subroutine required for the DN2F minimization routine from the PORT library +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!      SUBROUTINE INUTILE
!      RETURN
!      END
