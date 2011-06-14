module ica2_module

use kind_module
use data_definition
use message_definition
use screen_io_module
use statistics_module
use srr_module

contains

! ///////////////////////////////////////////////////////////////////////     
                        
      Subroutine ICA2(Params,VCV,IxCor,pmaxparm,RWOpt,maxweight) !  Fit the full model by least-squares

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
!                       
!                       
! ///////////////////////////////////////////////////////////////////////     
!                       
!                       
!          Program Description:                    
!          ====================                    
!                       
!          The program implements a Deriso-Gudmundsson type integrated catch-at-age analysis.            
!          It relies upon a prior run of ICA1 for  finding an approximate minimum from which to begin the search.
!                       
!          The solution is found in three concentric loops , as below (the third loop is within          
!   NAG routine, which solves for F,S and N by modified Newton method):       
!                       
!                       
!                      Set weights all = 1, (or set by user)                  
!      Recalculate until F changes by less than specified amount              
!      .                
!      .    Recalculate until SSQ changes by less than 0.05%                  
!      .    .   fit separable model using NAG routine                         
!      .    .   update the conventional VPA using populations & selection pattern from separable model   
!      .    Iterate     
!      .                
!      Calculate new weights                       
!      Iterate          
!                      write outputs               
!                      stop                        

      implicit none

      Integer Pmaxparm, RWOpt

      double precision Params(pmaxparm), VCV(pmaxparm,pmaxparm)

!     Local variables   
                        
      character*5 ytext 
      character*9 ntext 
      character*76 Text(5)                         
      double precision Resids(maxdata)           ! Residuals                  
      integer  i, j, ik, iage   
      character*1 dummy 
      integer age, index
      double precision ssq, SSQOld, endsq                   ! ssq for anova table                        
      integer liw, iw(maxdata), lw, ifail       ! Workspace for E0f routines  
      integer  nv, ns, job, ifaily           ! used in e04YCF call            
      double precision cj(maxparm)                ! not used; reqd for e04YCF syntax                     
      double precision Wk                    &     
       ((7*maxparm+maxparm*maxparm+2*maxparm*maxdata+                   &     
              3*maxdata + maxparm*(maxparm-1)/2))  
      double precision Wk2(maxparm)               ! E04YCF workspace          
!      real*4  Corr(maxparm,maxparm)          ! Parameter correlation matrix                   
      double precision  Var1                 ! Temporary variables used in calculating parameter correlat
      integer test, nodats, noparms          ! no of data, no of parameters   
      logical firstrun                       ! skips E04 calls in first reweighting loop: weights recalcu
      integer  it                      ! Reweighting option, iteration number                      
!FLR      double precision  IxCor(maxsurvey), LastF, FmTol                          ! Index Weights working variable, Internal correlati
      double precision  IxCor(maxsurvey), LastF                          ! Index Weights working variable, Internal correlati
      integer iyr,imon,iday, ihr, imin, isec, ith                       ! System time and date           
      real Time   ! Time as real; includes day of week in case of overnight ru

external lsfun1, e04fdf

      double precision mincv, MaxWeight,MinTol,MaxTol                 ! Minimum cv of mean for shrinkage                          
      logical shrink                         ! whether or not to shrink       
      integer shrinkyrs                      ! number of years to shrink to   
      integer tscan                          ! The tscan function replaces Microsoft 'SCAN'              
!______________________________EXECUTABLE CODE OF ICA2______________________  
                        

      if (pmaxparm< nxparm) then
        if (DLLFlag .eq. .false.) write(*,*) 'ERROR from ICA2_90: pmaxparm< nxparm '
        if (DLLFlag .eq. .false.) write(*,*) ' values : ',pmaxparm, nxparm
      endif

      nodats = nxdata
      noparms = nxparm  
      UseSep = .true.          ! always fit a separable model              
                        
        !------------------------ Take square roots of the weights, for computational efficiency         
                        
      do index=1,nssbix 
        Blambda(index) = dsqrt(Blambda(index))     
      enddo  ! indices of SSB                      
                        
      do index = 1,nageix                          
          do iage=1,lage(index)-fage(index)+1      
            Alambda(index,iage)=dsqrt(Alambda(index,iage) *             &     
            (IxCor(index)*dble(lage(index)-fage(index))+1) /           &     
                         dble(lage(index)-fage(index)+1))                     
          enddo  ! ages 
      enddo     ! years 
                        
      ! If (FitSRR) 
	SRRLambda = dsqrt(SRRLambda)     
                        
                        
                        
9050  format (A1)       
9075  format (' ', A37, ' --> ')                   
                        
                        
         ! -----------------------  Whether or not to shrink, and associated options                        
                        
      shrink = .false.  
!      dummy = 'N'       
                        
!      CAll Screen_in_a(HM(11,Language),dummy,KO(1,Language),Language)         

!      shrinkyrs = 5
!      MinCV=0.2d0
!      if (tscan(dummy, KY(1,Language)) .ne. 0) then
!        Call Screen_in_i(HM(12,Language),shrinkyrs,                     &     
!             lastyear-firstyear+1,2,Language)      
!        Call Screen_in_r(HM(13,Language),MinCV,1d0,0d0,Language)              
!        shrink = .true. 
!      endif  ! have chosen to shrink               
                        
 
!FLR to avoid file input                       
      IF (RWOpt .ne. 2) then
        MinTol=1d-8
        MaxTol=0.1d0
        FMTol =0.05d0 
      ELSE              
        FMtol=1d-9      
      ENDIF             

!      IF (RWOpt .ne. 2) then
!        MinTol=1d-8
!        MaxTol=0.1d0
!        FMTol = 0.05d0 
!        !Text(1)=HM(14,Language)                    
!        !Text(2)=HM(15,Language)                    
!        !Call Screen_out_a(Text,5,2)                
!        !Call Screen_in_r(HM(16,1),FmTol,MaxTol,MinTol,Language)  
!	read (12, *) FmTol
!      ELSE              
!        FMtol=1d-9      
!      ENDIF             
                        
                        
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

      do i = 1, noparms                   ! Update the parameter list         
        Params(i) = (Xbest(i))                     
      enddo             

                       
! --------------------------------------------------------------------        
!         ANALYSIS RESTARTS FROM HERE ON REWEIGHTING                          
! --------------------------------------------------------------------        
                        
      DO WHILE ((dabs(dexp(Params(NySep))-LastF)/LastF .gt. FmTol)  & ! Continue as long as the F is changing by more than the       
        .and. ( .not. (( it .ge. 1) .and. (RWOpt .eq. 2)) ) )        ! specified amount, unless no inverse-variance reweighting is  
               ! specified. In this latter case, do one loop only.            
      if (.not. firstrun) LastF = dexp(Params(NySep))                ! update the LastF from previous run
      it = it + 1                         ! update the number of iv wt iterations                        
                        
      Call DisplayWts                     ! show weights on the screen        
                        
                        
      do i = 1, noparms                   ! Update the parameter list         
        Params(i) = (Xbest(i))                     
      enddo             
                        
                        
! ----------------------------------------------------------------------      
! ------------------------------------- CALCULATE STARTING RESIDUALS          
! ----------------------------------------------------------------------      
                        
                        
      liw = 1000        

       lw=((7*maxparm+maxparm*maxparm+2*maxparm*maxdata+                   & 
              3*maxdata + maxparm*(maxparm-1)/2))         

      ifail= 1
                        
                       
                        
!  ------------------------- FOLLOWING LOOP IS THE ACTUAL MODEL FITTING FOR GIVEN WEIGHTS                

        if (DLLFlag .eq. .false.) write (*,*) "Calling LSFUN1: line 219 of ICA2"

        Call LSFUN1(nodats, Noparms, Params, Resids)                          
        endsq = 0d0     
        do i = 1, nodats
          endsq = endsq+Resids(i)*Resids(i)        
        enddo           
                       
                        
        SSQOld = 1d0           ! ----------arbitrary to make sure it starts   
        SSQ = endsq            ! ----------SSQ from starting position estimated by prior fit             
        do while ( dabs(SSQ-SSQOld)/SSQold .gt. 0.0005d0) ! --------- Keep going until the SSQ changes by less than 0.05%           
          !write(*,*) 'Minimising SSQ ----> ',SSQ         ! --------- for debugging                      
          SSQOld = SSQ
	ifail = 1
          Full = .false.
          call e04fdf(nodats, noparms, params, SSQ, iw,liw, wk, lw, ifail) ! ------- NAG routine : fir the separable model                 
          if (DLLFlag .eq. .false.) write(*,*) 'SSQ --- > ',SSQ              
          Full = .true.                   !------------------------ but recalculate the full VPA afterwar

          Call LSFUN1(nodats, Noparms, Params, Resids)                        
        enddo           
        
!        call Tableout(1)
!        pause

! MODEL HAS BEEN FITTED: NOW THE VAR-COVAR MATRIX ESTIMATE                    
!                      See NAG Documentation to explain this                  
!                       
!                       
                       
       NS = 6*noparms + 2*nodats + nodats*noparms + 1 +                 &     
             max0(1, (noparms*(noparms-1))/2)      
        nv = ns + noparms                          
                        
       ifaily = 1       
       job = -1         
                        
       call e04ycf(job, nodats, noparms, ssq, wk(ns), wk(nv), noparms,  &     
       cj, wk2, ifaily) 
                        
 ! --------------------------------------- display the NAG diagnostic message  
                        
      Call Report(Ifail, Ifaily)                   

      if (Ifaily .ne. 0) then
        if (DLLFlag .eq. .false.) write(*,*) 'Ifaily : ',Ifaily
      endif
                        
      do i = 1, noparms       ! --------------------- copy parameters into common block                  
        Xbest(i) = Params(i)                       
      enddo             
                        
                        
                        
!      If (FitSRR) then  
!        a = dexp(XBest(Noparms-1))                 
!        b = dexp(Xbest(Noparms))                   
!        Call WriteSRRFile                          
!      endif             
                        
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
                        

                        
!---------------------------------------- Model HAS BEEN FITTED AND VCV ESTIMATED                        
                        
!      Call WriteVCV(Wk, lw, nv, noparms)  !-------------- write var-covar matrix to file                 
                        
return 
                        
      do i= 0, (noparms*noparms)-noparms, noparms     !copy variables to square matrix                        
        do j=0,noparms-1  
          VCV((i/noparms)+1,j+1)=Wk(nv+i+j)   
        enddo
      enddo             


                        
!-------------------------------------------------------- CALCULATE THE PARAMETER S.D.s                  
      do i = 1, noparms       ! --------------------- see NAG routine documentation for the way in which the VCV matrix             
          Var1 = Wk(nv+(i-1)+((i-1)*noparms))   !---- stored in the Wk workspace vector                  
          if (Var1 .gt. 0d0) then                  
            Var1 = dsqrt(Var1)                     
          else          
            Var1 = 0d0  
           if (DLLFlag .eq. .false.) write(*,*) 'Negative variance error in ICA2.'                      
          endif         
          XHigh(i) = Xbest(i) + Var1               
          XLow(i) = Xbest(i) - Var1                
      enddo             
                        
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
   
                        
        ! If (FitSRR) 
	SRRLambda = SRRLambda*SRRLambda



                        
                        
!        Call TableOut(1)       !---------------- this does the final printing out                        
                        
                        
!      if (shrink) then  
                        
!        call ShrinkF(Wk,lw,nv,mincv,shrinkyrs,ica_shr)    !--------------------- shrink estimates using WK var-covar matrix,        
!      endif             
                        
                        
                        
                        
!     ---------------------------------------------------------- Calculate and print out the execution time                         
                        
                        
!      Call Writeblock  ! Write out the  contents of the common blocks to      
                       !  disk                     
                        
                        
                        
!9060  format(' ',A17, 1X, A40, 1X, '--> ',\)      
!9065  format(' ',A17, 1X, A30, A8, I2, ' --> ',\) 
9070  format(F8.3)      
9010  format (A1)       
9015  format (' ',A17,      F12.4,F12.4,1X,   I3,1X,F12.4,1X,F8.5,F12.4)      
!             name        fssq   ssq        df    var     ivar   lambda       
9016  format (' ',A14,1X,I1,1X,F12.7,F12.7,1X,I3,1X,F12.4,1X,F8.5,F12.4)      
                        
      return 
      end subroutine ICA2

!///////////////////////////////////////////////////////////////////////      
                        
      Subroutine DisplayWts                        
                        
!/////////////////////////////////////////////////////////////////////        
                        
      character*77 text(30)                        
      character*250 line
                        
                        
      integer index,age,iage, i                    
                        
!     Write the weights to the screen              
                       
      if (DLLFlag .eq. .false.) write (*,*) "Index weighting:"

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
         if (DLLFlag .eq. .false.) write(Line ,111) HM(24,Language),   &     
                      (age,age=fage(index),lage(index))                       

         if (DLLFlag .eq. .false.) write(*, 111) HM(24,Language),   &     
                      (age,age=fage(index),lage(index))                       

         Text(i)= Line  
         i=i+1          
         if (DLLFlag .eq. .false.) write(Line,112) HM(25,Language),    &     
               (Alambda(index,iage)*Alambda(index,iage),iage=1,lage(index)-fage(index)+1)       

         if (DLLFlag .eq. .false.) write(*, 112) HM(25,Language),    &     
               (Alambda(index,iage)*Alambda(index,iage),iage=1,lage(index)-fage(index)+1)       

         Text(i)=Line   
        enddo           
      endif             

      if (FitSRR) then  
        i=i+1           
         if (DLLFlag .eq. .false.) write(Text(i),113) HM(26,Language), SRRLambda*SRRLambda
      endif             
                        
      Call Screen_out_a(Text, 30, i)               
                        
      return            
                        
                        
109   format(' ',10(1X,F5.3))                      
110   format(' ',A11,1X,I2)                        
111   format(' ',A10,3X,30(I2,4X))                 
112   format(' ',A10,30(F5.3,1X))                  
113   format(' ',A30,1X,F5.3)                      
                        
      end subroutine displaywts

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       
                        
                        
! /////////////////////////////////////////////////////////////////////       
                        
      Subroutine WriteVCV(Wk, lw, nv, noparms)     
                        
! ////////////////////////////////////////////////////////////////////        
                        
      integer lw, noparms, nv                      
                        
      double precision Wk(lw), Corr(maxparm,maxparm), Cov 
                        
      integer filech,i, j                          
      double precision Var1, Var2                  
                        
      filech = 8        
      open(filech, file = ica_vc, status='unknown')
                        
      write(filech, *) 'Variance-Covariance Matrix'
                        
      write(filech, 9080) (i, i= 1, noparms)       
                        
      do i= 0, (noparms*noparms)-noparms, noparms  
        write(filech,*) i+1, (Wk(nv+i+j), j=0,noparms-1)                      
      enddo             
                        
      write(filech, *) ' '                         
                        
                        
      do i = 1, noparms 
       do j=1, noparms  
          Var1 = Wk(nv+(i-1)+((i-1)*noparms))      
          Var2 = Wk(nv+(j-1)+((j-1)*noparms))      
          Cov  = Wk(nv+(j-1)+((i-1)*noparms))      
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
                        
      end subroutine WriteVCV
                        
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
                        
                        
! ////////////////////////////////////////////////////////////////////        
                        
       Subroutine Report (Ifail, Ifaily)           
                        
! ///////////////////////////////////////////////////////////////////         
!                       
!      This routine simply displays on the screen the error code from the     
!      E04 NAG routines together with the corresponding messages.             
!                       
!                       
      Integer Ifail, Ifaily                        
      character*5 ytext 
      character*78 text(1)                         
                        
      If ((Ifail .eq. 0) .or. (Ifail .eq. 5)) then 
        ! do nothing ; could use HM(27) if required
      else              
        call IntToChar(Ifail, ytext, 1)            
        call Concat(Text(1),HM(28,Language),' IFail = ')
        call Concat(Text(1),Text(1),ytext)
        call Screen_out_a(Text(1),1,1)             
        stop            
      endif             
                        
      If (Ifaily .eq. 0)  then                     
        ! do nothing ; could use HM(29) if required
      else if (Ifaily .eq. 3) then                 
        call Screen_out_a(HM(30,Language),1,1)     
      else              
        call Screen_out_a(HM(31,Language),1,1)     
      endif             
                        
                        
      return            
      end subroutine Report
                        
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
                        
                        
                        
! //////////////////////////////////////////////////////////////////////      
                        
      Subroutine UpdateWts(MaxValue)               
                        
! ///////////////////////////////////////////////////////////////////////     
                        
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

      !If (FitSRR) then
        SRRLambda = DMIN1(MaxValue, CVar/SRRVar) 
        SRRLambda = DSQRT(SRRLambda)
      !endif
                        
                        
      return            
      end subroutine UpdateWts
                        
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    

end module ica2_module

! ************************************************************************

! Stand-alone subroutines, called as "external" in ICA2:

!///////////////////////////////////////////////////////////////////////      
!                       
	real (wp) function CalcSSBlsfun1 (year) result (ssb)
    
!                       
!///////////////////////////////////////////////////////////////////////      
!                       
!     Calculates the SSB in a given year from fitted populations,             
!                    at the time of spawning.      
!                       

	use kind_module
	use data_definition
	use message_definition
	use reader_module
                        
!     Local variables   
                        
      integer :: year, iage
                        
                        
!     ____________________EXECUTABLE CODE______________________________       
                        
      ssb = 0.0d0         
      do iage = 1,lastage-firstage+1               
	if (MO(year-firstyear+1, iage) .gt. 0.0d0) then     
	 ssb=ssb+(N(year-firstyear+1, iage)*        &     
                 dble(MO(year-firstyear+1, iage))*                      &     
                 dble(SW(year-firstyear+1, iage))*                      &     
                 dexp(- F(year-firstyear+1, iage)*PF                    &     
                      -NM(year-firstyear+1, iage)*PM) )                       
	end if                  
      end do             
      end function CalcSSBlsfun1
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       


!     /////////////////////////////////////////////////////////////////       
                        
      Subroutine LSFUN1(M,Nparm, AP, FC)           
                        
!     //////////////////////////////////////////////////////////////////      
                        
!     GENERAL PURPOSE OBJECTIVE FUNCTION           

	use kind_module
	use data_definition
	use message_definition
	use reader_module
	use srr_module
	use cvpa1_module

!     local variables   
                        
      integer m,nparm   
      integer ndata     
      double precision fs(maxyear)              ! F by year in separable model
      double precision S2(maxage)    ! S at age in separable model 
	! real (wp), dimension (maxage) :: s
      double precision Stock(maxyear), recruit(maxyear)                       
      integer parmno, index,                 &     
       iage, iyear      
      integer i, age, year       ! Local looping variables                    
!      double precision NP(maxyear,maxage),FM(maxyear,maxage) ! Predicted catches and poplns from separable model                   
      double precision FC(maxdata)             ! Residual for each observation
      double precision AP(maxparm)              ! Parameters  
	real (wp) :: SSB                
      double precision Q, Observed,     &     
                     expected             ! internal vars                     
      integer syear
	! integer :: nosrrdata                     
      double precision Z_here       ! internal vars
      logical UseRecr   
      double precision QK                

	real (wp) :: CalcSSBlsfun1
	external CalcSSBlsfun1
                        
!     __________________EXECUTABLE CODE OF LSFUN1_______________________      
                        
                        
      parmno = 0        
      i = 0             
                        
!     Copy the parameters out of AP                
                        
!     Separable model parameters                   
                        
      do i = 1, nysep   
        fs(i) = dexp(AP(i))+1d-6                   
        parmno = parmno+1                          
      enddo             
                        
      do age = firstage, lastage-1                 
        iage = age-firstage+1                      
        If (age .eq. Refage) then                  
          s(iage) = 1.0d0                          
        else if (age .eq. lastage-1) then          
          s(iage) = TermS                          
        else if ((age .ne. Refage) .and. (age .ne. lastage-1)) then           
          parmno = parmno+1                        
          s(iage) = dexp(AP(parmno))+1d-6          
        endif           
      enddo             
                        
      If (TwoSel) then ! second selection pattern  
        do age = firstage, lastage-1               
          iage = age-firstage+1                    
          If (age .eq. Refage) then                
            s2(iage) = 1.0d0                       
          else if (age .eq. lastage-1) then        
            s2(iage) = TermS2                      
          else if ((age .ne. Refage) .and. (age .ne. lastage-1)) then         
            parmno = parmno+1                      
            s2(iage) = dexp(AP(parmno))+1d-6       
          endif         
        enddo           
      endif  ! two selection patterns              
                        
!     first do the separable VPA                   
                        
!      Initialise edges of the predicted population matrix                    
                        
!     Nos at age in last year of the separable analysis                       
                        
      do age= firstage, lastage-1                  
        iyear =lastyear-firstyear+1                
        parmno=parmno+1 
        N(iyear, age-firstage+1) = dexp(AP(parmno))+1d0                       
      enddo !   ages    
                        
!     Terminal Poplns at last true age in the years of the separable analysis 
                        
      iage = lastage-firstage                      
      do year = lastyear-nysep+1, lastyear-1       
        iyear = year-firstyear+1                   
        parmno=parmno+1 
        N(iyear, iage)=dexp(AP(parmno))+1d0        
      enddo !   years   
                        
                        
!     Generate the Fishing Mortality Matrix        

      if (RecalculatePopulations) then 

        if (.not. TwoSel) then ! Simplest model, one selection pattern          
          do year = lastyear-NySep+1, lastyear       
            do iage=1, lastage-firstage              
              iyear = year-firstyear+1               
              syear = year-lastyear+NYSep            
              F(iyear,iage) = FS(syear)*S(iage)      
            enddo ! ages  
          enddo   ! years 
        else  ! two selection vectors                
          If (StepSel) then ! step change in selection pattern                  
            do year = lastyear-NySep+1, lastyear     
              do iage=1, lastage-firstage            
                iyear = year-firstyear+1             
                syear = year-lastyear+NYSep          
                If (year .le. ChangeSel) then        
                  F(iyear,iage) = FS(syear)*S(iage)  
                else      
                  F(iyear,iage) = FS(syear)*S2(iage) 
                endif ! in period of first selection pattern                    
              enddo ! ages
            enddo   ! years                          
          else  ! Gradual change in selection pattern
            do year = lastyear-NySep+1, lastyear     
              do iage=1, lastage-firstage            
                iyear = year-firstyear+1             
                syear = year-lastyear+NYSep          
                If (year .le. ChangeSel) then        
                  F(iyear,iage) = FS(syear)*S(iage)  
                else      
                  F(iyear,iage)= FS(syear)*    &     
                   (S2(iage)*dble( year - ChangeSel+1)+                   &     
                         S(iage)*dble(lastyear-year))/                    &     
                         dble(lastyear-ChangeSel+1)  
                endif ! in years of second selection pattern                    
              enddo   ! ages                         
            enddo     ! years                        
          endif    ! Step change in selection pattern
        endif     ! one or two selection vectors     
                        
                        
                        
!     Fill in the rest of the matrix backwards, predicted                     
!     catches go in PredCN, Abundances in N        
                        
        ndata=0           
        do year = lastyear, lastyear-NySep+1, -1     
          do age = lastage-1, firstage, -1           
            iage = age-firstage+1                    
            iyear = year-firstyear+1                 
            syear = year-lastyear+NYSep              
            Z_here = F(iyear,iage) + NM(iyear,iage)  
            if ((age .ne. lastage-1) .and. (year .ne. lastyear))          &     
                N(iyear,iage) = N(iyear+1,iage+1)*dexp(Z_here)                  
            PredCN(iyear,iage) = N(iyear, iage) *                         &     
             (F(iyear,iage)/Z_here) * (1.0d0-dexp(-Z_here))                     
            if (CN(iyear,iage) .gt. 0d0) then        
               ndata= ndata+1                        
               FC(ndata)= W(iyear,iage)*       &     
               dlog(CN(iyear,iage)/PredCN(iyear,iage))                          
            endif ! missing catch datum              
          enddo   ! ages  
        enddo     ! years 
                        
!     Now the 'conventional ' VPA part             
                        
      ! Following code stops the VPA being recalculated for full = false      
                        
        If (full) then ! do the VPA                  
          Call CVPA2(S,lastyear-NySep+1)             
        endif             
                        
                        
      ! now deal with the plus-group               
                        
!     All +gp Fs assumed equal to F on last true age group                    
                        
        do year =firstyear, lastyear                 
          iyear = year-firstyear+1                   
              F(iyear, lastage-firstage+1) = F(iyear, lastage-firstage)         
        enddo             
                        
!     Pool +gp Ns calculated by accumulating along cohorts; +gp catches ignored                          
                        
!      Npool(1) = N(1,lastage-firstage+1)          
!      do year = firstyear+1, lastyear             
!          iyear = (year-firstyear+1)              
!          iage = lastage-firstage+1               ! the plus-group           
!           Npool(iyear) = (Npool(iyear-1)*        
!     *        dexp(-F(iyear-1,iage)-NM(iyear-1,iage)))                       
!     *        + (Npool(iyear-1)*                  
!     *        dexp(-F(iyear-1,iage-1)-NM(iyear-1,iage-1)))                   
!      enddo            
                        
!    Conventional +gp Ns on +gp catches            
                        
!      do year = firstyear, lastyear               
!          iyear = (year-firstyear+1)              
!          iage = lastage-firstage+1               ! the plus-group           
!          N(iyear,iage) = (CN(iyear,iage)*        
!     *      (F(iyear,iage)+NM(iyear,iage)))/      
!     *       F(iyear,iage)*(1d0-dexp(-(F(iyear,iage)+NM(iyear,iage))))       
!      enddo            
                        
                        
                        
                        
!      If (.not. Twosel) then                      
!                          this is ica1_2 version code for +gp Ns             
!        do year = firstyear+1, lastyear           
!          iyear=(year-firstyear+1)                
!          iage = lastage-firstage+1               
!          N(iyear,iage) = (N(iyear-1,iage)*       
!     *       dexp(-F(iyear-1,iage)-nm(iyear-1,iage)))                        
!     *       + (N(iyear-1,iage-1) *               
!     *       dexp(-F(iyear-1,iage-1)-NM(iyear-1,iage-1)))                    
!        enddo          
                        
!      else             
                        
!     This is the +gp code from ICAGVS             
!     +gp Ns calculated from backwards catch equation using +gp catches       
                        
          do year = firstyear, lastyear              
              iyear = (year-firstyear+1)             
              iage = lastage-firstage+1                 ! the plus-group        
              if (CN(iyear,iage) .gt. 1d-1) then        ! not a missing value   
                Z_here = F(iyear,iage)+NM(iyear,iage)
                N(iyear, iage) = CN(iyear,iage)*Z_here/                   &     
F(iyear,iage)/    &     
(1d0 - dexp(-Z_here))   
              else if (year .ne. firstyear) then     
                N(iyear,iage) = (N(iyear-1,iage)*                       &     
                dexp(-F(iyear-1,iage)-NM(iyear-1,iage)))                &     
                + (N(iyear-1,iage-1)*        &     
                dexp(-F(iyear-1,iage-1)-NM(iyear-1,iage-1)))                  
              else        
                  N(iyear,iage) = 0d0 ! cannot be estimated                     
              endif       
          enddo           
!      endif  ! not two sel                        
                        
!     Project the population one more year forwards
                        
        DO age = firstage, lastage-2                 
          iage = age-firstage+1                      
          iyear = lastyear-firstyear+1               
                        
          N(iyear+1,iage+1) = N(iyear,iage) * dexp(-F(iyear,iage)-        &     
                       NM(iyear,iage))             
        enddo             
                        
!     Project the plus-group forwards with F, add the oldest true age         
!     group to the plus group                      
                        
        iage = lastage-firstage+1                    
        iyear = lastyear-firstyear+1                 
        N(iyear+1, iage) = (N(iyear,iage) *    &     
            dexp(-(F(iyear,iage)+NM(iyear,iage)))) +                    &     
            (N(iyear,iage-1)*dexp(-(F(iyear,iage-1)+NM(iyear,iage-1))))       
                        
!     Copy the Fs forwards one more year, so that indices of abundance        
!     one year after the last year of catch data can be fitted                
                        
        do age = firstage, lastage                   
          iyear = lastyear-firstyear+1               
          iage = age-firstage+1                      
          F(iyear+1, iage) = F(iyear, iage)          
        enddo             
                        
                        
! -------------- END OF VPA CALCULATIONS --------------------                 
                        
      else ! No need to recalculate the populations
           ! but still need to keep track of no of dat

      ndata=0           
      do year = lastyear, lastyear-NySep+1, -1     
        do age = lastage-1, firstage, -1           
          iage = age-firstage+1                    
          iyear = year-firstyear+1                 
          if (CN(iyear,iage) .gt. 0d0) then        
             ndata= ndata+1                        
          endif ! missing catch datum              
        enddo   ! ages  
      enddo     ! years 

                        
                        

      endif ! recalculate populations
                        
!     Take out the estimated recruitment parameter if there are               
!     data to estimate it                          
                        
      UseRecr = .false. 
      do index= 1, nageix                          
       if ((fage(index).eq.firstage).and.(lyear(index).eq.lastyear+1))  &     
             UseRecr = .true.                      
      enddo             
                        
      if (UseRecr) then 
        parmno = parmno+1                          
        N(lastyear-firstyear+2, 1) = dexp(AP(Parmno))                         
      endif             
                        
!     Calculate the residuals for each index       
                        
!     First the SSB indices                        
                        
      do index = 1, nssbix                         
        if (QBparm(index) .eq. 0) then             
          Q = 1d0       
          QK = 0d0      
        endif           
        if (QBparm(index) .eq. 1) then             
          parmno = parmno+1                        
          Q = 1d0       
          QK = AP(parmno)                          
        endif           
        if (QBparm(index) .eq. 2) then             
          parmno = parmno+1                        
          Q = AP(parmno)
          parmno=parmno+1                          
          QK = AP(parmno)                          
        endif           
        do  year = fbyear, lbyear                  
          if (BSurvey(index, year-fbyear+1) .ne. missing) then                   
            ndata = ndata+1                        
            SSB = CalcSSBlsfun1(year)                    
            Expected = Q*dlog(SSB)+ QK             
            Observed = BSurvey(index, year-fbyear+1)
            FC(ndata) =BLambda(index)*(Observed-Expected)                     
            PredBindex(index, year-fbyear+1)=expected                         
          endif         
        enddo   ! Years 
      enddo         ! Indices                      
                        
                        
!     Next the age-structured indices              
                        
      do index = 1, nageix                         
        do age = fage(index), lage(index)          
          if (QAparm(index) .eq. 0) then           
            Q = 1d0     
            QK = 0d0    
          endif         
          if (QAparm(index) .eq. 1) then           
            parmno = parmno+1                      
            Q = 1d0     
            QK = AP(parmno)                        
          endif         
          if (QAparm(index) .eq. 2) then           
            parmno = parmno+1                      
            Q = AP(parmno)                         
            parmno=parmno+1                        
            QK = AP(parmno)                        
          endif         
                        
          do  year = fyear(index), lyear(index)    
            if (ASurvey(index, year-fyear(index)+1,age-fage(index)+1)    &     
              .ne. missing) then                     
              ndata = ndata+1                      
              if ((age .eq. lage(index)) .and. plusgp(index)) then            
                    Expected = 0d0                 
                do iage = age, lastage             
                  Expected = Expected + N(year-firstyear+1,             &     
                   iage-firstage+1)*dexp(-(  &     
                   F(year-firstyear+1,iage-firstage+1)+                 &     
                   NM(year-firstyear+1,iage-firstage+1))*               &     
                   Timing(index))                  
                enddo ! Accumulating for the plus-group                       
                Expected = Q*dlog(Expected)+QK     
              else      
                if (N(year-firstyear+1,age-firstage+1).lt. 1d-20)       &
                      then                         
                  if (DLLFlag .eq. .false.) write(*,*) 'Index but no population at ',age,         &     
                         ' in year ',year          
                  if (DLLFlag .eq. .false.) write(*,*) ' attempting to fit aged index',           &     
                         index                     
                  stop  
                    endif ! an error                   
                Expected = Q*dlog(N(year-firstyear+1,                   &     
                 age-firstage+1)*dexp(-(     &     
                 F(year-firstyear+1,age-firstage+1)+                    &     
                 NM(year-firstyear+1,age-firstage+1))*                  &     
                  Timing(index)))+QK               
              endif   ! Not in the plus-group      
                Observed=dble(ASurvey(index, year-fyear(index)+1,        &     
                   age-fage(index)+1))             
                FC(ndata)=ALambda(index,age-fage(index)+1)*(Observed-   &     
                   Expected)                       
                PredAindex(index,year-fyear(index)+1,                   &     
                    age-fage(index)+1)=Expected    
            endif ! not a missing value            
          enddo    ! Years                         
        enddo       ! ages                         
      enddo          ! Indices                     
                        
!     STOCK - RECRUIT RELATIONSHIP                 
                        
      if (FitSRR) then  
	! SRRLambda = 0.1d0 ! Temporary fix
        Call GetSRR(Stock, Recruit, noSRRdata)     
        do i = 1, NoSRRdata                        
           ndata = ndata+1                         
           Observed = Recruit(i)                   
           Expected = (dexp(AP(Parmno+1))*Stock(i))/                    &     
 (dexp(AP(Parmno+2))+Stock(i))                     
           Predrecruit(i) = Expected               
           FC(ndata) = SRRLambda*(dlog(Observed/Expected))                    
        enddo ! SRR data
        parmno = parmno+2                          
      endif ! fitting a stock-recruit relation     
173   format (I2, 1X, 3(E25.16,1X))                
                        
                        
      if (parmno .ne. Nparm) then                  
            if (DLLFlag .eq. .false.) write(*,*) 'ICA 2 (LSFUN 1) : No of parameters is wrong ',parmno,   &     
             Nparm      
      endif             
                        
      if (Ndata .ne. M) then                       
            if (DLLFlag .eq. .false.) write(*,*) 'ICA 2 (LSFUN 1) : No of data is wrong ',Ndata,M               
      endif             
                        
!      ssq = 0d0        
!      do j=1,M         
!        ssq = ssq+(FC(j)*FC(j))                   
!      enddo            
!       write(*,*) 'SSQ --> ',SSQ                  
                        
                        
9000  format(A1)        
9010  format(I2,1X,I4,1X,I2,1X,2(F15.10,1X))       
9015  format(I4,1X,I2,1X,5(F15.10,1X))             
9016  format(I4,1X,I4,1X,2(F15.10,1X))             
                        
      return            
      end subroutine LSFUN1

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       

