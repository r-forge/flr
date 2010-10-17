! /////////////////////////////////////////////////////////////////////                                                             
!                                                                                                                                   
!    ADAPTIVE REJECTION SAMPLING ALGORITHM : MODIFIED SECANT METHOD                                                                 
!                                                                                                                                   
! ////////////////////////////////////////////////////////////////////                                                              
!                                                                                                                                   
!                                                                                                                                   
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
      Subroutine KPARS(V1, V2, Vupper,Vlower,param, maxparam,           &                                                           
        ierr, errortext, ribs, VWork,oldlikelh)                                                                                     
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                   
!    Draws a paramth sample for X2 given parameters in X1,                                                                          
!    array size maxparm, the log likelihood function must be external and                                                           
!    named Funct1(maxparam,X, LogLikelihood)                                                                                        
!    Priors must be returned by a function named                                                                                    
!            FUNCT2(maxparm,X,Xupper,Xlower,Xprior)                                                                                 
!                                                                                                                                   
!     Steps are as p. 84 of MCMC in practice                                                                                        
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      integer maxribs                                                                                                               
      parameter (maxribs=80)               ! maximum no of points in upper and lower                                                
                                        ! envelope functions                                                                        
                                                                                                                                    
      integer ribs                      ! current no of points in envelope function                                                 
      integer hi_shoulder, lo_shoulder  ! indexes of the shoulders of the distribution                                              
                                                                                                                                    
      double precision X(maxribs), Y(maxribs) ! points at which the loglikelihood                                                   
                                        ! function is evaluated (X- paramater, Y-loglikelihood)                                     
                                                                                                                                    
      double precision apex, keel       ! apex is estimate of loglikelihood function maximum                                        
      double precision area(maxribs)       ! probability mass in each rectangle                                                     
      double precision Work(maxribs)       ! a workspace                                                                            
                                                                                                                                    
      double precision h_old, h_new, crit ! variables for Metropolis step                                                           
                                                                                                                                    
      integer param, maxparam                                                                                                       
      double precision V1(maxparam), V2(maxparam),Vupper(maxparam),     &                                                           
      Vlower(maxparam), Vwork(maxparam)                                                                                             
      double precision sexp, slog                                                                                                   
                                                                                                                                    
      logical accept                                                                                                                
      double precision lowhull, uphull, Xo                                                                                          
      integer ierr, i, asample                                                                                                      
      double precision randx, recipe_ran, newlikelh                                                                                 
      character* (*) errortext                                                                                                      
                                                                                                                                    
      double precision loglike, logprior, OldLikelh                                                                                 
                                                                                                                                    
                                                                                                                                    
!     INITIALISE ENVELOPE                                                                                                           
                                                                                                                                    
      if (param .gt. maxparam) then                                                                                                 
        write(*,*) 'KPARS: incorrect call', param,maxparam                                                                          
      endif                                                                                                                         
                                                                                                                                    
      ribs = 5                                                                                                                      
      X(1) = Vlower(param)                                                                                                          
      X(3) = Vlower(param)+0.66667*(V1(param)-Vlower(param)) ! 2/3 the way between lower bound and last iterate                     
      X(2) = V1(param)+0.3333*(Vupper(param)-V1(param))      ! 1/3 the way between last iterate and upper bound                     
      X(4) = Vupper(param)                                                                                                          
      X(5) = V1(param)                                                                                                              
      Y(5) = OldLikelh                                                                                                              
                                                                                                                                    
      do i=1,  maxparam                                                                                                             
        Vwork(i) = V1(i)                                                                                                            
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      do i =1,4                                                                                                                     
        Vwork(param) = X(i)                                                                                                         
        call Funct1(maxparam, Vwork, LogLike)                                                                                       
        call Funct2(maxparam,Vwork,Vupper,Vlower,logprior)                                                                          
        Y(i) = -LogLike+logprior ! because FUNCT1 returns a -ve loglikelihood                                                       
      enddo                                                                                                                         
                                                                                                                                    
      Call InitialiseHull(ribs,maxribs,X,Y,hi_shoulder,                 &                                                           
         lo_shoulder, Apex, Keel,Area,Vwork,Vupper,Vlower,              &                                                           
         errortext,ierr, maxparam, param)                                                                                           
       if (ierr .gt. 0) then                                                                                                        
         write(*,*) errortext,ierr                                                                                                  
         call WriteENV(errortext,ierr,Keel,maxparam,Vwork,Vupper,       &                                                           
          Vlower, ribs, X, Y, maxribs, Area,param, Apex)                                                                            
       endif                                                                                                                        
                                                                                                                                    
                                                                                                                                    
      accept = .false.                                                                                                              
                                                                                                                                    
!  *******************************************************************                                                              
!   THE ADAPTIVE REJECTION SAMPLING LOOP STARTS HERE                                                                                
!                                                                                                                                   
      DO while (.not. accept)                                                                                                       
                                                                                                                                    
        ! PART 1: Take a sample from the full conditional distribution                                                              
                                                                                                                                    
        CAll SampleAnArea (ribs,Area,asample,ierr,errortext,Work)                                                                   
        if (ierr .gt. 0) then                                                                                                       
          write(*,*) errortext,ierr                                                                                                 
          pause                                                                                                                     
        endif                                                                                                                       
                                                                                                                                    
        Call SampleWithinArea (ribs,X,Xo,asample,ierr,errortext)                                                                    
        if (ierr .gt. 0) then                                                                                                       
          write(*,*) errortext,ierr                                                                                                 
          pause                                                                                                                     
        endif                                                                                                                       
                                                                                                                                    
!                      Xo is the candidate value for the next iteration in input vector V                                           
                                                                                                                                    
        ! PART 2: Take a sample from uniform distribution (0,1)                                                                     
                                                                                                                                    
        Randx = Recipe_ran(1)                                                                                                       
                                                                                                                                    
        ! PART 3: TEST whether to accept, using lower hull function                                                                 
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
        ! evaluate upper hull function                                                                                              
        Call Upperhull(Xo,ribs,X,Y,hi_shoulder,lo_shoulder,Apex,        &                                                           
          uphull, ierr, errortext)                                                                                                  
        if (ierr .gt. 0) then                                                                                                       
          write(*,*) errortext,ierr                                                                                                 
          pause                                                                                                                     
        endif                                                                                                                       
                                                                                                                                    
                                                                                                                                    
! FOLLOWING BLOCK IS SQUEEZING FUNCTION                                                                                             
                                                                                                                                    
!        ! evaluate lower hull function                                                                                             
!                                                                                                                                   
!        call LowerHull(LowHull,Xo,ribs,X,Y,ierr,errortext)                                                                         
!        crit = sexp(lowhull-uphull)                                                                                                
!        if (ierr .gt. 0) then                                                                                                      
!          write(*,*) 'LOWHULL: error',errortext                                                                                    
!          accept=.false.                                                                                                           
!        else if (Randx .lt. crit) then ! squeezing using lower hull                                                                
!          accept = .true.                                                                                                          
!          write(*,*) 'SQUEEZE ACCEPT',OldLikelh,NewLikelh,Lowhull                                                                  
!        else                                                                                                                       
!         ! evaluate Likelihood function at new value                                                                               
                                                                                                                                    
        Call NewEVAL(errortext,ierr,Xo,maxparam,Vwork,Vupper,           &                                                           
          Vlower, ribs, X, Y, maxribs, Area,param, Apex)                                                                            
          if (ierr .gt. 0) then                                                                                                     
            write(*,*) errortext,ierr                                                                                               
            pause                                                                                                                   
          endif                                                                                                                     
                                                                                                                                    
         NewLikelh = Y(ribs)                                                                                                        
         crit = NewLikelh-UpHull                                                                                                    
                                                                                                                                    
         If (Randx .le. sexp(crit)) then                                                                                            
           Accept = .true.                                                                                                          
!            write(*,*) 'ARS ACCEPT: ',randx,sexp(crit),NewLikelh,Uphull                                                            
!           write(*,*) 'LIKELIHOODS: ',OldLikelh, NewLikelh                                                                         
         else                                                                                                                       
!           write(*,*) 'ARS REJECT: ',randx,sexp(crit),NewLikelh,Uphull                                                             
           Call InitialiseHull(ribs,maxribs,X,Y,hi_shoulder,            &                                                           
             lo_shoulder, Apex, Keel,Area,Vwork,Vupper,Vlower,          &                                                           
              errortext,ierr, maxparam, param)                                                                                      
         endif                                                                                                                      
!       endif  ! needed for squeezing                                                                                               
      enddo ! ARS loop                                                                                                              
                                                                                                                                    
!                                                                                                                                   
!   END OF THE ADAPTIVE REJECTION SAMPLING LOOP                                                                                     
!**********************************************************************                                                             
                                                                                                                                    
         if (NewLikelh .gt. Uphull) then ! METROPOLIS STEP, for safety                                                              
           call Funct1(maxparam, V1, LogLike)                                                                                       
           call Funct2(maxparam,V1,Vupper,Vlower,logprior)                                                                          
           OldLikelh= -LogLike+logprior                                                                                             
           h_new=DMIN1(NewLikelh, UpHull)                                                                                           
           h_old=DMIN1(OldLikelh, UpHull)                                                                                           
           Randx= Recipe_Ran(1)                                                                                                     
           crit = NewLikelh+h_old-OldLikelh-h_new                                                                                   
           crit = sexp(crit)                                                                                                        
           if (Randx .le. DMIN1( 1d0, crit)) then                                                                                   
              ! candidate value used for next iteration                                                                             
                                                                                                                                    
           else                                                                                                                     
             Xo = V1(param)                                                                                                         
!             write(*,*) 'METROPOLIS REJECT'                                                                                        
           endif  ! Metropolis step accept/rejection test                                                                           
             accept=.true.                                                                                                          
       endif                                                                                                                        
                                                                                                                                    
!      now update the output vector                                                                                                 
        crit = NewLikelh-UpHull                                                                                                     
!        write(*,*)  'Apex UpHull  New LH   Crit'                                                                                   
!        write(*,*)  Apex, Uphull, Newlikelh, sexp(crit)                                                                            
      V2(param) = Xo                                                                                                                
                                                                                                                                    
       if (ierr .eq. -100) then                                                                                                     
       CAll WriteENV(errortext,ierr,Keel,maxparam,Vwork,Vupper,         &                                                           
       Vlower, ribs, X, Y, maxribs,Area,param,apex)                                                                                 
       endif                                                                                                                        
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!      write(*,*) 'KPARS Output and conditional param Vector '                                                                      
!      do i=1,maxparam                                                                                                              
!        write(*,*) V2(i),Vwork(i),V1(i)                                                                                            
!      enddo                                                                                                                        
                                                                                                                                    
!                                                                                                                                   
!  This code to make sure variance and Likelihood returned are correct                                                              
!     ONLY FOR TESTING ....                                                                                                         
!                                                                                                                                   
!      do i=1,maxparam                                                                                                              
!        VWork(i) = V1(i)                                                                                                           
!      enddo                                                                                                                        
!      Vwork(param) = Xo                                                                                                            
                                                                                                                                    
!      call Funct1(maxparam, Vwork, LogLike)                                                                                        
!      call Funct2(maxparam,Vwork,Vupper,Vlower,logprior)                                                                           
!      NewLikelH = -LogLike+logprior ! because FUNCT1 returns a -ve loglikelihood                                                   
                                                                                                                                    
      return                                                                                                                        
      end  ! of subroutine KPARS                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      Subroutine LowerHull(LowHull,Xin,npts,X,Y,ierr,errortext)                                                                     
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
!    Lower hull function evaluation. Returns <LowHull> for a given                                                                  
!     value of Xin, given npts function evaluations Y for values X                                                                  
!     lower hull is very straight forward as always linear interpolation                                                            
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      integer npts, ierr, i                                                                                                         
      character* (*) errortext                                                                                                      
      double precision Xin, X(npts),Y(npts),LowHull                                                                                 
                                                                                                                                    
                                                                                                                                    
      ierr= 0                                                                                                                       
!     first some checking...                                                                                                        
      if (Xin .lt. X(1)) then                                                                                                       
        ierr = 1                                                                                                                    
        errortext='LOWERHULL 1 : Input below lower bound'                                                                           
      else if (Xin .gt. X(npts)) then                                                                                               
        ierr = 1                                                                                                                    
        errortext='LOWERHULL 2: Input above upper bound'                                                                            
      else if (npts .lt. 4) then                                                                                                    
        ierr = 1                                                                                                                    
        errortext='LOWERHULL 3: Needs at least 4 points.'                                                                           
      endif                                                                                                                         
      do i = 1,npts-1                                                                                                               
        if (X(i) .gt. X(i+1)) then                                                                                                  
          ierr =1                                                                                                                   
          errortext='LOWERHULL 4: Input vector not sorted'                                                                          
        endif                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      if (ierr .eq. 0) then ! ie no error detected, do the calculation                                                              
        i =1                                                                                                                        
        do while (X(i+1) .lt. Xin )                                                                                                 
          i = i+1                                                                                                                   
        enddo  ! finds the X(i) below Xin                                                                                           
                                                                                                                                    
        If ((Y(i+1)-Y(i)) .eq. 0d0) then                                                                                            
          LowHull = Y(i)                                                                                                            
          write(*,*) 'LOWHULL: input Y equals eval Y'                                                                               
        else if (Xin-X(i) .eq. 0d0) then                                                                                            
          LowHull = Y(i)                                                                                                            
          write(*,*) 'LOWHULL: input X equals lower X'                                                                              
        else if (X(i)-X(i+1) .eq. 0d0) then                                                                                         
          LowHull = Y(i)                                                                                                            
          write(*,*) 'LOWHULL: No X inteval'                                                                                        
        else                                                                                                                        
          LowHull = Y(i) + ( Y(i+1)-Y(i) )/(X(i+1)-X(i)) *              &                                                           
                            (Xin-X(i))                                                                                              
        endif                                                                                                                       
                                                                                                                                    
                                                                                                                                    
        if (LowHull .gt. DMax1(Y(i+1),Y(i))) then                                                                                   
          ierr =1                                                                                                                   
!          write(*,*) Xin, LowHull, Y(i),Y(i+1),X(i),X(i+1),i                                                                       
          errortext='LOWERHULL 5: Calculation error 1'                                                                              
        else if (LowHull .lt. DMin1(Y(i),Y(i+1) )) then                                                                             
           ierr =1                                                                                                                  
           errortext='LOWERHULL 6: Calculation error 2'                                                                             
        endif                                                                                                                       
      endif ! an error detected                                                                                                     
                                                                                                                                    
      return                                                                                                                        
      end ! of routine LOWERHULL                                                                                                    
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
                                                                                                                                    
      Subroutine FindShoulders(npts, X,Y,hi_shoulder,                   &                                                           
       lo_shoulder)                                                                                                                 
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!   Finds the highest point and the next-highest adjacent (in X) point the Y distribution,                                          
!   termed the shoulders. They are integer indexes of the X,Y data pairs                                                            
!                                                                                                                                   
      implicit none                                                                                                                 
      integer npts, i                                                                                                               
!      character* (*) errortext                                                                                                     
      double precision X(npts),Y(npts), hi, lo                                                                                      
      integer hi_shoulder,lo_shoulder                                                                                               
                                                                                                                                    
                                                                                                                                    
!     find the highest                                                                                                              
                                                                                                                                    
      hi = -1d20                                                                                                                    
      i = 1                                                                                                                         
      do i =1,npts                                                                                                                  
        if (Y(i) .gt. hi) then                                                                                                      
          hi = Y(i)                                                                                                                 
          hi_shoulder=i                                                                                                             
        endif                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
! decide which point to be the second shoulder.                                                                                     
                                                                                                                                    
      if (hi_shoulder .eq. 1) then  ! highest is at lower limit, use second, etc.                                                   
        lo_shoulder = 2                                                                                                             
      else if (hi_shoulder .eq. npts) then                                                                                          
        lo_shoulder = npts-1                                                                                                        
      else if (Y(hi_shoulder-1) .gt. Y(hi_shoulder+1)) then                                                                         
        lo_shoulder=hi_shoulder-1                                                                                                   
      else if (Y(hi_shoulder+1) .gt. Y(hi_shoulder-1)) then                                                                         
        lo_shoulder=hi_shoulder+1                                                                                                   
      endif                                                                                                                         
                                                                                                                                    
      return                                                                                                                        
      end ! of routine FindShoulders                                                                                                
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!///////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine FindApex(npts, X,Y,hi_shoulder,lo_shoulder,Apex,       &                                                           
          Keel, errortext, ierr)                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
!  This routine seeks the Apex and keel of the upper hull (highest point)                                                           
!         Apex is estimate of maximum Y                                                                                             
!         Keel is corresponding estimate of X                                                                                       
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!  three possible cases:  highest Y or second-highest on lower X bound                                                              
!                        highest Y in between                                                                                       
!                        highest Y or second-highest upper X bound                                                                  
!            if an error, returns next point to evaluate at Keel                                                                    
!            and code = (case*100) + case-specific code                                                                             
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      integer hi_shoulder, lo_shoulder, npts, ierr, j                                                                               
      character* (*) errortext                                                                                                      
      double precision X(npts), Y(npts), Apex                                                                                       
      double precision a,b,c,d,l,m,n,o, keel, projected                                                                             
                                                                                                                                    
      ierr = 0                                                                                                                      
      errortext = ' '                                                                                                               
                                                                                                                                    
      if (npts .lt. 4) then                                                                                                         
        ierr= 1                                                                                                                     
        errortext ='FindApex: less than 4 points provided'                                                                          
        return                                                                                                                      
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      if ((hi_shoulder .eq. 1).or.((hi_shoulder.eq.2)                   &                                                           
          .and. (lo_shoulder.eq.1))) then  ! Apex by projection to lower bound                                                      
!         write(*,*) 'FindApex case 1'                                                                                              
                                                                                                                                    
!        calculate the slope between the 2nd and 3rd X values,                                                                      
!        project back for lower X bound, apex is the higher value of                                                                
!        the projected Y at the bound and the actual value at the bound                                                             
         if (Y(3) .gt. Y(2)) then                                                                                                   
           errortext= 'FindApex 1 : Internal error'                                                                                 
           ierr =101                                                                                                                
           write(*,*) errortext                                                                                                     
!           stop                                                                                                                    
         else                                                                                                                       
           Apex = Y(3)+(X(1)-X(3))* (Y(3)-Y(2))/(X(3)-X(2))                                                                         
           if (Apex .lt. Y(1)) then                                                                                                 
             Apex = Y(1) ! not log-concave !!                                                                                       
             ierr = -101                                                                                                            
           endif                                                                                                                    
           Keel = X(1)                                                                                                              
         endif                                                                                                                      
         ierr = -100                                                                                                                
       else if((hi_shoulder.eq.npts).or.                                &                                                           
          ((hi_shoulder.eq.npts-1).and. (lo_shoulder.eq.npts)) )   then ! highest Y on upper X bound                                
                                                                                                                                    
         ierr = -200                                                                                                                
         Apex = (Y(npts-1)) + (X(npts) - X(npts-1))*                    &                                                           
            ( Y(npts-1)-Y(npts-2))/(X(npts-1)-X(npts-2))                                                                            
         Keel = X(npts)                                                                                                             
         if (Apex .lt. Y(npts)) then                                                                                                
          Apex = Y(npts)                                                                                                            
           ierr =-201                                                                                                               
         endif  ! projection under max                                                                                              
!         write(*,*)'FindApex case 2',Apex,Y(npts),Y(hi_shoulder),ierr                                                              
                                                                                                                                    
       endif   ! case 2                                                                                                             
                                                                                                                                    
       if ((hi_shoulder .ne. 1) .and. (hi_shoulder .ne. npts)           &                                                           
           .and.(lo_shoulder.ne.1) .and. (lo_shoulder.ne.npts) ) then ! in the middle                                               
                                                                                                                                    
         if (hi_shoulder .lt. lo_shoulder) then                                                                                     
           j = -1                                                                                                                   
         else                                                                                                                       
           j = 1                                                                                                                    
         endif                                                                                                                      
         a = X(hi_shoulder+j)                                                                                                       
         b = X(hi_shoulder)                                                                                                         
         c = X(lo_shoulder)                                                                                                         
         d = X(lo_shoulder-j) ! in the opposite direction                                                                           
         l = Y(hi_shoulder+j)                                                                                                       
         m = Y(hi_shoulder)                                                                                                         
         n = Y(lo_shoulder)                                                                                                         
         o = Y(lo_shoulder-j)                                                                                                       
         Keel = c*(n-o)/(d-c) + a*(m-l)/(b-a) -l + n                                                                                
         Keel = Keel/ ( (n-o)/(d-c) + (m-l)/(b-a) )                                                                                 
         Apex = (n-o)/(d-c) * (c-Keel) + n                                                                                          
!         write(*,*) 'FindApex case 3', apex                                                                                        
         ierr = -300                                                                                                                
       endif                                                                                                                        
                                                                                                                                    
!      Error checking at the end                                                                                                    
                                                                                                                                    
       if (Apex .lt. Y(hi_shoulder)) then                                                                                           
         ierr = iabs(ierr) +10                                                                                                      
         errortext='FindApex 3: Apex < highest Y'                                                                                   
       else if ((Keel.lt. DMIN1(X(hi_shoulder), X(lo_shoulder))).or.    &                                                           
         (Keel.gt. DMAX1(X(hi_shoulder), X(lo_shoulder)))) then                                                                     
         errortext='FindApex 4: Apex not located between shoulders'                                                                 
         ierr = iabs(ierr)+20                                                                                                       
       endif                                                                                                                        
                                                                                                                                    
       return                                                                                                                       
       end                                                                                                                          
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                             
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////                                                             
                                                                                                                                    
      Subroutine  UpperHull(Eval,npts,X,Y,hi_shoulder,                  &                                                           
       lo_shoulder, Apex, uphull, ierr, errortext)                                                                                  
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////                                                              
!                                                                                                                                   
!    Inputs: npts - no of points for function evaluation                                                                            
!            X    - parameter values for function evaluation                                                                        
!            Y    - loglikelihood values for function evaluation                                                                    
!            hi_shoulder - index of X,Y arrays for highest Y                                                                        
!            lo_shoulder - index of X,Y arrays for second highest Y                                                                 
!            Eval - value of parameter                                                                                              
!     Output:  uphull - value of loglikleihood for parameter=eval                                                                   
!                                                                                                                                   
!      The routine assigns uphull to the higher value of Y                                                                          
!      corresponding to the two adjacent values of X in + and                                                                       
!      - direction, unless X lies between X(hi_shoulder) and                                                                        
!        X(lo_shoulder), in which case uphull is assigned to the                                                                    
!        apex value.                                                                                                                
!                                                                                                                                   
      implicit none                                                                                                                 
      integer npts, hi_shoulder, lo_shoulder, i, ierr                                                                               
      double precision Eval,X(npts),Y(npts),Apex, uphull                                                                            
      character*(*) errortext                                                                                                       
                                                                                                                                    
      ierr =0                                                                                                                       
      if (npts .lt. 4) then                                                                                                         
        ierr = 1                                                                                                                    
        errortext='UpperHull 1: Less than 4 points provided'                                                                        
      endif                                                                                                                         
                                                                                                                                    
      if ((Eval .gt. X(npts)) .and. (ierr .eq. 0)) then                                                                             
        ierr = 1                                                                                                                    
        errortext='UpperHull 2: Attempt to evaluate above X range'                                                                  
      else if (Eval .lt. X(1)) then                                                                                                 
        ierr =1                                                                                                                     
        errortext='UpperHull 3: Attempt to evaluate below X range'                                                                  
      endif                                                                                                                         
                                                                                                                                    
      IF (Ierr .eq. 0) THEN                                                                                                         
                                                                                                                                    
        if (((Eval.ge.X(lo_shoulder)).and.(Eval.le.X(hi_shoulder)))     &                                                           
        .or.((Eval.ge.X(hi_shoulder)).and.(Eval.le.X(lo_shoulder))))    &                                                           
         then                                                                                                                       
           Uphull = Apex                                                                                                            
        else                                                                                                                        
          i = 1                                                                                                                     
          do while ((Eval .ge. X(i)) .and. (i .lt. npts))                                                                           
            i=i+1                                                                                                                   
          enddo                                                                                                                     
          if ((i .eq. 1) .or. (i .gt. npts)  ) then                                                                                 
            ierr =1                                                                                                                 
            errortext ='UpperHull 4: Failed consistency check'                                                                      
          else                                                                                                                      
            Uphull = DMAX1( Y(i), Y(i-1) )                                                                                          
          endif                                                                                                                     
                                                                                                                                    
!          write(*,*) 'UpHULL : ',UpHull                                                                                            
        endif                                                                                                                       
      endif ! no  error                                                                                                             
                                                                                                                                    
      return                                                                                                                        
      end  ! of routine Upperhull                                                                                                   
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                       
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////////                                                        
                                                                                                                                    
      Subroutine CalcArea(npts,X,Y,hi_shoulder,lo_shoulder,             &                                                           
                  Apex, Area, errortext, ierr)                                                                                      
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////////                                                        
!                                                                                                                                   
!      Returns the log areas (log probability + log parameter interval)                                                             
!       from which to draw a sample                                                                                                 
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      integer npts, hi_shoulder,lo_shoulder, ierr                                                                                   
      double precision Apex,X(npts),Y(npts),Area(npts), slog                                                                        
                                                                                                                                    
      integer i, j                                                                                                                  
      double precision top, width, eval                                                                                             
                                                                                                                                    
      character*40 errortext                                                                                                        
                                                                                                                                    
      ierr  = 0                                                                                                                     
                                                                                                                                    
      j = npts-1                                                                                                                    
      do i =1, j                                                                                                                    
        top = Y(i)                                                                                                                  
        width = X(i+1)-X(i)                                                                                                         
        ierr = 0                                                                                                                    
        eval = (X(i+1)+X(i))/2d0 ! evaluate at the mid-point                                                                        
        CAll UpperHull(eval,npts,X,Y,hi_shoulder,                       &                                                           
         lo_shoulder, Apex, top, ierr, errortext)                                                                                   
        if (ierr .ne. 0) then                                                                                                       
          errortext='CalcArea Calling Upperhull'                                                                                    
        endif                                                                                                                       
        Area(i) = top+slog(width)                                                                                                   
      enddo                                                                                                                         
                                                                                                                                    
      return                                                                                                                        
      end ! of routine calcareas                                                                                                    
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
      Subroutine SampleAnArea(npts,Area,asample,ierr,errortext,P)                                                                   
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
!                                                                                                                                   
!      Returns a random choice of which rectangle to sample from,                                                                   
!      given the log area of all rectangles                                                                                         
!                                                                                                                                   
      implicit none                                                                                                                 
      integer npts, asample, ierr, i                                                                                                
      double precision area(npts), P(npts), total, val                                                                              
                                                                                                                                    
      character* (*) errortext                                                                                                      
                                                                                                                                    
      double precision random, Recipe_ran, sexp                                                                                     
                                                                                                                                    
! as they are logs, convert to (scaled) probability                                                                                 
                                                                                                                                    
      i=1                                                                                                                           
      P(i) = 1d0                                                                                                                    
      do i =2, npts-1                                                                                                               
        val =  area(i)-area(1)                                                                                                      
        P(i) = sexp(val)  ! scaled to area 1                                                                                        
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
!                                                                                                                                   
! make them cumulative                                                                                                              
                                                                                                                                    
      do i=2, npts-1                                                                                                                
        P(i) = P(i-1) + P(i)                                                                                                        
      enddo                                                                                                                         
                                                                                                                                    
!  Scale to unit probability                                                                                                        
                                                                                                                                    
      do i=1,npts-1                                                                                                                 
        P(i) = P(i)/P(npts-1)                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
! Take a sample from uniform random over range 0, P(npts)                                                                           
                                                                                                                                    
      RANDOM = Recipe_ran(12) * P(npts-1)                                                                                           
                                                                                                                                    
      asample = 1                                                                                                                   
                                                                                                                                    
      do while (Random .ge.  P(asample) )                                                                                           
        asample=asample+1                                                                                                           
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      if  (asample .gt. npts-1) then                                                                                                
        ierr = 1                                                                                                                    
        errortext = 'SampleAnArea 1: return exceeds max'                                                                            
      else if (Random .gt. P(asample)) then                                                                                         
        ierr = 1                                                                                                                    
        errortext = 'SampleAnArea 2: calcn failed'                                                                                  
      endif                                                                                                                         
      if (ierr .ne. 0) write(*,*) errortext                                                                                         
                                                                                                                                    
!      do i =1,npts-1                                                                                                               
!        write(*,*) 'Prob',Area(i),P(i), asample                                                                                    
!      enddo                                                                                                                        
                                                                                                                                    
      return                                                                                                                        
      end ! of routine sample an area                                                                                               
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                
                                                                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      Subroutine SampleWithinArea (npts,Xi,Xo,asample,ierr,errortext)                                                               
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      implicit none                                                                                                                 
      integer npts, asample, ierr, n                                                                                                
      double precision Xi(npts), Xo, Recipe_Ran                                                                                     
                                                                                                                                    
                                                                                                                                    
      character* (*) errortext                                                                                                      
                                                                                                                                    
      ierr =0                                                                                                                       
      if (asample .gt. npts-1) then                                                                                                 
        ierr = 1                                                                                                                    
        errortext='SampleWithinArea: rectangle outside data'                                                                        
      else if (asample .le. 0) then                                                                                                 
        ierr =1                                                                                                                     
        errortext='SampleWithinArea: area index =0 '                                                                                
      else                                                                                                                          
        Xo= Xi(asample)+Recipe_ran(2)*(Xi(asample+1)-Xi(asample))                                                                   
      endif                                                                                                                         
                                                                                                                                    
      return                                                                                                                        
      end ! of routine sample within area                                                                                           
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
       Subroutine PIKSR2(n,ARR,BRR)                                                                                                 
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
!     Sorts the ARR, BRR data pairs on ascending X                                                                                  
!       see Numerical Recipes p. 228                                                                                                
!                                                                                                                                   
       implicit none                                                                                                                
       integer n                                                                                                                    
       double precision ARR(n), BRR(n), A, B                                                                                        
       integer i, j                                                                                                                 
       do j=2,n                                                                                                                     
         A = ARR(j)                                                                                                                 
         B =BRR(j)                                                                                                                  
         do i=j-1,1,-1                                                                                                              
           if (ARR(i) .le. A) goto 10                                                                                               
           ARR(i+1) = ARR(i)                                                                                                        
           BRR(i+1)=BRR(i)                                                                                                          
         enddo                                                                                                                      
         i=0                                                                                                                        
10       ARR(i+1)=A                                                                                                                 
         BRR(i+1)=B                                                                                                                 
       enddo                                                                                                                        
       return                                                                                                                       
       end ! of routine PIKSR2                                                                                                      
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                   
      Subroutine WriteENV(errortext,ierr,Keel,maxparam,Vwork,Vupper,    &                                                           
          Vlower, ribs, X, Y, maxribs, Area,param, Apex)                                                                            
!                                                                                                                                   
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!   If an error occurs in the hull function due to failure of the                                                                   
!   interpolating method, this routine can be called to evaluate                                                                    
!   the likleh function at the problematic points                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      integer ierr, maxparam, ribs, maxribs,param,i                                                                                 
      double precision Keel, Vwork(maxparam),Vupper(maxparam),          &                                                           
      Vlower(maxparam), Xo, uphull, lowhull                                                                                         
      double precision X(maxribs), Y(maxribs),Area(maxribs),apex                                                                    
      character* (*) errortext                                                                                                      
                                                                                                                                    
      Double precision logLike, LogPrior, newlikelh                                                                                 
      integer hi_shoulder, lo_shoulder                                                                                              
                                                                                                                                    
                                                                                                                                    
!  CODE FOR TESTING  THE HULL FUNCTIONS                                                                                             
                                                                                                                                    
!      write(*,*) 'WRITEENV: parameter ',param,Keel,X(1),X(ribs)                                                                    
                                                                                                                                    
                                                                                                                                    
      Call FindShoulders(ribs, X,Y,hi_shoulder,lo_shoulder)                                                                         
      Call FindApex(ribs, X,Y,hi_shoulder,lo_shoulder,Apex,             &                                                           
             Keel, errortext, ierr)                                                                                                 
!      open(21,file='mc.tst',status='unknown')                                                                                      
!      write(21,*) errortext                                                                                                        
!      write(21,*)'Function evaluations...'                                                                                         
!      do i=1,ribs                                                                                                                  
!        write(21,*) X(i), Y(i)                                                                                                     
!      enddo                                                                                                                        
                                                                                                                                    
!      write(21,*) 'Upper and lower hull functions'                                                                                 
      do i =1,21                                                                                                                    
        Xo = (X(1)*1.01d0) + (X(ribs)-X(1))*dble(i-1)/dble(21) ! 21-1                                                               
                                                                                                                                    
        Call Upperhull(Xo,ribs,X,Y,hi_shoulder,lo_shoulder,Apex,        &                                                           
         uphull, ierr, errortext)                                                                                                   
         if (ierr .ne.0) write(*,*) errortext, X(1),Xo                                                                              
        CAll LowerHull(LowHull,Xo,ribs,X,Y,ierr,errortext)                                                                          
         if (ierr .ne.0) write(*,*) errortext, X(1),Xo                                                                              
!         write(21,*) Xo,uphull,lowhull                                                                                             
      enddo                                                                                                                         
!      close (21)                                                                                                                   
                                                                                                                                    
      return                                                                                                                        
      end ! of routine writeenv                                                                                                     
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                   
      Subroutine NewEVAL(errortext,ierr,Keel,maxparam,Vwork,Vupper,     &                                                           
          Vlower, ribs, X, Y, maxribs, Area,param, Apex)                                                                            
!                                                                                                                                   
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!     Adds one new function evaluation at X=Keel                                                                                    
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      integer ierr, maxparam, ribs, maxribs,param,i                                                                                 
      double precision Keel, Vwork(maxparam),Vupper(maxparam),          &                                                           
      Vlower(maxparam), Xo, uphull, lowhull                                                                                         
      double precision X(maxribs), Y(maxribs),Area(maxribs),apex                                                                    
      character* (*) errortext                                                                                                      
                                                                                                                                    
                                                                                                                                    
      Double precision logLike, LogPrior, newlikelh                                                                                 
      integer hi_shoulder, lo_shoulder                                                                                              
                                                                                                                                    
      ierr = 0                                                                                                                      
        if((Keel.lt.Vlower(param)).or.(Keel.gt.Vupper(param)))then                                                                  
           write(*,*) Keel,Apex,param, Vlower(param),Vupper(param)                                                                  
           write(*,*) 'NEWEVAL:Attempt to evaluate out of bounds.'                                                                  
           stop                                                                                                                     
        endif                                                                                                                       
        Vwork(param) = Keel                                                                                                         
        call Funct1(maxparam, Vwork, LogLike)                                                                                       
        call Funct2(maxparam,Vwork,Vupper,Vlower,logprior)                                                                          
        NewLikelh = -LogLike+logprior ! because FUNCT1 returns a -ve loglikelihood                                                  
        ribs = ribs+1                                                                                                               
        X(ribs) = Keel                                                                                                              
        Y(ribs) = NewLikelh                                                                                                         
      return                                                                                                                        
      end ! of routine NewEVAL                                                                                                      
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      Subroutine InitialiseHull(ribs,maxribs,X,Y,hi_shoulder,           &                                                           
         lo_shoulder, Apex, Keel,Area,Vwork,Vupper,Vlower,              &                                                           
         errortext,ierr, maxparam, param)                                                                                           
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      implicit none                                                                                                                 
      integer maxparam                                                                                                              
      integer ribs,maxribs,hi_shoulder,lo_shoulder,ierr,param                                                                       
      double precision X(maxribs), Y(maxribs),Vwork(maxparam),          &                                                           
       Vupper(maxparam), Vlower(maxparam),Area(Maxribs)                                                                             
      double precision Apex,Keel                                                                                                    
      character* (*) errortext                                                                                                      
                                                                                                                                    
                                                                                                                                    
      ierr = 1                                                                                                                      
      do while ((ierr .gt. 0).and. (ribs .le. maxribs))                                                                             
        Call PIKSR2(ribs, X, Y) ! sort the arrays to put old position in order                                                      
        CAll FindShoulders(ribs, X,Y,hi_shoulder,lo_shoulder)                                                                       
        CAll FindApex(ribs, X,Y,hi_shoulder,lo_shoulder,Apex,           &                                                           
          Keel, errortext, ierr)                                                                                                    
        if (ierr .gt. 0) then                                                                                                       
         CALL WriteENV(errortext,ierr,Keel,maxparam,Vwork,Vupper,       &                                                           
          Vlower, ribs, X, Y, maxribs, Area,param, Apex)                                                                            
        endif                                                                                                                       
                                                                                                                                    
        if (ierr.gt.0) then                   ! try to fix the problem by more evaluations                                          
          write(*,*) Errortext, ierr                                                                                                
          CAll NewEval(errortext,ierr,Keel,maxparam,Vwork,Vupper,       &                                                           
         Vlower, ribs, X, Y, maxribs,Area,param,apex)                                                                               
          Call PIKSR2(ribs, X, Y) ! sort the arrays to put old position in order                                                    
        endif                                                                                                                       
                                                                                                                                    
        CAll CalcArea(ribs,X,Y,hi_shoulder,lo_shoulder,                 &                                                           
                  Apex, Area, errortext, ierr)                                                                                      
      enddo                                                                                                                         
      if (ribs .eq. maxribs) then                                                                                                   
        write(*,*) 'Maximum ribs exceeded'                                                                                          
        write(*,*) errortext, ierr                                                                                                  
        write(*,*) 'Initialise Hull failed'                                                                                         
        stop                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
      return                                                                                                                        
      end ! of Initialise Hull                                                                                                      
                                                                                                                                    
                                                                                                                                    
