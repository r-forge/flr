


! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine  MCMC   ! Markov Chain Monte Carlo for ICA
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////                                                             
!
!
!     This routine does the ICA Bayesian MCMC by ARS
!
!            Functions calls:    FUNCT1, similar to LSFUN but returns -ve loglikleihood
!                                FUNCT2, a null function for unifom priors

!                                MCOptions, chooses the burn-in, thin and chain length
!                                KP4, does the ARS


     IMPLICIT NONE
     include 'INDAT.INC'
     include 'SEPMODEL.INC'
     include 'MESSAGE1.INC'

     integer ithin, iburn, maxits, ierr, j, ribs, jj, i, k, nxp, MaxVPAParm
     double precision OldLH, Xprior, Xupper(maxparm), Xlower(maxparm), X2(mAXPARM),work(maxparm),X(maxparm)
     double precision Recipe_ran, random, MBAL, MBalMin,MBalMax
     character*132 errortext
     integer n_percentiles, nmax
     external Recipe_ran
     parameter(nmax=10)

     double precision percentile(nmax)

     ribs = 80
     Nxp=Nxparm
!     write(*,*) 'Parms: ',NXP

     call GetMCMCOptions(maxits, ithin, iburn)

     MBal=800000d0
     MBalMin=0d0
     MBalMax=1d15

     call GetPercentiles(N_percentiles, percentile, nmax)

     call Screen_in_r(HW(28,Language),MBal,MbalMax,MbalMin,Language)

!  set up max and min for all parameters

     do i =1, NxP
       Xlower(i)= Xbest(i)-dlog(3d0)
       Xupper(i) = Xbest(i)+dlog(3d0)
       X(i)=Xbest(i)
       X2(i)=Xbest(i)
     enddo


      call OPenItnFiles( (maxits-iburn)/ithin, MBAL,13)

      do i=1,maxits  ! Markov Chain iterates                                                                                        
        if (i-ithin*(i/ithin) .eq. 0) write(*,*)'Iteration: ', i,-OldLH   
                                                                                                                                    
        call Funct1(Nxp, X, OldLh) 
        call Funct2(Nxp, X, Xupper, Xlower, Xprior)
!        call UpdateWts (5d0) 
        Oldlh=-OldLh+Xprior                                                                                                         

        Random=Recipe_ran(2)                                                                                                      
          do k=1,NXP   ! Initial values are old values...                                                                           
            X2(k)=X(k)                                                                                                              
          enddo                                                                                                                     
                                                                                                                                    
          j = 1 + random*dble(Nxp)   ! J random variate between 1 and NXP                                                           

!         J is the parameter to be sampled

          ! Code to only calculate populations when needed

          MaxVPAParm = NySep  +(lastage-firstage+1)-2  +(lastage-firstage+1)-1 + NySep

          if (TwoSel) MaxVPAParm=MaxVPAParm+(lastage-firstage+1)-2

           
          if (J .gt. MaxVPAParm) then
            RecalculatePopulations = .false.
          else
            RecalculatePopulations = .true.
          endif


          ierr=0                                                                                                                  
!          write(*,*) i, j, OldLh
          call KPARS(X,X2,Xupper,Xlower,j,nxp,ierr,errortext,ribs,    &    
            Work,oldlh)
!          write(*,*) j, oldlh
            if (ierr .ne. 0) write(*,*) errortext                                                                                   
                                                                                                                                    
        if (i-ithin*(i/ithin) .eq. 0) then    

           Call writeIterations(X, 13)

        endif                                                                                                                       
                                                                                                                                    
        do jj=1,nxp
          if (X2(jj).lt. Xlower(jj)) then                                                                                           
             write(*,*) 'Param under BL : ',jj,X(jj)                                                                                
          endif                                                                                                                     
          if (Xupper(jj) .lt. X2(jj)) then                                                                                          
             write(*,*) 'Param over BU : ',jj,X(jj)                                                                                 
          endif                                                                                                                     
        enddo                                                                                                                       
                                                                                                                                    
                                                                                                                                    
         do jj=1,NXP   ! Update to next Markov Chain iteration                                                                       
            X(jj)=X2(jj)  
         enddo                                                                                                                      
                                                                                                                                    
                                                                                                                                    
       enddo !  MC its

      call CloseItnFiles(13)  



      call CreatePBYFiles(N_percentiles, Percentile,nmax)


!      write(*,*) 'Normal exit from MCMC'                                                                                          
      stop                                                                                                                          
      end                                                                                                                           
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           




! //////////////////////////////////////////////////////////////////////

       Subroutine GetMCMCOptions(maxit, ithin, iburn)

! //////////////////////////////////////////////////////////////////////

      implicit none

      include 'MESSAGE1.INC'
      integer maxit,ithin,iburn
      integer min, max


      ithin=100
      max=5000
      min=1
      Call Screen_in_i(HL(40,Language),ithin,max,min, language)

      iburn=1000
      max=5000
      min=0

      Call Screen_in_i(HL(41,Language),iburn,max,min, language)

      max=100000000
      min=10
      maxit=(ithin*1000)+iburn
      Call Screen_in_i(HL(39,Language),maxit,max,min,language)


      return
      end


! /////////////////////////////////////////////////////////////////////

     subroutine  Funct1(Nparm, X, OldLh)

! /////////////////////////////////////////////////////////////////////
     implicit none
     include 'INDAT.INC'
     include 'SEPMODEL.INC'

     double precision X(Maxparm), FC(maxdata), OldLH
     integer i, nparm

     full =.true.

     writeout=.false.
!	M.K.: swap the order of the first 2 arguments to enable MINUIT TO WORK
!     call LSFun1(nxdata,nparm,X,FC)
     call LSFun(nparm,nxdata,FC,X)

     oldlh =0d0
     do i=1,Nxdata
       oldlh=oldlh+FC(i)*FC(i)
     enddo

!     Funct1= OldLH  ! negative log-likelihood proportional to SSQ

!     write(*,*) OldLh
     return
     end



! /////////////////////////////////////////////////////////////////////

     subroutine  Funct2(Nxparm, X, Xupper, Xlower, Xprior)

! /////////////////////////////////////////////////////////////////////

     ! the prior function is a null function, so far.
     implicit none
     integer nxparm
     double precision X(Nxparm),Xupper(NxParm),Xlower(NxParm),Xprior

     Xprior=0d0

     return
     end



!c ///////////////////////////////////////////////////////////////////

       Double Precision Function SLOG(x)

!c //////////////////////////////////////////////////////////////////
!c
!c    an error-trapped log function
!c
        implicit none
        double precision x

        if ((x .le. 1d-23) .or. (x .ge. 1d23)) then
!c          write(*,*) 'Safe Log Error : ',x
          if (x .le. 1d-23) then
            SLOG=dlog(1d-23)
          endif
          if (x .ge. 1d23) then
            SLOG=dlog(1d23)
          endif
        else
          SLOG=dlog(x)
        endif
        return
        end

!c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



!c ///////////////////////////////////////////////////////////////////

       Double Precision Function SEXP(x)

!c //////////////////////////////////////////////////////////////////
!c
!c    an error-trapped dexp function
!c
        implicit none
        double precision x
        
        if ( dabs(x) .le. 53d0) then
          SEXP = dexp(x)
        else
!c          write(*,*) 'Safe Exp Error : ',x
          if (x .lt. 0) then
            SEXP=dexp(-53d0)
          else
            SEXP=dexp(53d0)
          endif
        endif

        return 
        end

!c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


