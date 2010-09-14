! //////////////////////////////////////////////////////////////////////////                                                        
                                                                                                                                    
      Subroutine SepVPA(BeginF, Bres, ARes)                                                                                         
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////////                                                       
!                                                                                                                                   
!         This routine perfomrs the following calculations:                                                                         
!                (1) fit a separable VPA by Pope and Shepherd (1987) method,                                                        
!                    with parameters in the 'Sepmodel.inc' block and with                                                           
!                    F at reference age in the last year = BeginF                                                                   
!                (2) puts initialising values in the Xbest parameter vector                                                         
!                (3) then calls a routine CONVPA which:                                                                             
!                         (2.1.) calculates a conventional VPA                                                                      
!                         (2.2.) calculates the fit of the surveys to VPA populations                                               
!                (4) returns the residuals of the surveys about the VPA populations                                                 
!                   as Bres (SSB surveys) and ARes (aged surveys)                                                                   
!                                                                                                                                   
!       The algorithm used is documented in :                                                                                       
!               Pope, J.G. and J.G. Shepherd (1982). A simple method for the consistent                                             
!                   interpretation of catch-ata-ge data. J. Cons. int. Explor. Mer 40:176-184.                                      
!                                                                                                                                   
!                                                                                                                                   
!          CALLS    :   CONVPA  (conventional VPA + survey fitting)                                                                 
!                       MISSVAL (treatment of missing and zero values)                                                              
!                                                                                                                                   
!               Last updated 11/1/1995 at 1000 h                                                                                    
!                                                                                                                                   
!                                                                                                                                   
                                                                                                                                    
                                                                                                                                    
      Include "INDAT.INC"                                                                                                           
      Include "SEPMODEL.INC"                                                                                                        
                                                                                                                                    
                                                                                                                                    
!     LOCAL VARIABLES                                                                                                               
                                                                                                                                    
      Double precision BeginF, BRes(maxbsurv), ARes(maxsurvey,maxage)                                                               
      Double precision D(maxyear+1,Maxage)            ! D are the catch ratios                                                      
      double precision DP(maxyear+1,maxage)                                                                                         
      double precision SepRes,OldRes                                                                                                
      double precision FS(maxyear), S(maxage), E, Z_here, F_here                                                                    
      Integer age, year, iage, iyear, parmno,  syear, l, k, test, i, j                                                              
      Integer kmax                                                                                                                  
      double precision lowest   ! used in allowing for missing catch data                                                           
      double precision MISSVAL, Z_next, Z_accum                                                                                     
                                                                                                                                    
!     --- SEPVPA : EXECUTABLE CODE ------------------------------------                                                             
                                                                                                                                    
      SepRes = 0d0                                                                                                                  
                                                                                                                                    
!      allocate (D(maxyear,maxage), stat = test)  ! D need only be used here, hence it is enough to only allocate the memory while i
!      if (test .ne. 0) then                                                                                                        
!        write(*,*) 'SEPVPA: Memory allocation error'                                                                               
!        stop                                                                                                                       
!      endif                                                                                                                        
                                                                                                                                    
!     find the lowest non-zero, non-negative datum in the catch-at-age matrix                                                       
      lowest = 1d20                                                                                                                 
      do iage = 1, lastage-firstage                                                                                                 
        do year = lastyear-NySep+1, lastyear                                                                                        
          iyear = year-firstyear+1                                                                                                  
          if ((CN(iyear,iage) .gt. 0d0 ).and.                           &                                                           
                 (lowest.gt.CN(iyear,iage))) then                                                                                   
            lowest = CN(iyear,iage)                                                                                                 
          endif                                                                                                                     
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
!     Copy parameters from the common block to local workspace                                                                      
                                                                                                                                    
      do age = firstage, lastage-1                                                                                                  
        iage = age-firstage+1                                                                                                       
        S(iage) = 1d0                                                                                                               
      enddo                                                                                                                         
                                                                                                                                    
      do i = 1, NYSep                                                                                                               
        FS(i) = BeginF                                                                                                              
      enddo                                                                                                                         
                                                                                                                                    
      do age= firstage, lastage-2                                                                                                   
        do year = lastyear-NySep+1, lastyear-1                                                                                      
        iage = age-firstage+1                                                                                                       
        iyear = year-lastyear+NySep                                                                                                 
        D(iyear,iage)=dlog(MISSVAL(CN(year-firstyear+2,iage+1),lowest)/ &                                                           
                         MISSVAL(CN(year-firstyear+1, iage),lowest))                                                                
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
      ! Minimise the catch ratio residuals (DP - D)**2                                                                              
                                                                                                                                    
      OldRes = 100d0                                                                                                                
                                                                                                                                    
      do  j =1,20  ! while (dabs((SepRes - OldRes)/OldRes) .gt. 1d-4)                                                               
        OldRes = SepRes                                                                                                             
        SepRes = 0d0                                                                                                                
        do i= 1, NySep                                                                                                              
          DP(i, lastage-firstage+1) = 0d0                                                                                           
        enddo                                                                                                                       
        do age=firstage, lastage-1                                                                                                  
          DP(22,age-firstage+1) = 0d0      ! Age totals summed here                                                                 
        enddo                                                                                                                       
        do year=lastyear-NySep+1, lastyear-1                                                                                        
          iyear = year- lastyear+NySep                                                                                              
          do age=firstage,lastage-2                                                                                                 
            iage = age-firstage+1                                                                                                   
            Z_here = FS(iyear) * S(iage) + NM(year-firstyear+1,iage)                                                                
            Z_next = FS(iyear+1)*S(iage+1)+NM(year-firstyear+2,iage+1)                                                              
            DP(iyear,iage) = dlog( (FS(iyear+1) * S(iage+1) *           &                                                           
            Z_here*(1-dexp(-Z_next))*dexp(-Z_here)   )/                 &                                                           
            (FS(iyear)*S(iage)*Z_next*(1-dexp(-Z_here)) )   )                                                                       
            SepRes = SepRes + (D(iyear,iage)-DP(iyear,iage))*           &                                                           
                              (D(iyear,iage)-DP(iyear,iage))                                                                        
                                                                                                                                    
            DP(iyear,lastage-firstage+1) = DP(iyear,lastage-firstage+1) &                                                           
             - D(iyear,iage) + DP(iyear,iage)                                                                                       
            DP(21+1,iage)=DP(21+1,iage)-D(iyear,iage)+  & ! Year totals summed here                                                  
                                        DP(iyear,iage)                                                                              
          enddo ! Ages                                                                                                              
        enddo   ! Years                                                                                                             
                                                                                                                                    
        do year = lastyear-NySep+1, lastyear-1                                                                                      
          iyear = year-lastyear+NySep                                                                                               
          FS(iyear) = FS(iyear)*dexp(DP(iyear,lastage-firstage+1)/      &                                                           
                              dble(lastage-firstage))                                                                               
        enddo                                                                                                                       
                                                                                                                                    
        do age = firstage,lastage-2                                                                                                 
          iage = age-firstage+1                                                                                                     
          S(iage) = S(iage)*dexp(DP(22,iage)/dble(NySep))                                                                           
        enddo                                                                                                                       
                                                                                                                                    
!       Renormalize to reference age                                                                                                
                                                                                                                                    
        do age = firstage,lastage-2                                                                                                 
          iage = age-firstage+1                                                                                                     
          S(iage) = S(iage)/S(refage-firstage+1)                                                                                    
        enddo                                                                                                                       
                                                                                                                                    
!      write(*,*) 'Separable Residuals : ',SepRes                                                                                   
                                                                                                                                    
      enddo ! the overall iteration of the separable model                                                                          
                                                                                                                                    
                                                                                                                                    
!     Copy the separable VPA parameters to the starting parameters for the minimisation proper                                      
                                                                                                                                    
      parmno = 0                                                                                                                    
                                                                                                                                    
      do i = 1, NySep                                                                                                               
         Xbest(i) = dlog(FS(i))                                                                                                     
         Xlow(i) = 0d0                                                                                                              
         Xhigh(i) = 0d0                                                                                                             
         parmno=parmno+1                                                                                                            
      enddo                                                                                                                         
                                                                                                                                    
      do age = firstage, lastage-1                                                                                                  
        iage = age-firstage+1                                                                                                       
        if (age .eq. refage) then                                                                                                   
           ! do nothing                                                                                                             
        else if (age .eq. lastage-1)  then                                                                                          
           ! do nothing                                                                                                             
        else if ((age .ne. RefAge) .and. (age .ne. lastage-1)) then                                                                 
           parmno = parmno+1                                                                                                        
           Xbest(parmno) = dlog(S(iage))                                                                                            
           Xlow(parmno) = 0d0                                                                                                       
           Xhigh(parmno) = 0d0                                                                                                      
        endif                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
!      The recruitments                                                                                                             
                                                                                                                                    
       do year = lastyear-NySep+2, lastyear                                                                                         
          iyear = year-lastyear+NySep                                                                                               
          kmax = MIN0(lastage-firstage, Lastyear-year+1)                                                                            
          N(year-firstyear+1,1) = 0d0                                                                                               
          do k = 1,kmax   ! K IS AGES/YEARS ALONG ONE COHORT                                                                        
            Z_here = FS(iyear+k-1)*S(k)+NM(year-firstyear+1+k-1,k)                                                                  
            Z_accum = 0d0                                                                                                           
            do l = 1, k-1                                                                                                           
               Z_accum = Z_accum + FS(iyear+l-1)*S(l)                   &                                                           
                                 + NM(year-firstyear+1+l-1,l)                                                                       
            enddo                                                                                                                   
                                                                                                                                    
            E = FS(iyear+k-1)*S(k)*(1-dexp(-Z_here))*dexp(-Z_accum)/    &                                                           
                                Z_here                                                                                              
            N(year-firstyear+1,1) = N(year-firstyear+1,1)+(             &                                                           
             dlog(MISSVAL(CN(year-firstyear+1+k-1,k),lowest))-dlog(E))                                                              
                                                                                                                                    
          enddo    ! AGES IN THE COHORT                                                                                             
          N(year-firstyear+1,1)= dexp(N(year-firstyear+1,1)/dble(kmax))                                                             
!          write(*,*) 'Recruitment ',year,N(year-firstyear+1,1)                                                                     
       enddo      ! YEARS                                                                                                           
                                                                                                                                    
!        SIMILARLY FOR THE POPULATIONS AT FIRST YEAR OF SEP ANALYSIS                                                                
                                                                                                                                    
       do age= firstage,lastage-1                                                                                                   
          year = lastyear-NySep+1                                                                                                   
          syear = year-lastyear+NySep                                                                                               
          iyear = year-firstyear+1                                                                                                  
          iage = age-firstage+1                                                                                                     
          kmax = MIN0(lastage-age, NySep)                                                                                           
!          write(*,*) 'year, age, kmax ',year, age, lastage-age,kmax                                                                
          N(iyear,iage) = 0d0                                                                                                       
          do k = 1,kmax   ! K IS AGES/YEARS ALONG ONE COHORT                                                                        
            Z_here = FS(syear+k-1)*S(iage+k-1)+NM(iyear+k-1,iage+k-1)                                                               
            Z_accum = 0d0                                                                                                           
            do l = 1, k-1                                                                                                           
               Z_accum = Z_accum + FS(syear+l-1)*S(iage+l-1)            &                                                           
                                 + NM(iyear+l-1,iage+l-1)                                                                           
            enddo                                                                                                                   
                                                                                                                                    
           E=FS(syear+k-1)*S(iage+k-1)*(1-dexp(-Z_here))*dexp(-Z_accum) &                                                           
                               / Z_here                                                                                             
            N(iyear,iage) = N(iyear, iage)+(                            &                                                           
             dlog( MISSVAL(CN(iyear+k-1,iage+k-1),lowest)) - dlog(E) )                                                              
          enddo ! ks                                                                                                                
          N(iyear,iage)= Dexp(N(iyear,iage)                             &                                                           
           /Dble(kmax))                                                                                                             
!          write(*,*) 'Start N : ',year,age,N(iyear,iage)                                                                           
      enddo      ! AGES                                                                                                             
                                                                                                                                    
!     Project the starting populations through the Fs to get ending populations,                                                    
!     which are the actual parameters the ICA2 estimates                                                                            
                                                                                                                                    
                                                                                                                                    
      do year = lastyear-NySep+2, lastyear                                                                                          
        iyear = year-firstyear+1                                                                                                    
        syear = year-lastyear+NySep                                                                                                 
        do age = firstage+1, lastage-1                                                                                              
          iage = age-firstage+1                                                                                                     
!          write(*,*) year, age, iyear, syear, iage                                                                                 
          N(iyear,iage) = N(iyear-1,iage-1)*dexp(-                      &                                                           
              (FS(syear-1)*S(iage-1) + NM(iyear-1,iage-1)))                                                                         
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
!    and here again, copy through to the Xbests                                                                                     
!    Populations in last year                                                                                                       
                                                                                                                                    
      iyear = lastyear-firstyear+1                                                                                                  
      do age = firstage, lastage-1                                                                                                  
        iage = age-firstage+1                                                                                                       
        parmno = parmno+1                                                                                                           
        Xbest(parmno) = dlog(N(iyear,iage))                                                                                         
        Xlow(parmno) = 0d0                                                                                                          
        Xhigh(parmno) = 0d0                                                                                                         
      enddo                                                                                                                         
                                                                                                                                    
      iage = lastage-firstage                                                                                                       
      do year = lastyear-NySep+1, lastyear-1                                                                                        
       iyear = year-firstyear+1                                                                                                     
       parmno = parmno+1                                                                                                            
       Xbest(parmno) = dlog(N(iyear,iage))                                                                                          
       Xlow(parmno) = 0d0                                                                                                           
       Xhigh(parmno) = 0d0                                                                                                          
      enddo                                                                                                                         
                                                                                                                                    
!      write(*,*) 'Ns written to Xbest '                                                                                            
                                                                                                                                    
!     Project the populations through to the end                                                                                    
                                                                                                                                    
      SepRes = 0d0                                                                                                                  
                                                                                                                                    
      do year = lastyear-NySep+1, lastyear-1                                                                                        
        iyear = year-firstyear+1                                                                                                    
        syear = year-lastyear+NySep                                                                                                 
        do age = firstage, lastage-2                                                                                                
          iage = age-firstage+1                                                                                                     
          Z_here = FS(syear)*S(iage)+NM(iyear,iage)                                                                                 
          F_here = FS(syear)*S(iage)                                                                                                
          N(iyear+1,iage+1)=N(iyear,iage)*exp(-Z_here)                                                                              
!          write(*,*) 'SEP N: ',year+1,age+1, N(iyear+1,iage+1)                                                                     
!     Calculate the residuals of the observed and expected catches                                                                  
                                                                                                                                    
         IF (CN(iyear,iage) .gt. 0d0) then                                                                                          
          SepRes=SepRes+(dlog(CN(iyear,iage))-                          &                                                           
           dlog(N(iyear,iage)*F_here/Z_here*(1d0-dexp(-Z_here)))) *     &                                                           
           (dlog(CN(iyear,iage)) -                                      &                                                           
           dlog(N(iyear,iage)*F_here/Z_here*(1d0-dexp(-Z_here))))                                                                   
         ENDIF ! not a missing value                                                                                                
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
      do year = firstyear, lastyear-nysep                                                                                           
         iyear= year-firstyear+1                                                                                                    
         do age = firstage, Refage+1                                                                                                
           iage = age-firstage+1                                                                                                    
           F(iyear,iage) = S(iage)*FS(1)                                                                                            
         enddo                                                                                                                      
      enddo                                                                                                                         
                                                                                                                                    
!      deallocate( D, stat=test)                                                                                                    
                                                                                                                                    
!      if (test .ne. 0) then                                                                                                        
!        write(*,*) 'Memory deallocation error in SepVPA : ', test                                                                  
!      endif                                                                                                                        
                                                                                                                                    
      Nxparm = parmno                                                                                                               
                                                                                                                                    
      CALL CONVPA(BRes,ARes,S,SepRES)                                                                                               
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                   
      Double precision function MISSVAL(A1, A2)                                                                                     
!                                                                                                                                   
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      double precision A1, A2     ! A1 is the CN value, A2 is lowest value in matrix                                                
                                                                                                                                    
!                                                                                                                                   
!      If a catch-at-age value is specified as equal to zero, it is                                                                 
!      replaced with the lowest value in the catch-at-age matrix.                                                                   
!      If is given as a negative value, the negative value is                                                                       
!      taken as rough guess to be used for the initialisation procedure.                                                            
!                                                                                                                                   
!      This is only for the SEPVPA algorithm                                                                                        
!                                                                                                                                   
                                                                                                                                    
      if (A1 .gt. 0) then                                                                                                           
        MISSVAL = A1                                                                                                                
      else                                                                                                                          
       if (A1 .eq. 0) then                                                                                                          
          MISSVAL = A2                                                                                                              
       else                                                                                                                         
         MISSVAL = -A1                                                                                                              
       endif                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
