! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                
!    UNIT CVPA1 : holds two routines for conventional VPA. They                                                                     
!                 differ in the way they initialise the populations                                                                 
!                 in the last year. CVPA is conventional VPA over                                                                   
!                 all years of the analysis as initialised by                                                                       
!                 Pope & Shepherd separable VPA; CVPA2 is                                                                           
!                 conventional VPA with specified selection                                                                         
!                 pattern and run only up to a given end year.                                                                      
!                 CVPA2 is called by the objective function                                                                         
!                                                                                                                                   
!                                                                                                                                   
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                 
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////                                                               
!                                                                                                                                   
      Subroutine CVPA(S)                                                                                                            
!                                                                                                                                   
! ///////////////////////////////////////////////////////////////////                                                               
!                                                                                                                                   
!     Runs a conventional VPA based on separable selection pattern,                                                                 
!     over all years of data                                                                                                        
!                                                                                                                                   
!     Age-structured data are in common blocks, selection pattern                                                                   
!     is passed as a parameter                                                                                                      
!                                                                                                                                   
!     CNX is a temporary data structure holding the catches-at-age                                                                  
!     which have been adjusted for zero and missing vlaues.                                                                         
!                                                                                                                                   
!                                                                                                                                   
      Include "indat.inc"                                                                                                           
      Include "sepmodel.inc"                                                                                                        
                                                                                                                                    
!     LOCAL VARIABLES                                                                                                               
                                                                                                                                    
      integer j, iyear, iage, jyear, jage, test, age, year                                                                          
      double precision tolerance, S(maxage), lowest, f_here                                                                         
      double precision cohort, refine                                                                                               
      double precision CNX (maxyear,maxage)                                                                                         
!                                                                                                                                   
! --------------------- EXECUTABLE CODE ---------------------------------                                                           
                                                                                                                                    
!   Set up the catch matrix, with missing data replaced with lowest                                                                 
!   value                                                                                                                           
!   Except where specified as negative, whence take the -ve value as                                                                
!   a rough guess to be used for initialisation                                                                                     
!                                                                                                                                   
                                                                                                                                    
! ------------  allocate memory to a temporary array holding the catch-at-age matrix adjusted for                                   
! ------------- missing and zero values                                                                                             
                                                                                                                                    
!      allocate (CNX(maxyear,maxage), stat = test)                                                                                  
!      if (test .ne. 0) then                                                                                                        
!        write(*,*) 'Memory allocation error in CVPA'                                                                               
!        stop                                                                                                                       
!      endif                                                                                                                        
                                                                                                                                    
! ---------------- Find the lowest value in the catch-at-age matrix                                                                 
                                                                                                                                    
      lowest = 1d20                                                                                                                 
                                                                                                                                    
      do iyear =1, lastyear-firstyear+1                                                                                             
        do iage =1, lastage-firstage+1                                                                                              
          if ((CN(iyear,iage) .gt. 0d0) .and.                           &                                                           
              (CN(iyear,iage) .lt. lowest)) lowest = CN(iyear,iage)                                                                 
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
! ----------------- Form the catch matrix adjusted for missing and zero values                                                      
                                                                                                                                    
      do iyear = 1, lastyear-firstyear+1                                                                                            
        do iage = 1, lastage-firstage+1                                                                                             
          if (CN(iyear,iage) .ge. 0d0) then         ! datum is used                                                                 
             CNX(iyear,iage) = CN(iyear,iage)                                                                                       
          else                                                                                                                      
             if (CN(iyear,iage) .eq. 0d0) then      ! is a zero, replace with lowest value                                          
                CNX(iyear,iage) = lowest                                                                                            
             else                                                                                                                   
                CNX(iyear,iage) = - CN(iyear,iage)  ! is negative, ie missing: take +ve value as rough guess to be used             
             endif                                                                                                                  
          endif                                                                                                                     
        enddo    ! ages                                                                                                             
      enddo      ! years                                                                                                            
                                                                                                                                    
                                                                                                                                    
!  -------------------------  Conventional VPA initiated from Separable VPA                                                         
                                                                                                                                    
!     SET UP THE END COLUMN Fs FROM SEPARABLE MODEL FIT                                                                             

      F_here = 0.4d0 ! to initialise

      do age = firstage, lastage-1                                                                                                  
        iyear = lastyear-firstyear+1                                                                                                
        iage = age-firstage+1                                                                                                       
         do while ( dabs(F_here - F(iyear,iage)) .gt. 1d-6)                                                                         
          F_here = F(iyear,iage)                                                                                                    
          F(iyear,iage)=CNX(iyear,iage)*(F_here+NM(iyear,iage))         &                                                           
           /(N(iyear,iage)*(1d0-dexp(-F_here-NM(iyear,iage))))                                                                      
          F(iyear,iage) = DMIN1(F(iyear,iage), 3d0)                                                                                 
         enddo ! F iterations                                                                                                       
      enddo    ! ages                                                                                                               
                                                                                                                                    
!      write(*,*) 'End Column OK'                                                                                                   
                                                                                                                                    
      DO J = 1,8    ! ------------VPA iterations; precision of calculation increases with each pass                                 
                                                                                                                                    
      if (j .eq. 1) tolerance = 1d-5                                                                                                
      if (j .eq. 2) tolerance = 1d-6                                                                                                
      if (j .eq. 3) tolerance = 1d-7                                                                                                
      if (j .eq. 4) tolerance = 1d-8                                                                                                
      if (j .eq. 5) tolerance = 1d-9                                                                                                
      if (j .ge. 6) tolerance = 1d-9 !                                                                                              
                                                                                                                                    
!------------------------------- Take the separable Fs                                                                              
                                                                                                                                    
      do year = firstyear, lastyear-NySep                                                                                           
            iyear= year-firstyear+1                                                                                                 
            F(iyear,lastage-firstage+1) = 0d0                                                                                       
            do age = firstage+1, lastage-2                                                                                          
              iage= age-firstage+1                                                                                                  
              F(iyear,lastage-firstage+1)=F(iyear,lastage-firstage+1)+  &                                                           
               F(iyear,iage)/(S(iage) * dble(lastage-2-(firstage+1)+1))                                                             
            enddo  ! ages                                                                                                           
            F(iyear,lastage-firstage+1)=                                &                                                           
             F(iyear,lastage-firstage+1)*TermS                                                                                      
            F(iyear,lastage-firstage) = F(iyear,lastage-firstage+1)                                                                 
      enddo        ! years                                                                                                          
                                                                                                                                    
! check for missing catch-at-age values; copy in terminal F where CN is missing,                                                    
!  and put in a terminal population                                                                                                 
                                                                                                                                    
      do year = firstyear, lastyear-NySep                                                                                           
        iyear = year-firstyear+1                                                                                                    
        do iage = 1, lastage-firstage                                                                                               
          if (CNX(iyear,iage) .le. 1d-1) then                                                                                       
            if (iage .lt. lastage-firstage) F(iyear,iage) =  0d0                                                                    
            N(iyear, iage) = 0d0                                                                                                    
          else                                                                                                                      
            N(iyear,iage) = 1000d0                                                                                                  
          endif    ! catch datum missing                                                                                            
        enddo   ! iages                                                                                                             
      enddo   ! years                                                                                                               
                                                                                                                                    
                                                                                                                                    
!     SET UP THE BOTTOM ROW Fs FROM SEPARABLE MODEL FIT                                                                             
                                                                                                                                    
      do year = (lastyear-NySep+1), lastyear                                                                                        
         iyear = year-firstyear+1                                                                                                   
         iage = lastage-firstage                                                                                                    
         F(iyear,iage) = 0.5  ! starting guess                                                                                      
         do while ( dabs(F_here - F(iyear,iage)) .gt. 1d-5)                                                                         
           F_here = F(iyear,iage)                                                                                                   
           F(iyear,iage)=CNX(iyear,iage)*(F_here+NM(iyear,iage))        &                                                           
            /(N(iyear,iage)*(1d0-dexp(-F_here-NM(iyear,iage))))                                                                     
           F(iyear,iage) = DMIN1(F(iyear,iage), 3d0)  ! Upper limit on F                                                            
         enddo  ! F iteration                                                                                                       
      enddo     ! years                                                                                                             
                                                                                                                                    
1921  format(I2,1X,I2,1X,2F15.0,1X,F10.5,F12.0)                                                                                     
                                                                                                                                    
                                                                                                                                    
!     Back up along the cohorts from last age in each year ...                                                                      
                                                                                                                                    
      do year = lastyear-1, firstyear, -1                                                                                           
        iyear = year-firstyear+1                                                                                                    
        N(iyear, lastage-firstage+1) = 0d0  ! +gp Ns set to zero                                                                    
        iage = lastage-firstage ! start from last true age                                                                          
        jyear = iyear                                                                                                               
        jage = iage                                                                                                                 
        do while (( jage .ge. 1) .and. (jyear .ge. 1))                                                                              
          if (N(jyear+1,jage+1) .gt. 0d0)  then ! can do a cohort analysis                                                          
             F(jyear,jage) = Cohort(N(jyear+1,jage+1),CNX(jyear,jage),  &                                                           
                                 NM(jyear,jage))                                                                                    
             N(jyear,jage) = N(jyear+1,jage+1) * dexp(F(jyear,jage)+    &                                                           
                           NM(jyear,jage))                                                                                          
             F(jyear,jage)=Refine(N(jyear+1,jage+1), CNX(jyear,jage),   &                                                           
                        NM(jyear,jage), N(jyear,jage), F(jyear,jage),   &                                                           
                        tolerance)                                                                                                  
          else if (CNX(jyear,jage) .le. 1d-1 ) then ! N is set to zero if zero catch                                                
             N(jyear, jage) = 0d0                                                                                                   
             F(jyear,jage)  = F(jyear,refage-firstage+1) * S(jage)                                                                  
          else                                     ! set a terminal N from F on last true age                                       
             N(jyear,jage) = CNX(jyear,jage)*(F(jyear,lastage-firstage)+&                                                           
             NM(jyear,jage))/                                           &                                                           
             (F(jyear,lastage-firstage)*(1d0-dexp(-(F(jyear,            &                                                           
             lastage-firstage)+NM(jyear,jage)))))                                                                                   
             F(jyear,jage)  = F(jyear,lastage-firstage)                                                                             
          endif                                                                                                                     
          jage = jage -1                                                                                                            
          jyear = jyear -1                                                                                                          
        enddo  ! going back up cohort                                                                                               
      enddo    ! years                                                                                                              
                                                                                                                                    
!      pause                                                                                                                        
                                                                                                                                    
100   format (' ',I4,1X,I4,1X,F8.0,1X,F8.4,1X,F8.0)                                                                                 
                                                                                                                                    
! and now the same for the cohorts finishing in the last year                                                                       
                                                                                                                                    
      do age = lastage-2, firstage, -1                                                                                              
        iyear = lastyear-firstyear  ! start from last year-1                                                                        
        iage = age-firstage+1                                                                                                       
        jyear = iyear                                                                                                               
        jage = iage                                                                                                                 
        do while (( jage .ge. 1) .and. (jyear .ge. 1))                                                                              
          if (N(jyear+1,jage+1) .gt. 0d0)  then ! can do a cohort analysis                                                          
             F(jyear,jage) = Cohort(N(jyear+1,jage+1),CNX(jyear,jage),  &                                                           
                                 NM(jyear,jage))                                                                                    
             N(jyear,jage) = N(jyear+1,jage+1) * dexp(F(jyear,jage)+    &                                                           
                           NM(jyear,jage))                                                                                          
             F(jyear,jage)=Refine(N(jyear+1,jage+1), CNX(jyear,jage),   &                                                           
                        NM(jyear,jage), N(jyear,jage), F(jyear,jage),   &                                                           
                        tolerance)                                                                                                  
!           write(*,*) jyear,jage,F(jyear,jage),N(jyear,jage)                                                                       
          else if (CNX(jyear,jage) .le. 1d-1 ) then ! N is set to zero                                                              
             N(jyear, jage) = 0d0                                                                                                   
          else                                     ! set a terminal N from F already in the matrix                                  
             N(jyear,jage) = CNX(jyear,jage)*(F(jyear,jage)+            &                                                           
             NM(jyear,jage))/                                           &                                                           
             (F(jyear,jage)*(1d0-dexp(-(F(jyear,jage)+NM(jyear,jage)))))                                                            
          endif                                                                                                                     
          jage = jage -1                                                                                                            
          jyear = jyear -1                                                                                                          
        enddo  ! going back up cohort                                                                                               
      enddo    ! ages                                                                                                               
                                                                                                                                    
!      A 'tweak' for zero values in the catch at age matrix                                                                         
!         - project the Ns forwards from the last catch-at-age observation                                                          
!           using the terminal Fs in the appropriate years                                                                          
!                                                                                                                                   
       do iyear = 2, lastyear- firstyear+1                                                                                          
         do iage= 2, lastage-firstage  ! can't do this for the first age !                                                          
           if (N(iyear,iage) .eq. 0) then                                                                                           
             jyear = iyear                                                                                                          
             jage  = iage                                                                                                           
             do while ((jyear .le. lastyear-firstyear+1) .and.          &                                                           
                    (jage  .le. lastage-firstage))                                                                                  
                N(jyear,jage) = N(jyear-1,jage-1)                       &                                                           
                   *dexp(-F(jyear-1,jage-1)-NM(jyear-1,jage-1))                                                                     
                jyear=jyear+1                                                                                                       
                jage = jage+1                                                                                                       
             enddo  ! going down the cohort                                                                                         
           endif ! no numbers at age                                                                                                
         enddo                                                                                                                      
       enddo                                                                                                                        
                                                                                                                                    
!       write(*,*) 'CVPA: Conventional VPA OK'                                                                                      
                                                                                                                                    
      ! now deal with the plus-group                                                                                                
                                                                                                                                    
!     All +gp Fs assumed equal to F on last true age group                                                                          
                                                                                                                                    
      do year =firstyear, lastyear                                                                                                  
        iyear = year-firstyear+1                                                                                                    
            F(iyear, lastage-firstage+1) = F(iyear, lastage-firstage)                                                               
      enddo  ! years                                                                                                                
                                                                                                                                    
!      write(*,*) 'CVPA : Plus gp OK'                                                                                               
                                                                                                                                    
!     +gp Ns calculated from backwards catch equation using +gp catches                                                             
                                                                                                                                    
      do year = firstyear, lastyear                                                                                                 
          iyear = (year-firstyear+1)                                                                                                
          iage = lastage-firstage+1                                                                                                 
          if (CNX(iyear,iage) .gt. 1d-1) then ! use catch equation                                                                  
                                                                                                                                    
!       Error in plus-gp N has been corrected here                                                                                  
                                                                                                                                    
       N(iyear,iage)=(CNX(iyear,iage)*(F(iyear,iage)+NM(iyear,iage)))/  &                                                           
       (F(iyear,iage)*(1d0-dexp(-F(iyear,iage)-NM(iyear,iage))))                                                                    
                                                                                                                                    
          else  if (year .ne. firstyear)  then ! missing value: fill in                                                             
            N(iyear,iage) = N(iyear-1,iage)*dexp(-F(iyear-1,iage)       &                                                           
                -NM(iyear-1,iage))                                      &                                                           
                + N(iyear-1,iage-1)*dexp(-F(iyear-1,iage-1)             &                                                           
                -NM(iyear-1,iage-1))                                                                                                
          endif                                                                                                                     
      enddo  ! years                                                                                                                
                                                                                                                                    
!     Fill in the plus-group where there are zero values in the catch                                                               
!     at age matrix                                                                                                                 
                                                                                                                                    
                                                                                                                                    
!     Project the population one more year forwards                                                                                 
                                                                                                                                    
      DO age = firstage, lastage-2                                                                                                  
        iage = age-firstage+1                                                                                                       
        iyear = lastyear-firstyear+1                                                                                                
        N(iyear+1,iage+1) = N(iyear,iage) * dexp(-F(iyear,iage)-        &                                                           
                                                  NM(iyear,iage))                                                                   
      enddo  ! Ages                                                                                                                 
                                                                                                                                    
!      write(*,*) 'CVPA : Projection of +gp '                                                                                       
                                                                                                                                    
!     Project the plus-group forwards with F, add the oldest true age                                                               
!     group to the plus group                                                                                                       
                                                                                                                                    
      iage = lastage-firstage+1                                                                                                     
!     iyear = lastyear-firstyear+1                                                                                                  
                                                                                                                                    
      N(iyear+1, iage) = (N(iyear,iage) *                               &                                                           
            dexp(-(F(iyear,iage)+NM(iyear,iage)))) +                    &                                                           
            (N(iyear,iage-1)*dexp(-(F(iyear,iage-1)+NM(iyear,iage-1))))                                                             
                                                                                                                                    
!     Copy the Fs forwards one more year, so that indices of abundance                                                              
!     one year after the last year of catch data can be fitted                                                                      
                                                                                                                                    
      do age = firstage, lastage                                                                                                    
         iyear = lastyear-firstyear+1                                                                                               
         iage = age-firstage+1                                                                                                      
         F(iyear+1, iage) = F(iyear, iage)                                                                                          
      enddo  ! Ages                                                                                                                 
                                                                                                                                    
!      write(*,*) 'CVPA: Forwards projection'                                                                                       
                                                                                                                                    
      ENDDO ! J VPA ITERATIONS : CAREFUL THIS IS A BODGE AND NEEDS OPTIMISING                                                       
                                                                                                                                    
!      deallocate(CNX)                                                                                                              
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////                                                               
!                                                                                                                                   
      Subroutine CVPA2(S,endyear)                                                                                                   
!                                                                                                                                   
! ///////////////////////////////////////////////////////////////////                                                               
!                                                                                                                                   
!     Runs a conventional VPA based on separable selection pattern,                                                                 
!     ending in endyear.                                                                                                            
!                                                                                                                                   
!     Populations in endyear will be used to initiate the VPA.                                                                      
!                                                                                                                                   
!     CVPA2 is called by LSFUN1; CVPA is called by CONVPA                                                                           
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      Include "indat.inc"                                                                                                           
      Include "sepmodel.inc"                                                                                                        
                                                                                                                                    
!     LOCAL VARIABLES                                                                                                               
                                                                                                                                    
      integer j, iyear, iage, jyear, jage, endyear, age, year                                                                       
      double precision tolerance, S(maxage), f_here                                                                                 
      double precision cohort, refine                                                                                               
                                                                                                                                    
!     Conventional VPA initiated from Separable populations                                                                         
                                                                                                                                    
                                                                                                                                    
      DO J = 1,8                                                                                                                    
                                                                                                                                    
      if (j .eq. 1) tolerance = 1d-5                                                                                                
      if (j .eq. 2) tolerance = 1d-6                                                                                                
      if (j .eq. 3) tolerance = 1d-7                                                                                                
      if (j .eq. 4) tolerance = 1d-8                                                                                                
      if (j .eq. 5) tolerance = 1d-9                                                                                                
      if (j .ge. 6) tolerance = 1d-9 !                                                                                              
                                                                                                                                    
!      Set up the bottom row Fs, by estimating what the 'reference F'                                                               
!      would be from the selection pattern from the second to the                                                                   
!      penultimate true ages, and multiplying this by the terminal                                                                  
!      selection                                                                                                                    
                                                                                                                                    
      do year = firstyear, endyear-1                                                                                                
            iyear= year-firstyear+1                                                                                                 
            F(iyear,lastage-firstage+1) = 0d0                                                                                       
            do age = firstage+1, lastage-2                                                                                          
              iage= age-firstage+1                                                                                                  
              F(iyear,lastage-firstage+1)=F(iyear,lastage-firstage+1)+  &                                                           
               F(iyear,iage)/(S(iage) * dble(lastage-2-(firstage+1)+1))                                                             
            enddo  ! ages                                                                                                           
            F(iyear,lastage-firstage+1)=                                &                                                           
             F(iyear,lastage-firstage+1)*TermS                                                                                      
            F(iyear,lastage-firstage) = F(iyear,lastage-firstage+1)                                                                 
      enddo        ! years                                                                                                          
                                                                                                                                    
! check for missing catch-at-age values;                                                                                            
!  and put in a zero population as a marker                                                                                         
                                                                                                                                    
      do year = firstyear, endyear-1                                                                                                
        iyear = year-firstyear+1                                                                                                    
        do iage = 1, lastage-firstage                                                                                               
          if (CN(iyear,iage) .le. 1d-1) then                                                                                        
            if (iage .lt. lastage-firstage) F(iyear,iage) =  0d0                                                                    
            N(iyear, iage) = 0d0                                                                                                    
!         else                                                                                                                      
!           N(iyear,iage) = 1000d0                                                                                                  
          endif    ! catch datum missing                                                                                            
        enddo   ! iages                                                                                                             
      enddo   ! years                                                                                                               
                                                                                                                                    
                                                                                                                                    
1921  format(I2,1X,I2,1X,2F15.0,1X,F10.5,F12.0)                                                                                     
                                                                                                                                    
                                                                                                                                    
!     Back up along the cohorts from last true age from the first year                                                              
!     to the last year of the conventional VPA                                                                                      
                                                                                                                                    
      do year = endyear-1, firstyear, -1                                                                                            
        iyear = year-firstyear+1                                                                                                    
        N(iyear+1, lastage-firstage+1) = 0d0  ! +gp Ns set to zero                                                                  
        iage = lastage-firstage ! start from last true age                                                                          
        jyear = iyear                                                                                                               
        jage = iage                                                                                                                 
        do while (( jage .ge. 1) .and. (jyear .ge. 1))                                                                              
          if (N(jyear+1,jage+1) .gt. 0d0)  then ! can do a cohort analysis                                                          
             F(jyear,jage) = Cohort(N(jyear+1,jage+1),CN(jyear,jage),   &                                                           
                                 NM(jyear,jage))                                                                                    
             N(jyear,jage) = N(jyear+1,jage+1) * dexp(F(jyear,jage)+    &                                                           
                           NM(jyear,jage))                                                                                          
             F(jyear,jage)=Refine(N(jyear+1,jage+1), CN(jyear,jage),    &                                                           
                        NM(jyear,jage), N(jyear,jage), F(jyear,jage),   &                                                           
                        tolerance)                                                                                                  
          else if (CN(jyear,jage) .le. 1d-1 ) then ! N is set to zero if zero catch                                                 
             N(jyear, jage) = 0d0                                                                                                   
             F(jyear,jage)  = F(jyear,refage-firstage+1) * S(jage)                                                                  
          else                                     ! set a terminal N from F on last true age                                       
             N(jyear,jage) = CN(jyear,jage)*(F(jyear,lastage-firstage)+ &                                                           
             NM(jyear,jage))/                                           &                                                           
             (F(jyear,lastage-firstage)*(1d0-dexp(-(F(jyear,            &                                                           
             lastage-firstage)+NM(jyear,jage)))))                                                                                   
             F(jyear,jage)  = F(jyear,lastage-firstage)                                                                             
          endif                                                                                                                     
          jage = jage -1                                                                                                            
          jyear = jyear -1                                                                                                          
        enddo  ! going back up cohort                                                                                               
      enddo    ! years                                                                                                              
                                                                                                                                    
                                                                                                                                    
100   format (' ',I4,1X,I4,1X,F8.0,1X,F8.4,1X,F8.0)                                                                                 
                                                                                                                                    
! and now the same for the cohorts finishing in the last year                                                                       
                                                                                                                                    
      do age = lastage-2, firstage, -1  ! start from  penultimate true age                                                          
        iyear = endyear-firstyear       ! start from finishing year-1                                                               
        iage = age-firstage+1                                                                                                       
        jyear = iyear                                                                                                               
        jage = iage                                                                                                                 
        do while (( jage .ge. 1) .and. (jyear .ge. 1))                                                                              
          if (N(jyear+1,jage+1) .gt. 0d0)  then ! can do a cohort analysis                                                          
             F(jyear,jage) = Cohort(N(jyear+1,jage+1),CN(jyear,jage),   &                                                           
                                 NM(jyear,jage))                                                                                    
             N(jyear,jage) = N(jyear+1,jage+1) * dexp(F(jyear,jage)+    &                                                           
                           NM(jyear,jage))                                                                                          
             F(jyear,jage)=Refine(N(jyear+1,jage+1), CN(jyear,jage),    &                                                           
                        NM(jyear,jage), N(jyear,jage), F(jyear,jage),   &                                                           
                        tolerance)                                                                                                  
          else if (CN(jyear,jage) .le. 1d-2 ) then ! N is set to zero                                                               
             N(jyear, jage) = 0d0                                                                                                   
          else            ! set a terminal N from F already in the matrix                                                           
             N(jyear,jage) = CN(jyear,jage)*(F(jyear,jage)+             &                                                           
             NM(jyear,jage))/                                           &                                                           
             (F(jyear,jage)*(1d0-dexp(-(F(jyear,jage)+NM(jyear,jage)))))                                                            
          endif                                                                                                                     
          jage = jage -1                                                                                                            
          jyear = jyear -1                                                                                                          
        enddo  ! going back up cohort                                                                                               
      enddo    ! ages                                                                                                               
                                                                                                                                    
!      A 'tweak' for zero values in the catch at age matrix                                                                         
!         - project the Ns forwards from the last catch-at-age observation                                                          
!           using the terminal Fs in the appropriate years                                                                          
!                                                                                                                                   
       do iyear = 2, endyear- firstyear+1                                                                                           
         do iage= 2, lastage-firstage                                                                                               
           if (N(iyear,iage) .eq. 0) then                                                                                           
             jyear = iyear                                                                                                          
             jage  = iage                                                                                                           
             do while ((jyear .le. lastyear-firstyear+1) .and.          &                                                           
                    (jage  .le. lastage-firstage))                                                                                  
                N(jyear,jage) = N(jyear-1,jage-1)                       &                                                           
                   *dexp(-F(jyear-1,jage-1)-NM(jyear-1,jage-1))                                                                     
                jyear=jyear+1                                                                                                       
                jage = jage+1                                                                                                       
             enddo  ! going down the cohort                                                                                         
           endif ! no numbers at age                                                                                                
         enddo                                                                                                                      
       enddo                                                                                                                        
                                                                                                                                    
!       write(*,*) 'SEPVPA: Conventional VPA OK'                                                                                    
                                                                                                                                    
      ! now deal with the plus-group up to endyear                                                                                  
                                                                                                                                    
!     All +gp Fs assumed equal to F on last true age group                                                                          
                                                                                                                                    
      do year =firstyear, endyear                                                                                                   
        iyear = year-firstyear+1                                                                                                    
            F(iyear, lastage-firstage+1) = F(iyear, lastage-firstage)                                                               
      enddo  ! years                                                                                                                
                                                                                                                                    
!      write(*,*) 'CONVPA : Plus gp OK'                                                                                             
                                                                                                                                    
!     +gp Ns calculated from backwards catch equation using +gp catches                                                             
                                                                                                                                    
      do year = firstyear, endyear                                                                                                  
          iyear = (year-firstyear+1)                                                                                                
          iage = lastage-firstage+1                                                                                                 
          if (CN(iyear,iage) .gt. 1d-1) then ! use catch equation                                                                   
                                                                                                                                    
!       Error in plus-gp N has been corrected here                                                                                  
                                                                                                                                    
        N(iyear,iage)=(CN(iyear,iage)*(F(iyear,iage)+NM(iyear,iage)))/  &                                                           
         (F(iyear,iage)*(1d0-dexp(-F(iyear,iage)-NM(iyear,iage))))                                                                  
                                                                                                                                    
          else if (year .ne. firstyear) then !                                                  Missing value: fill in              
            N(iyear,iage) = N(iyear-1,iage)*dexp(-F(iyear-1,iage)       &                                                           
                -NM(iyear-1,iage))                                      &                                                           
                + N(iyear-1,iage-1)*dexp(-F(iyear-1,iage-1)             &                                                           
                -NM(iyear-1,iage-1))                                                                                                
          endif                                                                                                                     
      enddo  ! years                                                                                                                
                                                                                                                                    
      ENDDO ! J VPA ITERATIONS : CAREFUL THIS IS A BODGE AND NEEDS OPTIMISING                                                       
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!///////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
      double precision Function CalcSSB(Year)                                                                                       
!                                                                                                                                   
!///////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
!     Calculates the SSB in a given year from fitted populations,                                                                   
!                    at the time of spawning.                                                                                       
!                                                                                                                                   
      Include "indat.inc"                                                                                                           
                                                                                                                                    
                                                                                                                                    
!     Local variables                                                                                                               
                                                                                                                                    
      integer year, iage                                                                                                            
      double precision ssb                                                                                                          
                                                                                                                                    
                                                                                                                                    
!     ____________________EXECUTABLE CODE______________________________                                                             
                                                                                                                                    
      ssb = 0d0                                                                                                                     
      do iage = 1,lastage-firstage+1                                                                                                
	if (MO(year-firstyear+1, iage) .gt. 0d0) then                                                                                      
	 ssb=ssb+(N(year-firstyear+1, iage)*                                   &                                                           
                 dble(MO(year-firstyear+1, iage))*                      &                                                           
                 dble(SW(year-firstyear+1, iage))*                      &                                                           
                 dexp(- F(year-firstyear+1, iage)*PF                    &                                                           
                      -NM(year-firstyear+1, iage)*PM) )                                                                             
	endif                                                                                                                              
      enddo                                                                                                                         
      CalcSSB = SSB                                                                                                                 
      return                                                                                                                        
      end                                                                                                                           
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                             
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!///////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      double precision Function Cohort (Ni, Ci, Mi)                                                                                 
                                                                                                                                    
!///////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
!         Does cohort analysis by Pope's approximation.                                                                             
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!     local variables                                                                                                               
                                                                                                                                    
      double precision Nn, result                                                                                                   
      double precision Mi, Ci, Ni                                                                                                   
                                                                                                                                    
!     ___________________EXECUTABLE CODE________________________________                                                            
                                                                                                                                    
                                                                                                                                    
      Nn = Ni * dexp(Mi) + (Ci* dexp(Mi/2d0))                                                                                       
      result = dlog(Nn/Ni)-Mi                                                                                                       
                                                                                                                                    
      cohort = result                                                                                                               
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!///////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
      double precision Function Refine(Nplus, Ci, Mi, Ni, Fi, precis)                                                               
!                                                                                                                                   
!///////////////////////////////////////////////////////////////////////                                                            
!     Refine initial estimates obtained by cohort analysis: Lowestoft method                                                        
!     note the G = dY/dF                                                                                                            
!                                                                                                                                   
!     This code gives true VPA to a precision which is specified with                                                               
!     the <precis> parameter. This is a tolerance in the estimated population                                                       
!     size. <precis> should obviously never be less than machine precision,                                                         
!     which is about 1d-16 for IBM PC and MS-FORTRAN.                                                                               
!     For more efficient processing, increase the precision as the VPA converges.                                                   
!                                                                                                                                   
                                                                                                                                    
!     local variables                                                                                                               
                                                                                                                                    
      double precision Y, G, Z, Nplus, Ci, Mi, Ni, Fi, Gprime                                                                       
      double precision precis, lastNi                                                                                               
      integer i                                                                                                                     
                                                                                                                                    
      Z = Fi+Mi                                                                                                                     
      Gprime = dexp(-3d0*Mi/2d0)                                                                                                    
      G = Gprime* (Ni/Nplus)                                                                                                        
                                                                                                                                    
      Y = 1.0d0                                                                                                                     
      LastNi = 0d0                                                                                                                  
                                                                                                                                    
      i = 0                                                                                                                         
      do while (( dabs(Ni-LastNi).gt.precis).and.(i.le.1000)) ! Iterates down to precis                                             
	i = i+1                                                                                                                            
	LastNi = Ni                                                                                                                        
	Y = dexp(-Z) * (Ci/Nplus) - ((Fi/Z)*(1d0-dexp(-Z)))                                                                                
	Z = Z + (Y/(G*dexp(-Fi)))                                                                                                          
	Fi = Z -Mi                                                                                                                         
	Ni = Nplus * dexp(Z)                                                                                                               
	G = Gprime* (Ni/Nplus)                                                                                                             
      enddo                                                                                                                         
                                                                                                                                    
!      if (i .gt. 999) write(*,*) 'Refine : VPA precision not attained'                                                             
                                                                                                                                    
      Refine = Fi                                                                                                                   
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
       Subroutine CalcSOP                                                                                                           
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
!        Calculate Sum-Of-Products                                                                                                  
                                                                                                                                    
       Include "indat.inc"                                                                                                          
       integer iyear, iage                                                                                                          
                                                                                                                                    
       do iyear = 1, lastyear-firstyear+1                                                                                           
	 SOP(iyear) = 0d0                                                                                                                  
	 do iage=1, lastage-firstage+1                                                                                                     
	   SOP(iyear) = SOP(iyear)+ CW(iyear,iage)*CN(iyear,iage)                                                                          
	 enddo                                                                                                                             
       enddo                                                                                                                        
                                                                                                                                    
       do iyear = 1, lastyear-firstyear+1                                                                                           
         SOP(iyear) = LA(iyear)*100.0/SOP(iyear)                                                                                    
       enddo                                                                                                                        
                                                                                                                                    
       return                                                                                                                       
       end                                                                                                                          
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
