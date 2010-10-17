! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine ShrinkF(Wk, Lw, Nv, MinCV, Shrinkyr, filename)                                                                     
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////                                                             
!                                                                                                                                   
!    Generates a 'shrunk' VPA based on ICA2 model fit. The new VPA is                                                               
!         written to a new output file called ICA.SHR; this will be                                                                 
!         different from the populations in ICA.OUT, which will not                                                                 
!         be shrunk.                                                                                                                
!                                                                                                                                   
!    The shrunk VPA is initiated with Fs in the last year which are a weighted mean of                                              
!       a historic mean F (MF) over a span of years specified by Shrinkyr, and an                                                   
!       F which is derived from the model estimate (Fest). The two estimates at each                                                
!       age are combined in a mean weighted by the inverse of the estimates of the variances                                        
!       attached to each estiamte.                                                                                                  
!              The variance of MF is found from the variance of the F at                                                            
!       age across years in the F matrix; the variance of the Fest is found                                                         
!       by the delta method from the estimates of variance and covariance of                                                        
!       F at reference age in the last year and of S at each age.                                                                   
!              Then a weighted mean F is formed from the two estimates and their                                                    
!       variances and used to initiate a conventional VPA.                                                                          
!                                                                                                                                   
!                                                                                                                                   
!      Last updated 13 January at 1700                                                                                              
!                                                                                                                                   
!                                                                                                                                   
      include "indat.inc"                                                                                                           
      include "sepmodel.inc"                                                                                                        
      include 'message1.inc'                                                                                                        
                                                                                                                                    
      double precision Wk(lw)         ! the variance-covariance matrix in linear form, returned by E04YCF                           
      integer nv, shrinkyr            ! nv is used to index the WK array; see NAG routine documentaion;                             
                                      !    number of years for shrinkage                                                            
      double precision Mincv          ! minimum allowable CV for the historic mean                                                  
      character*10 filename           ! of the file for output                                                                      
                                                                                                                                    
!     Local variables                                                                                                               
                                                                                                                                    
      double precision VarMF(maxage)                 ! Variance of mean log F                                                       
      double precision VarFest(maxage)               ! Variance of recent log F estimate                                            
      double precision Fest(maxage)                  ! F estimate from model fit                                                    
      double precision MF(maxage)                    ! F estimate from historic mean                                                
      double precision theta(maxage)                 ! the inverse-variance weights                                                 
      double precision S(maxage)                     ! Selection at age                                                             
      double precision TotB(maxyear), SSB(maxyear)         ! total and spawning biomasses                                           
      integer age, iage, year, iyear, parmno, its                                                                                   
      double precision Z_here                                                                                                       
      double precision cohort, refine ! VPA functions                                                                               
      double precision CalcSSB        ! VPA to SSB function                                                                         
      character*80 Text(3)                                                                                                          
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ---------------- EXECUTABLE CODE ------------------------------------                                                             
                                                                                                                                    
!------------ First calculate the variance of current-year F at age by delta method                                                 
                                                                                                                                    
                                                                                                                                    
       do iyear= 1, lastyear-firstyear+1                                                                                            
         do iage = 1, firstage-lastage+1                                                                                            
           if (CN(iyear, iage) .lt. 0d0) then                                                                                       
             Text(1) = HM(50,Language)                                                                                              
             call screen_out_a(Text,3,1)                                                                                            
             goto 1077                                                                                                              
           endif                                                                                                                    
         enddo                                                                                                                      
       enddo                                                                                                                        
                                                                                                                                    
                                                                                                                                    
       Text(1)= HM(49,1)                                                                                                            
       call screen_out_a(Text,3,1)                                                                                                  
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      ! --------------------- First the variance of FS*S from the model fit                                                         
                                                                                                                                    
       parmno = NySep                                                                                                               
       If (TwoSel) parmno=parmno+Lastage-firstage+1-3  ! two selection model                                                        
                                                                                                                                    
       do age = firstage, lastage                                                                                                   
          iage = age-firstage+1                                                                                                     
          if ( age .eq. Refage ) then                                                                                               
            VarFest(iage) =  Wk(nv+(NySep-1)+((NySep-1)*nxparm))                                                                    
            Fest(iage) = Xbest(NySep)                                                                                               
            S(iage) = 1.0                                                                                                           
          else if (age  .eq. lastage-1) then                                                                                        
            VarFest(iage) =                                             &                                                           
                          Wk(nv+(NySep-1)+((NySep-1)*nxparm))                                                                       
            Fest(iage) = Xbest(NySep)+ dlog(TermS)                                                                                  
            S(iage) = TermS                                                                                                         
          else                                                                                                                      
            parmno = parmno+1                                                                                                       
            VarFest(iage) = Wk(nv+(NySep-1)+((NySep-1)*nxparm)) +       &                                                           
                            Wk(nv+(parmno-1)+((parmno-1)*nxparm))+      &                                                           
                       2d0* Wk(nv+(NySep-1)+((parmno-1)*nxparm))                                                                    
            Fest(iage) = Xbest(NySep) + Xbest(parmno)       !--------------- ie log(F in last year) + log(Selection at age)         
            S(iage) = dexp(Xbest(parmno))+1e-6                                                                                      
          endif                                                                                                                     
       enddo ! ages                                                                                                                 
                                                                                                                                    
! Now the variances at age of log(F) over previous Shrinkyr years                                                                   
                                                                                                                                    
       do iage = 1, lastage-firstage+1                                                                                              
         MF(iage) = 0.0                                                                                                             
         VarMF(iage) = 0.0                                                                                                          
       enddo                                                                                                                        
                                                                                                                                    
       do year = lastyear-shrinkyr+1, lastyear                                                                                      
         iyear = year-firstyear+1                                                                                                   
         do iage = 1, lastage-firstage+1                                                                                            
           MF(iage) = MF(iage) + dlog(F(iyear,iage))                                                                                
           VarMF(iage) = VarMF(iage) + dlog(F(iyear,iage))              &                                                           
                                      *dlog(F(iyear,iage))                                                                          
         enddo                                                                                                                      
       enddo                                                                                                                        
                                                                                                                                    
       do iage = 1, lastage-firstage+1                                                                                              
          VarMF(iage)=(VarMF(iage)-MF(iage)*MF(iage)/float(shrinkyr))/  &                                                           
                        float(shrinkyr-1)                                                                                           
          MF(iage) = MF(iage)/float(shrinkyr)                                                                                       
          VarMF(iage) = VarMF(iage) / float(shrinkyr)  ! var of estimate of mean                                                    
          if ( VarMF(iage) .lt. MinCV*MinCV)                            &                                                           
             VarMF(iage) = MinCV*MinCV                                                                                              
       enddo                                                                                                                        
                                                                                                                                    
!     Calculate the inverse-variance weights, theta(iage)                                                                           
                                                                                                                                    
      do iage = 1, lastage-firstage+1                                                                                               
         theta(iage) = (VarMF(iage)) /                                  &                                                           
                       (VarFest(iage) + VarMF(iage) )                                                                               
!         write(*,*) 'theta ',iage,theta(iage)                                                                                      
      enddo                                                                                                                         
                                                                                                                                    
!     Plug in the shrunk estimates of F into the last year of the VPA                                                               
                                                                                                                                    
      do iage = 1, lastage-firstage+1                                                                                               
        F(lastyear-firstyear+1,iage)=dexp( (1d0-theta(iage))*MF(iage)+  &                                                           
                              theta(iage)*Fest(iage) )                                                                              
      enddo                                                                                                                         
                                                                                                                                    
!     Now a conventional VPA started with these Fs, assuming constant                                                               
!     selection over the whole series.                                                                                              
                                                                                                                                    
      do its = 1,20    !----------------------- 20 VPA iterations                                                                   
                                                                                                                                    
                       !----------------------- set up  F on last true age and on +gp                                               
        do year = firstyear, lastyear                                                                                               
          iyear = year-firstyear+1                                                                                                  
          F(iyear, lastage-firstage) = 0d0                                                                                          
          do age = firstage, lastage-2                                                                                              
            iage =age-firstage+1                                                                                                    
            F(iyear, lastage-firstage) = F(iyear, lastage-firstage)+    &                                                           
              S(lastage-firstage) * F(iyear, iage)/(S(iage)*            &                                                           
              dble(lastage-firstage-1))                                                                                             
          enddo  ! ages                                                                                                             
          F(iyear, lastage-firstage+1) = F(iyear, lastage-firstage)                                                                 
        enddo ! years                                                                                                               
                                                                                                                                    
                                                                                                                                    
        ! ----------------------------------------- Set up N on last age                                                            
                                                                                                                                    
        do year = lastyear-NySep, firstyear, -1                                                                                     
          iyear = year-firstyear+1                                                                                                  
          do age = lastage, lastage-1, -1                                                                                           
            iage = age-firstage+1                                                                                                   
            N(iyear,iage)=CN(iyear,iage)*(F(iyear,iage)+NM(iyear,iage))/&                                                           
            (F(iyear,iage)*(1d0-dexp(-(F(iyear,iage)+NM(iyear,iage)))))                                                             
          enddo                                                                                                                     
        enddo                                                                                                                       
                                                                                                                                    
        ! ------------------------------------------ Set up N on last year                                                          
                                                                                                                                    
        iyear = lastyear-firstyear+1                                                                                                
        do age = lastage, lastage-1, -1                                                                                             
          iage = age-firstage+1                                                                                                     
          N(iyear,iage)=CN(iyear,iage)*(F(iyear,iage)+NM(iyear,iage))/  &                                                           
          (F(iyear,iage)*(1d0-dexp(-(F(iyear,iage)+NM(iyear,iage)))))                                                               
        enddo                                                                                                                       
                                                                                                                                    
!----------------------------------- The cohort analysis and VPA                                                                    
                                                                                                                                    
        do year = lastyear-1, firstyear, -1                                                                                         
           iyear = year-firstyear+1                                                                                                 
           do age = lastage-2, firstage, -1                                                                                         
             iage = age-firstage+1                                                                                                  
             F(iyear, iage) = Cohort(N(iyear+1,iage+1),CN(iyear,iage),  &                                                           
                                NM(iyear, iage))                                                                                    
!             write(*,*) year, age, F(iyear,iage), N(iyear+1,iage+1)                                                                
             N(iyear, iage) = N(iyear+1, iage+1)* dexp(F(iyear,iage)+   &                                                           
                                NM(iyear, iage))                                                                                    
             F(iyear,iage)=Refine(N(iyear+1,iage+1),CN(iyear,iage),     &                                                           
               NM(iyear, iage),N(iyear,iage), F(iyear,iage),1d-9)                                                                   
           enddo ! ages                                                                                                             
        enddo ! years                                                                                                               
                                                                                                                                    
                                                                                                                                    
!       The plus-group                                                                                                              
                                                                                                                                    
        do year = firstyear, lastyear                                                                                               
          iyear = year-firstyear+1                                                                                                  
          F(iyear,lastage-firstage+1) = F(iyear,lastage-firstage)                                                                   
          iage = lastage-firstage+1                                                                                                 
          Z_here = F(iyear, iage)+NM(iyear,iage)                                                                                    
          N(iyear, iage) = CN(iyear, iage)*Z_here/F(iyear, iage)/       &                                                           
            (1d0 - dexp(-Z_here))                                                                                                   
        enddo                                                                                                                       
                                                                                                                                    
!   Forwards projection by one year                                                                                                 
                                                                                                                                    
        do age = firstage, lastage-2                                                                                                
          iage = age-firstage+1                                                                                                     
          iyear = lastyear-firstyear+1                                                                                              
          N(iyear+1, iage+1) = N(iyear,iage) *                          &                                                           
            dexp(-F(iyear,iage)-NM(iyear,iage))                                                                                     
          F(iyear+1, iage) = F(iyear, iage)                                                                                         
        enddo                                                                                                                       
                                                                                                                                    
        iage = lastage-firstage+1                                                                                                   
        iyear = lastyear-firstyear+1                                                                                                
        N(iyear+1,iage) = (N(iyear, iage) *                             &                                                           
         dexp(-(F(iyear, iage)+NM(iyear, iage)))) +                     &                                                           
         (N(iyear,iage-1)*dexp(-(F(iyear, iage-1)+NM(iyear, iage-1))))                                                              
                                                                                                                                    
      enddo ! its                                                                                                                   
                                                                                                                                    
! ---------------------------------------------------------------------                                                             
!                                                                                                                                   
!   Create the conventional shrunk VPA output file                                                                                  
!                                                                                                                                   
! ---------------------------------------------------------------------                                                             
                                                                                                                                    
      Open(17,file = filename, status = "unknown")                                                                                  
      write(17,*) ' '                                                                                                               
      write(17,*) HM(51,Language)                                                                                                   
      write(17,*)                                                                                                                   
      write(Text(1), '(I4)') shrinkyr                                                                                               
      Call Concat(Text(1), HM(52,Language),  Text(1))                                                                               
      Call Concat(Text(1), Text(1),  HM(53,Language))                                                                               
      write(17,'(A)') Text(1)                                                                                                       
      write(Text(1), '(F9.5)') MinCV                                                                                                
      Call Concat(Text(1), HM(54,Language),  Text(1))                                                                               
                                                                                                                                    
      write(17,'(A)') Text(1)                                                                                                       
      write(17,*)                                                                                                                   
      write(17,*) HM(55,Language)                                                                                                   
      write(17,*)                                                                                                                   
                                                                                                                                    
      write(17,120) HM(56,Language),HM(57,Language),HM(58,Language)                                                                 
                                                                                                                                    
120   format(' ',A16,4X,A17,3X,21X,A15)                                                                                             
      write(17,130) HM(59,language),HM(59,Language),HM(60,Language)                                                                 
                                                                                                                                    
130   format(' ',A18,2X,A18,2X,A19,2X)                                                                                              
      do iage = 1, lastage-firstage+1                                                                                               
        write(17,140) exp(Fest(iage)), VarFest(iage), exp(MF(iage)),    &                                                           
         VarMF(iage),                                                   &                                                           
        theta(iage),F(lastyear-firstyear+1,iage)                                                                                    
140   format(' ',2X,F5.3,5X,F5.3,3X,2X,F5.3,5X,F5.3,3X,7X,F5.3,         &                                                           
       7X,5X,F5.3)                                                                                                                  
      enddo ! ages                                                                                                                  
      write(17,*)                                                                                                                   
                                                                                                                                    
!  HERE WRITE F, N , AND STOCK SUMMARY                                                                                              
                                                                                                                                    
      CAll WriteMatrices(17, 'S', 132,8)                                                                                            
      write(17,*)                                                                                                                   
                                                                                                                                    
      Call WriteStockSummary(17)                                                                                                    
                                                                                                                                    
                                                                                                                                    
      Text(1)= ' '                                                                                                                  
      call ConCat(Text(2), HM(61,Language), ica_shr)
      Text(3)= ' '                                                                                                                  
   
      call Screen_out_a(Text,3,3)
                                                                                                                                    
      close(17)                                                                                                                     
                                                                                                                                    
9000  format(5X, 30(4X,I4))                                                                                                         
9010  format(I2, 3X,30(F8.0))                                                                                                       
9020  format(I2, 3X,30(F8.4))                                                                                                       
                                                                                                                                    
1077  Continue                                                                                                                      
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
