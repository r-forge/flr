!     /////////////////////////////////////////////////////////////////                                                            
                                                                                                                                   
      Subroutine LSFUN1(M,Nparm, AP, FC)                                                                                           
                                                                                                                                   
!     //////////////////////////////////////////////////////////////////                                                           
                                                                                                                                   
                                                                                                                                   
!     GENERAL PURPOSE OBJECTIVE FUNCTION                                                                                           
                                                                                                                                   
      Include "INDAT.INC"                                                                                                          
      Include "SEPMODEL.INC"                                                                                                       
      Include "PREDIC.INC"                                                                                                         
      Include "SRR.INC"                                                                                                            
                                                                                                                                   
!     local variables                                                                                                              
                                                                                                                                   
      integer m,nparm                                                                                                              
      integer ndata                                                                                                                
      double precision fs(maxyear)              ! F by year in separable model                                                     
      double precision s(maxage), S2(maxage)    ! S at age in separable model                                                      
      double precision Stock(maxyear), recruit(maxyear)                                                                            
      integer parmno, index,                                            &                                                          
       iage, iyear                                                                                                                 
      integer i, age, year       ! Local looping variables                                                                         
!      double precision NP(maxyear,maxage),FM(maxyear,maxage) ! Predicted catches and poplns from separable model                  
      double precision FC(maxdata)             ! Residual for each observation                                                     
      double precision AP(maxparm)              ! Parameters                                                                       
      double precision Q, SSB, Observed,                                &                                                          
                     expected             ! internal vars                                                                          
      integer syear, nosrrdata                                                                                                     
      double precision Z_here       ! internal vars                                                                                
      double precision CalcSSB  ! Functions                                                                                        
      logical UseRecr                                                                                                              
      double precision QK                                                                                                          
                                                                                                                                   
!     __________________EXECUTABLE CODE OF LSFUN1_______________________                                                           
                                                                                                                                   
                                                                                                                                   
!      write(*,*) 'CALLING LSFUN1 ..'                                                                                              
                                                                                                                                   
                                                                                                                                   
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
                  F(iyear,iage)= FS(syear)*                               &                                                        
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
               FC(ndata)= W(iyear,iage)*                                  &                                                        
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
                           F(iyear,iage)/                               &                                                          
                           (1d0 - dexp(-Z_here))                                                                                   
              else if (year .ne. firstyear) then                                                                                   
                N(iyear,iage) = (N(iyear-1,iage)*                       &                                                          
                dexp(-F(iyear-1,iage)-NM(iyear-1,iage)))                &                                                          
                + (N(iyear-1,iage-1)*                                   &                                                          
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
        N(iyear+1, iage) = (N(iyear,iage) *                               &                                                        
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
             FC(ndata)= W(iyear,iage)*                                  &                                                        
             dlog(CN(iyear,iage)/PredCN(iyear,iage))                                                                             
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
          if (Bindex(index, year-fbyear+1) .ne. -99d0) then                                                                        
            ndata = ndata+1                                                                                                        
            SSB = CalcSSB(year)                                                                                                    
            Expected = Q*dlog(SSB)+ QK                                                                                             
            Observed = Bindex(index, year-fbyear+1)                                                                                
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
            if (Aindex(index, year-fyear(index)+1,age-fage(index)+1)    &                                                          
              .ne. -99d0) then                                                                                                     
              ndata = ndata+1                                                                                                      
              if ((age .eq. lage(index)) .and. plusgp(index)) then                                                                 
                    Expected = 0d0                                                                                                 
                do iage = age, lastage                                                                                             
                  Expected = Expected + N(year-firstyear+1,             &                                                          
                   iage-firstage+1)*dexp(-(                             &                                                          
                   F(year-firstyear+1,iage-firstage+1)+                 &                                                          
                   NM(year-firstyear+1,iage-firstage+1))*               &                                                          
                   Timing(index))                                                                                                  
                enddo ! Accumulating for the plus-group                                                                            
                Expected = Q*dlog(Expected)+QK                                                                                     
              else                                                                                                                 
                if (N(year-firstyear+1,age-firstage+1).lt. 1d-20)       &                                                          
                      then                                                                                                         
                  write(*,*) 'Index but no population at ',age,         &                                                          
                         ' in year ',year                                                                                          
                  write(*,*) ' attempting to fit aged index',           &                                                          
                         index                                                                                                     
                  stop                                                                                                             
                endif ! an error                                                                                                   
                Expected = Q*dlog(N(year-firstyear+1,                   &                                                          
                 age-firstage+1)*dexp(-(                                &                                                          
                 F(year-firstyear+1,age-firstage+1)+                    &                                                          
                 NM(year-firstyear+1,age-firstage+1))*                  &                                                          
                  Timing(index)))+QK                                                                                               
              endif   ! Not in the plus-group                                                                                      
                Observed=dble(Aindex(index, year-fyear(index)+1,        &                                                          
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
            write(*,*) 'LSFUN 1 : No of parameters is wrong ',parmno,   &                                                          
             Nparm                                                                                                                 
            pause                                                                                                                  
      endif                                                                                                                        
                                                                                                                                   
                                                                                                                                   
      if (Ndata .ne. M) then                                                                                                       
            write(*,*) 'LSFUN 1 : No of data is wrong ',Ndata,M                                                                    
            pause                                                                                                                  
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
      end  ! of subroutine LSFUN1 (objective function)                                                                             
                                                                                                                                   
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
                                                                                                                                   
                                                                                                                                   
