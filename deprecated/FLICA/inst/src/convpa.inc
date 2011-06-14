module convpa_module

use kind_module
use data_definition
use message_definition
use screen_io_module
use statistics_module
use output_module
use cvpa1_module

contains

! ///////////////////////////////////////////////////////////////////////////                            
                               
   Subroutine CONVPA(BRes, Ares, S, SepRES)        
                               
! ///////////////////////////////////////////////////////////////////////////                            
!                               
!                               
!   Conventional VPA initiated with Sep VPA, and fit the surveys                                 
!     and calculate the variance of the surveys about the
!     VPA populations.                  
!       Also calculate inverse-variance weights for initiating ICA2                             
!   Modified Dec 1998 sot that IV weights not put into Alambda, Blambda arrays                            
                               
                               
!   LOCAL VARIABLES                     
                               
   double precision BRes(maxbsurv), ARes(maxsurvey,maxage),     &                              
    S(maxage), PopnSize,                       &                              
    Sum, SSB                   
   integer year,age,iage,parmno,ndata,noBdata(maxbsurv),       &                              
    NoAdata(maxsurvey,maxage)               
   double precision IvarA(maxsurvey,maxage), IVarB(maxbsurv),    &                              
    SepRES                        
   integer dfageix(maxsurvey,maxage), dfssbix(maxbsurv)  
   double precision Q, IsepVar, SumIvar          
   logical UseRecr                     
   double precision QK, ssxx,ssxy,sigX,sigX2,sigY,sigXY  
   double precision observed, expected           
                               
!   ---------EXECUTABLE CODE OF CONVPA ----------------------------                                
                               
   parmno = Nxparm                     
                               
   Call CVPA(S) ! run a VPA initiated with sep populations & Fs                                 
                               
                               
!   Starting estimate of recruitment is a GM Mean over last NYSep years:                             
!   This is only a parameter if there is at least one index of recruitment                            
                               
   UseRecr = .false.                    
   do index = 1,nageix                   
    if((fage(index).eq.firstage).and.(lyear(index).eq.lastyear+1)) &                              
      UseRecr = .true.                 
   enddo ! indices                    
                               
   if (UseRecr) then                    
    Parmno = Parmno+1                   
    Xbest(Parmno) = 0d0                  
    do year = lastyear-NySep+1, lastyear         
     Xbest(Parmno) = Xbest(Parmno)+ dlog(N(year-firstyear+1,1))/  &                              
                        dble(NySEP)  
    enddo ! Years                    
    N(lastyear-firstyear+2,1) = dexp(Xbest(Parmno))    
   else   ! UseRecr                   
     N(lastyear-firstyear+2, 1) = 0d0          
    do year = lastyear-NySep+1, lastyear         
     N(lastyear-firstyear+2, 1)=N(lastyear-firstyear+2,1)+     &                              
       dlog(N(year-firstyear+1,1))/dble(NySep)     
    enddo                         
    N(lastyear-firstyear+2,1)=dexp(N(lastyear-firstyear+2,1))                                  
   endif ! UseRecr                     
                               
!-------------------------------- Calculate the Qs and residuals for each index                          
                               
!-------------------------------- First the SSB indices   
                               
   do index = 1, nssbix                  
                               
    ndata = 0                       
                               
    IF (QBParm(index) .eq. 0) then  !---------------- Absolute estimator                            
      Q = 1d0                      
      Bres(index) = 0d0                 
      do year = fbyear, lbyear             
       if (BSurvey(index, year-fbyear+1) .ne. -99d0) then
         SSB = CalcSSB(year)             
         Bres(index) = Bres(index) +              &                              
          (BSurvey(index,year-fbyear+1) -           &                              
          dlog(Q * SSB)) *                  &                              
          (BSurvey(index,year-fbyear+1) -           &                              
          dlog(Q * SSB))              
          ndata = ndata+1              
        endif ! not a missing value          
      enddo  ! Years                 
      NoBdata(index) = ndata              
    endif    ! Absolute Estimator           
                               
    IF (QBParm(index) .eq. 1) then  ! ----------------- Proportionate Model                          
                               
      ndata = 0                     
      SUM = 0d0                     
      Bres(index) = 0d0                 
      do year = fbyear, lbyear             
       if (BSurvey(index, year-fbyear+1) .ne. -99d0) then
         SSB = CalcSSB(year)             
         SUM = SUM + BSurvey(index,year-fbyear+1)        &                              
              - dlog(SSB)            
         ndata = ndata+1               
       endif ! not a missing value           
      enddo  ! Years                 
      Q = dexp( SUM/dble(ndata))            
      do year = fbyear, lbyear             
       if (BSurvey(index, year-fbyear+1) .ne. -99d0) then
         SSB = CalcSSB(year)             
         Bres(index) = Bres(index) +              &                              
          (BSurvey(index,year-fbyear+1) -           &                              
          dlog(Q * SSB)) *                  &                              
          (BSurvey(index,year-fbyear+1) -           &                              
          dlog(Q * SSB))              
       endif ! not a missing value           
      enddo  ! Years                 
                               
      parmno = parmno+1                 
      Xbest(parmno) = dlog(Q)              
      Xlow(parmno) = 0d0                
      Xhigh(parmno) = 0d0                
      NoBdata(index) = ndata              
                               
    endif     ! proportionate model         
                               
                               
    IF (QBParm(index) .eq. 2) then  !---------------------Power Model                             
                               
      Bres(index) = 0d0                 
      sigX = 0d0                    
      sigX2 = 0d0                    
      sigY = 0d0                    
      sigXY = 0d0                    
      ndata = 0                     
      do year = fbyear, lbyear             
       if (BSurvey(index, year-fbyear+1) .ne. -99d0) then
         SSB = CalcSSB(year)             
         sigX = sigX +dlog(SSB)            
         sigX2 = sigX2 + dlog(SSB)*dlog(SSB)     
         sigY = sigY+BSurvey(index,year-fbyear+1)   
         sigXY = sigXY + dlog(SSB)*BSurvey(index,        &                              
         year-fbyear+1)               
         ndata = ndata+1               
       endif ! not a missing value           
      enddo  ! Years                 
      ssxx = sigX2 - sigX*sigX/dble(ndata)       
      ssxy = sigXY - sigX*sigY/dble(ndata)       
      Q = ssxy/ssxx                   
      QK = sigY/dble(ndata) - Q*sigX/dble(ndata)    
      do year = fbyear, lbyear             
       if (BSurvey(index, year-fbyear+1) .ne. -99d0) then
         SSB = CalcSSB(year)             
         Observed = BSurvey(index,year-fbyear+1)   
         Expected = Q*dlog(SSB)+QK          
         Bres(index) = Bres(index) +              &                              
          (Observed-Expected)*(Observed-Expected)  
        endif ! not a missing value          
      enddo  ! Years                 
                               
      parmno = parmno+1                 
      Xbest(parmno) = Q                 
      Xlow(parmno) = 0d0                
      Xhigh(parmno) = 0d0                
      parmno = parmno+1                 
      Xbest(parmno) = QK                
      Xlow(parmno) = 0d0                
      Xhigh(parmno) = 0d0                
      NoBdata(index) = ndata              
                               
    endif     ! SSB POWER CATCHABILITY       
                               
   enddo     ! SSB Indices              
                               
! -------------------------------  Next the age-structured indices                                
                               
   do index = 1, nageix                  
                               
    if (QAParm(index) .eq. 0) then  ! --------------------------- AGED: ABSOLUTE ESTIMATOR                  
     Q = 1d0                       
     do age = fage(index), lage(index)          
      ARes(index,age-fage(index)+1) = 0d0        
      do year = fyear(index), lyear(index)       
       if (ASurvey(index, year-fyear(index)+1,age-fage(index)+1) &                              
         .ne. -99d0) then               
         if ((age .eq. lage(index)) .and. plusgp(index)) then  !------ PLUS GROUP                    
          PopnSize = 0d0              
          do iage = age, lastage          
            PopnSize = PopnSize + N(year-firstyear+1,    &                              
            iage-firstage+1)*dexp(-(             &                              
            F(year-firstyear+1,iage-firstage+1)+       &                              
            NM(year-firstyear+1,iage-firstage+1))*      &                              
            Timing(index))             
          enddo ! Accumulating for the plus-group 
         else   ! Not in the plus-group      
          PopnSize = (N(year-firstyear+1,           &                              
          age-firstage+1)*dexp(-(               &                              
          F(year-firstyear+1,age-firstage+1)+         &                              
          NM(year-firstyear+1,age-firstage+1))*        &                              
          Timing(index)))              
         endif  ! in plus-group          
                               
         ! Here the PopnSize are Ns from VPA corrected to the time of the index,                      
         ! and accumulated for the +gp at the last age of the index if necessary                      
                               
         ARes(index,age-fage(index)+1) =            &                              
         Ares(index,age-fage(index)+1) +            &                              
         (dlog(Q*PopnSize) -                  &                              
         (ASurvey(index,year-fyear(index)+1,age-fage      &                              
         (index)+1 )) )*                    &                              
         (dlog(Q*PopnSize) -                  &                              
         (ASurvey(index,year-fyear(index)+1,age-fage      &                              
         (index)+1 )) )               
       endif ! not a missing value           
      enddo  ! Years                 
     enddo   ! Ages                  
                               
    ENDIF    ! ABSOLUTE ESTIMATOR : AGED       
                               
    IF (QAParm(index) .eq. 1) then  !------------------------  AGED: PROPORTIONATE MODEL                  
                               
     do age = fage(index), lage(index)          
      ndata = 0                     
      sum = 0d0                     
      ARes(index,age-fage(index)+1) = 0d0        
      do year = fyear(index), lyear(index)       
       if (ASurvey(index, year-fyear(index)+1,age-fage(index)+1) &                              
         .ne. -99d0) then               
         ndata = ndata+1               
         NoAdata(index,age-fage(index)+1) =          &                              
         NoAdata(index,age-fage(index)+1) +1     
         if ((age .eq. lage(index)) .and. plusgp(index)) then  ! PLUS GROUP                       
          PopnSize = 0d0              
          do iage = age, lastage          
            PopnSize = PopnSize + N(year-firstyear+1,    &                              
            iage-firstage+1)*dexp(-(             &                              
            F(year-firstyear+1,iage-firstage+1)+       &                              
            NM(year-firstyear+1,iage-firstage+1))*      &                              
            Timing(index))             
          enddo ! Accumulating for the plus-group  
         else                    
          PopnSize = (N(year-firstyear+1,           &                              
          age-firstage+1)*dexp(-(               &                              
          F(year-firstyear+1,age-firstage+1)+         &                              
          NM(year-firstyear+1,age-firstage+1))*        &                              
          Timing(index)))              
         endif  ! Not in the plus-group       
                               
         ! Here the PopnSize are Ns from VPA corrected to the time of the index,                      
         ! and accumulated for the +gp at the last age of the index if necessary                      
                               
         SUM = SUM+ASurvey(index,year-fyear(index)+1,      &                              
           age-fage(index)+1) -dlog(PopnSize)   
       endif ! not a missing value           
      enddo  ! Years                 
                               
      Q = dexp(SUM/dble(Ndata))             
      do year = fyear(index), lyear(index)       
       if (ASurvey(index, year-fyear(index)+1,age-fage(index)+1) &                              
         .ne. -99d0) then               
         if ((age .eq. lage(index)) .and. plusgp(index)) then  ! PLUS GROUP                       
          PopnSize = 0d0              
          do iage = age, lastage          
            PopnSize = PopnSize + N(year-firstyear+1,    &                              
            iage-firstage+1)*dexp(-(             &                              
            F(year-firstyear+1,iage-firstage+1)+       &                              
            NM(year-firstyear+1,iage-firstage+1))*      &                              
            Timing(index))             
          enddo ! Accumulating for the plus-group  
         else                    
          PopnSize = (N(year-firstyear+1,           &                              
          age-firstage+1)*dexp(-(               &                              
          F(year-firstyear+1,age-firstage+1)+         &                              
          NM(year-firstyear+1,age-firstage+1))*        &                              
          Timing(index)))              
         endif  ! Not in the plus-group       
                               
         ! Here the PopnSize are Ns from VPA corrected to the time of the index,                      
         ! and accumulated for the +gp at the last age of the index if necessary                      
                               
         ARes(index,age-fage(index)+1) =            &                              
         Ares(index,age-fage(index)+1) +            &                              
         (dlog(Q*PopnSize) -                  &                              
         ASurvey(index,year-fyear(index)+1,age-fage       &                              
         (index)+1 ) )*                    &                              
         (dlog(Q*PopnSize) -                  &                              
         ASurvey(index,year-fyear(index)+1,age-fage       &                              
         (index)+1 ) )                
        endif ! not a missing value          
      enddo  ! Years                 
      parmno = parmno+1                 
      Xbest(parmno) = dlog(Q)              
      Xlow(parmno) = 0d0                
      Xhigh(parmno) = 0d0               
     enddo    ! ages                 
   EndIf  ! Linear model                 
                               
    IF (QAParm(index) .eq. 2) then  !----------------------  POWER MODEL: DO LOG-TRANSFORMED                
                     !             LINEAR REGRESSION OF INDEX ON VPA               
     do age = fage(index), lage(index)          
      sigX = 0d0                    
      sigX2 = 0d0                    
      sigY = 0d0                    
      sigXY =0d0                    
      ARes(index,age-fage(index)+1) = 0d0        
      ndata = 0                     
      do year = fyear(index), lyear(index)       
       if (ASurvey(index, year-fyear(index)+1,age-fage(index)+1) &                              
         .ne. -99d0) then               
         ndata = ndata+1               
         NoAdata(index, age-fage(index)+1) =          &                              
         NoAdata(index, age-fage(index)+1) +1    
         if ((age .eq. lage(index)) .and. plusgp(index)) then  ! PLUS GROUP                       
          PopnSize = 0d0              
          do iage = age, lastage          
            PopnSize = PopnSize + N(year-firstyear+1,    &                              
            iage-firstage+1)*dexp(-(             &                              
            F(year-firstyear+1,iage-firstage+1)+       &                              
            NM(year-firstyear+1,iage-firstage+1))*      &                              
            Timing(index))             
          enddo ! Accumulating for the plus-group  
         else                    
          PopnSize = (N(year-firstyear+1,           &                              
          age-firstage+1)*dexp(-(               &                              
          F(year-firstyear+1,age-firstage+1)+         &                              
          NM(year-firstyear+1,age-firstage+1))*        &                              
          Timing(index)))              
         endif  ! Not in the plus-group       
                               
         ! Here the PopnSize are Ns from VPA corrected to the time of the index,                      
         ! and accumulated for the +gp at the last age of the index if necessary                      
                               
         sigX = sigX + dlog(PopnSize)         
         sigX2 = sigX2 + dlog(PopnSize)*dlog(PopnSize)
         sigY =sigY+ASurvey(index,year-fyear(index)+1,      &                              
                     age-fage(index)+1) 
         sigXY = SigXY +dlog(PopnSize)*ASurvey(index,      &                              
              year-fyear(index)+1, age-fage(index)+1)                                 
        endif ! not a missing value          
      enddo  ! Years                 
                               
      ssxx = sigX2 - sigX*sigX/dble(ndata)       
      ssxy = sigXY - sigX*sigY/dble(ndata)       
      Q = ssxy/ssxx                   
      QK = sigY/ndata - Q*sigX/dble(ndata)       
                               
      do year = fyear(index), lyear(index)       
       if (ASurvey(index, year-fyear(index)+1,age-fage(index)+1) &                              
         .ne. -99d0) then               
         if ((age .eq. lage(index)) .and. plusgp(index)) then  ! PLUS GROUP                       
          PopnSize = 0d0              
          do iage = age, lastage          
            PopnSize = PopnSize + N(year-firstyear+1,    &                              
            iage-firstage+1)*dexp(-(             &                              
            F(year-firstyear+1,iage-firstage+1)+       &                              
            NM(year-firstyear+1,iage-firstage+1))*      &                              
            Timing(index))             
          enddo ! Accumulating for the plus-group  
         else                    
          PopnSize = (N(year-firstyear+1,           &                              
          age-firstage+1)*dexp(-(               &                              
          F(year-firstyear+1,age-firstage+1)+         &                              
          NM(year-firstyear+1,age-firstage+1))*        &                              
          Timing(index)))              
         endif  ! Not in the plus-group       
                               
         ! Here the PopnSize are Ns from VPA corrected to the time of the index,                      
         ! and accumulated for the +gp at the last age of the index if necessary                      
                               
         Observed=ASurvey(index,year-fyear(index)+1,      &                              
          age-fage(index)+1)             
         Expected = Q*dlog(PopnSize) + QK      
         ARes(index,age-fage(index)+1) =            &                              
          ARes(index,age-fage(index)+1) +           &                              
          (Observed-Expected)*(Observed-Expected)  
        endif ! not a missing value          
      enddo  ! Years                 
      parmno = parmno+1                 
      Xbest(parmno) = Q                 
      Xlow(parmno) = 0d0                
      Xhigh(parmno) = 0d0               
      parmno = parmno+1                 
      Xbest(parmno) = QK                
      Xlow(parmno) = 0d0                
      Xhigh(parmno) = 0d0                
     enddo    ! ages                  
                               
    endif     ! Power model              
                               
   enddo     ! Indices                
                               
                               
! ----------------THE INVERSE-VARIANCE WEIGHTS (Lambdas)   
                               
   if (NySep*(lastage-firstage) - NYSep+Nysep+lastage-firstage-   &                              
    1+lastage-firstage-2 .gt. 0) then           
    ISepVAR = dble(NYSep*(lastage-firstage) -(NYSep+NySep+     &                              
     (lastage-firstage-1)+                     &                              
     (lastage-firstage-2) ) )  /SepRES         ! Inverse of the variance about the separable model         
   else                          
    ISepVar = -1d0                    
    write(*,*) 'Separable model not fitted: insufficient d.f.'                                 
   endif                          
                               
   if (ISepVar .gt. -1d0) then               
    SumIVar = ISepVar                   
   else                          
    SumIVar = 0d0                     
   endif                          
                               
!   Calculate degrees of freedom : No of data - No of fitted params                                
                               
   do index = 1, nssbix                  
    dfssbix(index) = NoBdata(index) - QBParm(index)    
   enddo                          
                               
   do index = 1, nageix                  
    do iage = 1, lage(index)-fage(index)+1        
     dfageix(index,iage) = NoAdata(index,iage)- QAParm(index)                                 
    enddo                         
   enddo                          
                               
!   The inverse-variances                  
                               
   do index =1, nssbix                   
    IVarB(index) = dble(dfssbix(index))/Bres(index)    
    SumIVar = SumIVar + IvarB(index)           
   enddo                          
                               
   do index = 1, nageix                  
    do iage = 1, lage(index)-fage(index)+1         
    IvarA(index,iage)=dble(dfageix(index,iage))/Ares(index,iage)                                
    SumIVar = SumIvar + IvarA(index,iage)         
    enddo ! ages                      
   enddo ! indices                    
                               
   If (ISepVar .eq. -1d0) then ! Weights referenced to total index var if no sep constraint                   
    ISepVar = SumIVar                   
   endif                          
                               
!   do index =1, nssbix                   
!    Blambda(index) = IvarB(index)/(ISepVar) ! *dble(NoBdata(index)))                              
!   enddo                          
                               
!   do index = 1, nageix                  
!    do iage = 1, lage(index)-fage(index)+1        
!     Alambda(index,iage) = IvarA(index,iage)/(ISepVar *      &                              
!      dble(lage(index)-fage(index)+1))          
!     enddo ! ages                     
!   enddo                          
                               
   NxParm = Parmno                     
                               
   return                         
   end subroutine CONVPA

end module convpa_module
                               
