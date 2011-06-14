module ica_module

use kind_module
use data_definition
use message_definition
use screen_io_module
use statistics_module
use output_module
use srr_module
use convpa_module
use sepvpa_module
use cvpa1_module
use ica2_module

contains

!////////////////////////////////////////////////////////////////////////////                                                       
                                                                                                                                    
      Double Precision Function BRENT(AX, BX, CX, FN, TOL, XMIN)                                                                    
                                                                                                                                    
!///////////////////////////////////////////////////////////////////////////                                                        
                                                                                                                                    
!     This routine is from p. 284 of                                                                                                
!           Press, W.H., Flannery, B.P., Teukovsky, S.A. and                                                                        
!           W.T. Vetterling (1989)  NUMERICAL RECIPES: the art of scientific                                                        
!           computing (FORTRAN Version). Cambridge University Press, 702pp.                                                         
!                                                                                                                                   
!       where it is explained and documented.                                                                                       
!                                                                                                                                   
                                                                                                                                    
      double precision zeps, CGold                                                                                                  
      double precision A,B,V,W,AX,BX,CX,FN,TOL, XMIN,E,X,XM,Tol2                                                                    
      double precision TOL1, p,q,r, fx,fv,fw, etemp, d, u, fu                                                                       
      integer iter, itmax                                                                                                           
                                                                                                                                    
      itmax = 100                                                                                                                   
      zeps = 1.0d-6                                                                                                                 
      CGold = 0.3819660                                                                                                             
      A = DMin1 (AX, CX)                                                                                                            
      B = DMax1 (Ax, CX)                                                                                                            
      V = BX                                                                                                                        
      W = V                                                                                                                         
      x = v                                                                                                                         
      E = 0d0                                                                                                                       
      FX = FN(X)                                                                                                                    
      FV = FX                                                                                                                       
      FW = FX                                                                                                                       
      do iter = 1, itmax                                                                                                            
        XM = 0.5d0 *(A+B)                                                                                                           
        TOL1 = TOL * DABS(X) + ZEPS                                                                                                 
        TOL2 = 2d0 * TOL1                                                                                                           
        If (dabs(X-XM) .le. (TOL2 - 0.5d0*(B-A))) goto 3                                                                            
        if (dabs(e) .gt. tol1) then                                                                                                 
          r = (x-w) * ( fx-fv)                                                                                                      
          q = (x-v) * (fx-fw)                                                                                                       
          p = (x-v) * q-(x-w)*r                                                                                                     
          q = 2d0 * (q-r)                                                                                                           
          if (q .gt. 0) p = -p                                                                                                      
          q = dabs(q)                                                                                                               
          etemp = e                                                                                                                 
          e =d                                                                                                                      
          if (dabs(p).ge.dabs(0.5d0*q*etemp) .or. p .le. q*(a-x) .or.   &                                                           
            p .ge. q*(b-x)) goto 1                                                                                                  
          d = p/q                                                                                                                   
          u = x+d                                                                                                                   
          if (u-a .lt. tol2 .or. b-u .lt. tol2) d= dsign(tol1,XM-X)                                                                 
          goto 2                                                                                                                    
        endif                                                                                                                       
1       if (x .ge. xm) then                                                                                                         
          e = a-x                                                                                                                   
        else                                                                                                                        
          e = b -x                                                                                                                  
        endif                                                                                                                       
        d = cgold*e                                                                                                                 
2       if (dabs(d) .ge. tol1) then                                                                                                 
          u = x+d                                                                                                                   
        else                                                                                                                        
          u = x+dsign(tol1,d)                                                                                                       
        endif                                                                                                                       
        fu = fn(u)                                                                                                                  
        if (fu.le.fx) then                                                                                                          
          if (u .ge. x) then                                                                                                        
            a = x                                                                                                                   
          else                                                                                                                      
            b = x                                                                                                                   
          endif                                                                                                                     
          v=w                                                                                                                       
          fv=fw                                                                                                                     
          w=x                                                                                                                       
          fw=fx                                                                                                                     
          x=u                                                                                                                       
          fx=fu                                                                                                                     
        else                                                                                                                        
          if(u .lt.x) then                                                                                                          
            a = u                                                                                                                   
          else                                                                                                                      
            b = u                                                                                                                   
          endif                                                                                                                     
          if (fu .le. fw .or. w.eq.x) then                                                                                          
            v = w                                                                                                                   
            fv=fw                                                                                                                   
            w=u                                                                                                                     
            fw=fu                                                                                                                   
          else if (fu .le. fv .or. v.eq.x .or. v.eq.w) then                                                                         
            v=u                                                                                                                     
            fv=fu                                                                                                                   
          endif                                                                                                                     
       endif                                                                                                                        
       enddo                                                                                                                        
3     Xmin = x                                                                                                                      
      Brent = FX                                                                                                                    
      return                                                                                                                        
      end function BRENT            
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
                                                                                                                                    
                                                                                                                                    
      Double Precision Function SepRES(Fref)                                                                                        
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!   The purpose of this routine is to :                                                                                             
!            (1) Run the separable VPA with specified terminal  F                                                                   
!            (2) Calculate a total residual for all indices                                                                         
!            (3) return this value                                                                                                  
!                                                                                                                                   
!                                                                                                                                   
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ----------------------- LOCAL VARIABLES ------------------------------                                                            
                                                                                                                                    
      double precision BRes(maxbsurv),Ares(maxsurvey,maxage), Fref,     &                                                           
       SumRES                                                                                                                       
      integer i, iage                                                                                                               
! ----------------------------------------------------------------------                                                            
!                                                                                                                                   
!      Bres - SSQ of SSB index                                                                                                      
!      Ares - SSQ of age-structured index                                                                                           
!      Fref - Reference F for last year of separable VPA                                                                            
!      SumRes - total residuals for all indices                                                                                     
!                                                                                                                                   
!                                                                                                                                   
! ----------------------- EXECUTABLE CODE ------------------------------                                                            
                                                                                                                                    
      CAll SepVPA(Fref,BRes, ARes)                                                                                                  
                                                                                                                                    
      SumRes = 0d0                                                                                                                  
      do i = 1,nssbix                                                                                                               
        SumRes = SumRes + BRES(i)                                                                                                   
      enddo                                                                                                                         
                                                                                                                                    
      do i = 1,nageix                                                                                                               
        do iage = 1,lage(i)-fage(i)+1                                                                                               
          SumRes = SumRes + ARes(i,iage)/dble(lage(i)-fage(i)+1)                                                                    
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
      SepRES = SumRES                                                                                                               
                                                                                                                                    
      return                                                                                                                        
      end function SepRes
                                                                                                                                    
! ****************************************************************************                                                                                                                                   

Subroutine  AS1 (PLA, PCN, PNM, PSW, PCW, PMO, PPF, PPM, PAIndex, PBIndex, &
	PNssbix, PNageix, Pmaxage, Pmaxyear, Pmaxsurvey, Pmaxparm, Pmaxsep, &
	PmaxBsurvey, PQAParm, PQBParm, PHiFage, PLoFage, PRefage, PX, VCV, &
	PFF, PN, PS, Pfirstyear, Plastyear, Pfirstage, Plastage, Pfage, &
	Plage, Pfyear, Plyear, pfbyear, Plbyear,nextRec, PFitSRR, IXCorr, &
	PCNLambda, PSRRlambda, PAlambda, PBlambda, pnxparm, pnxdata, pTermS, &
	pTermS2, pChangeSel, pStepSel, pTwoSel, pNySep, RWOpt, maxweight, &
	pmissing,ptiming,pplusgp)

      implicit none                                                                                                                 

!     Variables passed as parameters

      integer pmaxage, pmaxyear, pmaxsurvey,pmaxbsurvey,pmaxparm,pmaxsep, &
      ploFage,pHiFage,pRefAge, pChangeSel

      integer pfirstage, plastage,  plage(pmaxsurvey), pfage(pmaxsurvey), &
      plyear(pmaxsurvey), pfyear(pmaxsurvey), pnxparm,pnxdata,pfirstyear,plastyear,   &
      pfbyear, plbyear, pnysep

      logical pfitsrr, pplusgp(pmaxsurvey), pstepsel, ptwosel

      double precision PCN(pmaxyear,pmaxage), PNM(pmaxyear,pmaxage), PSW(pmaxyear,pmaxage), &
       PCW(pmaxyear,pmaxage), PMO(pmaxyear,pmaxage), PPF, PPM, PAIndex(pmaxsurvey,pmaxyear,pmaxage), &
        PBIndex(pmaxbsurvey,pmaxyear), PLA(pmaxyear)

      double precision IXCOrr(pmaxsurvey), pCNlambda(pmaxyear,pmaxage), pAlambda(pmaxsurvey,pmaxage), &
      pBlambda(pmaxsurvey), pSRRlambda, Ptiming(pmaxsurvey), pmissing, maxweight

      double precision pterms, pterms2
      integer  pnssbix,pnageix,pqbparm(pmaxbsurvey),pqaparm(pmaxsurvey)

      double precision PFF(pmaxyear,pmaxage), PN(pmaxyear,pmaxage), PX(pmaxparm), &
      PS(pmaxage), nextRec

	integer :: RWOpt, i, j


!  Local Variables


      character*1 dummy
      integer iyear, iage, index
!FLR      integer tscan, iyear, iage, index
!FLR      external tscan
      character*77 text(1)


      double precision X(maxparm), VCV(Maxparm,maxparm)

! ....... Copy from variables passed in subroutine to variables in common block ....

! ....  fist check array sizes big enough

      if (maxage < pmaxage) then
         if (DLLFlag .eq. .false.) write(*,*) 'Too many ages in array size passed to AS1'
         stop
      else if (maxyear < pmaxyear) then
         if (DLLFlag .eq. .false.) write(*,*) 'Too many year in array size passed to AS1'
         stop
      else if (maxsurvey < pmaxsurvey) then
         if (DLLFlag .eq. .false.)  write(*,*) 'Too many aged surveys in array size passed to AS1'
         stop
      else if (maxbsurv < pmaxbsurvey) then
         if (DLLFlag .eq. .false.) write(*,*) 'Too many ssb surveys in array size passed to AS1'
         stop
      else if (maxparm < pmaxparm) then
         if (DLLFlag .eq. .false.)  write(*,*) 'Too many ssb surveys in array size passed to AS1'
         stop
      else if (maxsep < pmaxsep) then
         if (DLLFlag .eq. .false.) write(*,*) 'Too many years of selection pattern in array size passed to AS1'
         stop
      endif

! start and ends of the data
     firstyear = pfirstyear
     lastyear  = plastyear
     firstage  = pfirstage
     lastage   = plastage
     fbyear    = pfbyear
     lbyear    = plbyear
     nageix    = pnageix
     nssbix    = pnssbix

     do index = 1, nageix
       fage(index) = pfage(index)
       lage(index) = plage(index)
     enddo

     NySep     = pNySep
     Nxparm    = PnxParm
     Nxdata    = Pnxdata
     TermS     = PtermS
     TermS2    = PTermS2
     FitSRR    = PFitSRR
     StepSel   = PStepSel
     ChangeSel = PChangeSel
     TwoSel    = ptwosel
     HiFage    = PhiFage
     LoFage    = PLoFage
     Refage    = PrefAge
     Missing   = pmissing

	 if (DLLFlag .eq. .false.) print *, "AS1: Missing = ", Missing
     
! 'FAD' type stuff

      PF = PPF
      PM = PPM
      do iyear= 1, lastyear-firstyear+2
        LA(iyear) = PLA(iyear)
        do iage= 1, lastage-firstage+1
          CN(iyear,iage) = PCN(iyear,iage)
          NM(iyear,iage) = PNM(iyear,iage)
          MO(iyear,iage) = PMO(iyear,iage)
          SW(iyear,iage) = PSW(iyear,iage)
          CW(iyear,iage) = PCW(iyear,iage)
          W(iyear,iage)  = PCNLambda(iyear,iage)
        enddo
      enddo

! Surveys and Survey weights

      do index = 1,Nageix
        QAParm(index) = PQAParm(index)
        Plusgp(index) = pplusgp(index)
        Timing(index) = ptiming(index)
        fyear(index) = pfyear(index)
        lyear(index) = plyear(index)
        do iage=1,lage(index)-fage(index)+1
          Alambda(index,iage)=PAlambda(index,iage)
          do iyear=1,lyear(index)-fyear(index)+1
            ASurvey(index,iyear,iage)=Paindex(index,iyear,iage)
if (ASurvey(index,iyear,iage) == missing) then
	if (DLLFlag .eq. .false.) print *, "Survey:", index, iyear, iage, ASurvey(index,iyear,iage)
end if
          enddo
        enddo
      enddo


      do index = 1,Nssbix
        QBParm(index) = PQBParm(index)
        Blambda(index)=PBlambda(index)
        do iyear=1,lbyear-fbyear+1
          BSurvey(index,iyear)=PBindex(index,iyear)
        enddo
      enddo

      SRRlambda = PSRRlambda

      language=1
      RecalculatePopulations=.true.

      call ICA1
      call ICA2(X,VCV,IxCorr,pmaxparm,RWOpt,maxweight)

!     Decide whether to return a recruitment forecast

      NextRec = Missing
      do index = 1,nageix
        if (lyear(index) .ge. lastyear+1) then
          if (fage(index) .eq. firstage) then
            if (ASurvey(index,lastyear+1-fyear(index)+1,1) .ne. Missing) then
              NextRec = N(lastyear+1-firstyear+1,1)
            endif
          endif
        endif
      enddo

      return
      end subroutine AS1


!      dummy=KO(1,Language)
!      call Screen_in_a(HL(35,Language), dummy, KO(1,Language), Language)
!
!
!
!      if (Tscan(dummy, KY(1,Language)) .ne.0 ) then
!         dummy='c'
!         call Screen_in_a(HL(42,Language), dummy, 'bBcC', Language)
!         if (Tscan(dummy, 'cC') .ne. 0) then
!           call PBootstrap(X,VCV)
!         else
!           call MCMC
!         endif
!      endif
!
!      Text(1) =HL(43,Language)
!
!      call Screen_out_a(Text, 1, 1)
!      end                                                                                                                           
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
                                                                                                                                    
! **********************************************************************                                                                                                                                    
                                                                                                                                    
      Subroutine ICA1                                                                                                               
                                                                                                                                    
      implicit none

!     Local variables                                                                                                               
    
      character*75 text(24)                                                                                                         
      character*5 ytext                                                                                                             
      character*9 ntext                                                                                                             
      double precision temp,  SSQLow, Flow, Flower,Fupper,Fstart,       &                                                           
      toler, SSQ                                                                                                                    
      integer j, i, itemp, age, iage                                                                                                
      double precision RES(maxsurvey+maxbsurv),BRes(maxbsurv),          &                                                           
       ARes(maxsurvey,maxage), SumRES, low, high, max,min   
      double precision Resids(maxdata)                                                                                              
      integer Noparm, Lastsel1, lastsel2, lastparm, Index
      logical UseRec ! Whether there is an index of forthcoming recruitment                                                         
                                                                                                                                    
!                                                                                                                                   
!        Temp - the working value of reference F in last year                                                                       
!        toler - tolerance when solving for reference F                                                                             
!        BRes - SSQ for SSB index                                                                                                   
!        ARes - SSQ for survey index                                                                                                
!        SumRes - total SSQ                                                                                                         
!        itemp - number of parameters to be estimated                                                                               
!        SSQLow - lowest total SSQ found                                                                                            
!        Flow - reference F at which SumRes = SSQLow                                                                                
!        Flower, Fupper - range for terminal F                                                                                      
!                                                                                                                                   
!                                                                                                                                   
!     ___________________EXECUTABLE CODE______________________________                                                              




      Call CalcSOP              ! Sum-of-Products calculation
                                                                                                                                    
      if ( (Nageix+Nssbix) .eq. 0)  then                                                                                            
        Text(1)= HL(20,language)                                                                                                    
        Text(2)= HL(21,language)                                                                                                    
        Call Screen_out_a(Text,24,2)                                                                                                
        stop                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      itemp = NySep + Nysep+ 2*(lastage-firstage) - 1 - 2                                                                           
                                                                                                                                    
      do index = 1, nssbix                                                                                                          
        itemp = itemp + QBparm(index)                                                                                               
      enddo                                                                                                                         
                                                                                                                                    
      do index = 1, nageix                                                                                                          
        do age = fage(index),lage(index)                                                                                            
          itemp = itemp + QAparm(index)                                                                                             
        enddo                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
      If (TwoSel) itemp = itemp+(lastage-firstage-3)                                                                                
                                                                                                                                    
                                                                                                                                    
      if (itemp .gt. maxparm) then                                                                                                  
        Text(1)= HL(1,Language)                                                                                                     
        Call IntToChar(maxparm, ytext, 5)                                                                                           
        call Concat(Text(2), HL(2,Language),Ytext )                                                                                 
        Call concat(Text(2), Text(2), HL(3,Language))                                                                               
        call IntToChar(itemp, ytext, 5)                                                                                             
        call Concat(Text(3), HL(4,Language),Ytext )                                                                                 
        call Concat(Text(3), Text(3),HL(5,Language))                                                                                
        call Screen_out_a(Text,24,3)                                                                                                
        stop                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      low = 0.00d0  
      high = 0d0
      max=3d0
      min=0.02d0

      Low = 0.05d0
      High =1.8d0

!      do while ((low .ge. high)  .or. (low .lt. 0.02d0))
!        Call Screen_in_r(HL(6,Language),low,max,min,language)                                                                    
!                                                                                                                                    
!        Call Screen_in_r(HL(7,Language),high,max,min,language)                                                                   
!                                                                                                                                    
!        if (low .ge. high) then                                                                                                     
!          Text(1)= HL(10,Language)                                                                                                  
!          Call Screen_out_A(Text, 24, 1)                                                                                            
!          low = 0d0                                                                                                                 
!          high = 0d0                                                                                                                
!        endif                                                                                                                       
!      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      Text(1) = HL(11,Language)                                                                                                     
      Text(2) = ' '                                                                                                                 
      Text(3) =  '    F                  SSQ  '                                                                                     
      Text(4) =  '+--------+-------------------'                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      SSQLow = 1d10                                                                                                                 
      Flow = 0d0                                                                                                                    
                                                                                                                                    
!      open (11, file = ICAdiag_out, status = 'Unknown')                                                                             
!      write(11,*) 'ICA v1_3 TUNING DIAGNOSTICS FILE'                                                                                
!      write(11,'(I4,4X,I4)'  ) nssbix, nageix                                                                                       
!      write(11,*) 'F         SSQ'                                                                                                   
                                                                                                                                    
                                                                                                                                    
      do j= 1, 20  !     START THE 20 Separable VPAs                                                                                
                                                                                                                                    
        temp = low + dble(j-1)* (high-low)/19d0                                                                                     


        CAll SepVPA(Temp, BRes, ARes)                                                                                               

        SumRes = 0d0                                                                                                                
                                                                                                                                    
        do i = 1, nssbix                                                                                                            
          SumRes = SumRes + BRes(i)                                                                                                 
          Res(i) = BRes(i)                                                                                                          
        enddo                                                                                                                       
                                                                                                                                    
!        do i = nssbix+1, maxbsurv                                                                                                  
!           Res(i) = -1.0d0                                                                                                         
!        enddo                                                                                                                      


        do i = 1, nageix                                                                                                            
          Res(i+nssbix) = 0d0                                                                                                       
          do iage = 1, lage(i)-fage(i)+1                                                                                            
            SumRes = SumRes + ARes(i,iage)/dble(lage(i)-fage(i)+1)                                                                  
            Res(i+nssbix) = Res(i+nssbix) +                             &                                                           
             ARes(i,iage)/dble(lage(i)-fage(i)+1)                                                                                   
          enddo                                                                                                                     
        enddo                                                                                                                       
!        do i = nageix+1, maxsurvey                                                                                                 
!           RES(i+maxbsurv) = -1.0d0                                                                                                
!        enddo                                                                                                                      
                                                                                                                                    
        if (SumRes .lt. SSQLow) then                                                                                                
          SSQLow = SumRES                                                                                                           
          Flow = temp                                                                                                               
        endif                                                                                                                       

                                                                                                                                    
!        write(11,9040) temp, SumRES,                                    &                                                           
!         (Res(i), i=1,nssbix),(Res(index+nssbix),index=1,nageix)                                                                    


!         write(Text(4+j),9030) temp,SumRES                                                                                          

                                                                                                                                    
                                                                                                                                    
      enddo   ! 20 separable VPAs now run; Fs and SSQs written to ICADIAG.OUT                                                       



!      close(11)                                                                                                                     
                                                                                                                                    
!      Call Screen_out_a(text,24,24)                                                                                                 
                                                                                                                                    
!     Now start one-dimensional search in F dimension for lowest SSQ,                                                               
!     starting from FLow and using the BRENT method                                                                                 
                                                                                                                                    
                                                                                                                                    
      Fupper = (Flow + (high-low)/19d0)                                                                                             
      Flower = (Flow -(high-low)/19d0)                                                                                              
      toler = 0.01d0                                                                                                                
      Fstart = 0d0                                                                                                                  
                                                                                                                                    
!     Solve Flow for lowest SSQ between Flower and Fupper with specified tolerance                                                  
                                                                                                                                    
      SSQLow = Brent(Flower,Flow,Fupper,SepRES,toler,Fstart)                                                                        


!     solution should have been found here, assuming 1 selection pattern                                                            
                                                                                                                                    
!      write(ntext,100) Fstart                                                                                                       
100   format(' ',F8.3)                                                                                                              
      Call ConCat(Text(1),HL(12,Language),ntext)                                                                                    
!      Call Screen_out_a(text,24,1)                                                                                                  
                                                                                                                                    
                                                                                                                                    
!     Adjust the parameter list for two selection patterns if                                                                       
!     necessary                                                                                                                     
                                                                                                                                    
                                                                                                                                    
      If (TwoSel) then                                                                                                              
        LastParm = NxParm                                                                                                           
        NoParm=NySep+2*(lastage-firstage+1-3)+ &   ! F and S params                                                                 
         (NySep + (lastage-firstage+1-1) -1)      ! fitted populations                                                              
                                                                                                                                    
        UseRec = .false.                                                                                                            
        do i = 1,NAgeix                                                                                                             
          If(ASurvey(i,lastyear-fyear(i)+2,1).gt.0d0) UseRec=.True.
        enddo                                                                                                                       
                                                                                                                                    
        If (UseRec) NoParm = NoParm+1            ! forthcoming recruitment                                                          
                                                                                                                                    
        NxParm = 0                                                                                                                  
                                                                                                                                    
        LastSel1 = (NySep + (lastage-firstage+1) - 3) ! last selection in 1st vector                                                
        do i = 1, LastSel1                                                                                                          
          Xlow(i) =Xbest(i)                                                                                                         
          Nxparm = Nxparm+1                                                                                                         
        enddo                                                                                                                       
                                                                                                                                    
        LastSel2 = LastSel1+1+(lastage-firstage+1-3)                                                                                
                                                                                                                                    
        do i = LastSel1+1, LastSel2                                                                                                 
           Xlow(i) = Xbest(i-(lastage-firstage-2))                                                                                  
           NxParm = Nxparm+1                                                                                                        
        enddo                                                                                                                       
                                                                                                                                    
        do i =  LastSel2+1, NoParm                                                                                                  
           Nxparm=Nxparm+1                                                                                                          
           Xlow(Nxparm) = Xbest(i-(lastage-firstage-2))                                                                             
        enddo                                                                                                                       
                                                                                                                                    
                                                                                                                                    
        do i= (NoParm - (lastage-firstage+1-3)), LastParm-1                                                                         
          NxParm = NxParm+1                                                                                                         
          Xlow(NxParm) = Xbest(i+1)                                                                                                 
        enddo                                                                                                                       
                                                                                                                                    
        do i = 1,NxParm                                                                                                             
          Xbest(i) = Xlow(i)                                                                                                        
          Xlow(i) = Xbest(i)-0.2d0                                                                                                  
          Xhigh(i) = Xbest(i)+0.2d0                                                                                                 
        enddo                                                                                                                       
      endif                                                                                                                         
                                                                                                                                    
! find starting estimate for SRR fit; adjust parameter list                                                                         
                                                                                                                                    
      If (FitSRR) then                                                                                                              
        Call ApproxSRR                                                                                                              
        Nxdata = Nxdata+ (lastyear-firstyear+1)-lag                                                                                 
        NxParm = NxParm+2                                                                                                           
        if ((a .le. 0.0) .or. (b .le. 0.0)) then                                                                                    
          !Text(1)= HL(14,Language)                                                                                                   
          !Call Screen_out_a(Text(1),24,1)                                                                                            
          !stop
          FitSRR = .false.
        else                                                                                                                      
           Xbest(Nxparm-1) = dlog(a)                                                                                                   
           Xbest(Nxparm) = dlog(b)                                                                                                     
           Xlow(Nxparm-1) = -1                                                                                                         
           Xhigh(Nxparm-1) = -1                                                                                                        
           Xlow(Nxparm) = -1                                                                                                           
           XHigh(NxParm) = -1                                                                                                          
        endif                                                                                                                       
      endif                                                                                                                         
                                                                                                                                    
      if (nxdata .gt. maxdata) then                                                                                                 
        Text(1)=HL(15,Language)                                                                                                     
        call IntToChar(maxdata, ytext, 5)                                                                                           
        Call ConCat(Text(2), HL(16,Language), ytext)                                                                                
        CAll Concat(Text(2), Text(2), HL(17,Language))                                                                              
        call IntToChar(nxdata, ytext, 5)                                                                                            
        Call Concat(Text(2), Text(2),ytext)                                                                                         
        CAll Concat(Text(2), Text(2), HL(18,Language))                                                                              
        call Screen_out_a(text, 24, 2)                                                                                              
        stop                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
!      Show the model structure                                                                                                     
                                                                                                                                    
      Call DocModel(-1) ! to screen                                                                                                 
                                                                                                                                    
!   Output params so far                                                                                                            
                                                                                                                                    
                                                                                                                                    
!   Run the objective function to test                                                                                              
                                                                                                                                    
      CALL LSFUN1(Nxdata,NxParm, Xbest, Resids)                                                                                     
      SSQ=0d0
      do i=1,Nxdata                                                                                                                 
        SSQ= SSQ+Resids(i)*Resids(i)                                                                                                
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
!     Calculate variances, chi-squareds etc.                                                                                        

!      CALL CalcStats(.false.)   


!     Initiating values for the survey index weights (lambdas)
                                                                                                                                    
!      do i = 1,Nssbix                                                                                                               
!        Blambda(i) = CVar/BVar(i)                                                                                                   
!      enddo                                                                                                                         
                                                                                                                                    
!      do i = 1,Nageix                                                                                                               
!       do age = fage(i),lage(i)                                                                                                     
!        iage = age-fage(i)+1                                                                                                        
!        Alambda(i,iage) = CVar/AVar(i,iage)                                                                                         
!       enddo                                                                                                                        
!      enddo                                                                                                                         
                                                                                                                                    


                                                                                                                                    
!9000  format (A40)                                                                                                                 
!9010  format (A1)                                                                                                                  
9030  format (F8.2,2X,F20.10)                                                                                                       
9040  format (F6.2,1X,40(G16.10,1X))                                                                                                
!9050  format (F10.5)                                                                                                               
                                                                                                                                    
      return                                                                                                                        
      end subroutine ICA1
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////////

      Subroutine PBootstrap(X,VCV)

! ///////////////////////////////////////////////////////////////////////////
!
!   Estimate uncertainty by parametric bootstrap.
!
!

      implicit none                                                                                                                 
                                                                                                                                    



      double precision eps, Z(maxparm), VCV(maxparm,maxparm),X(maxparm)
      double precision R ( (maxparm+1)*(maxparm+2)/2  )
      double precision G05DDF
      integer NR,IFail,Its,iyear,iage


      double precision MBAL, MbalMin,MbalMax
      character*1 dummy
      character*80 text(2)
      integer itsToRun,Imin,i, ic, age
      integer N_percentiles, nmax,nmin

      parameter(nmax=10)
      parameter(Imin=100)
      parameter (MbalMin=0d0,MbalMax=1d12)

      double precision percentile(nmax), FC(maxdata)


      nmin=3
      NR=(maxparm+1)*(maxparm+2)/2
      MBal =0d0



!        Call Screen_in_i(HL(36,Language),n_percentiles,nmax,nmin,Language)

!      call GetPercentiles(N_percentiles, percentile, nmax)

      call Screen_in_i(HL(38,Language),ItsToRun,maxiters,Imin,Language)

      call Screen_in_r(HW(28,Language),MBal,MbalMax,MbalMin,Language)

!
!     Start the parametric bootstrap process
!
!

!
!     Open the files
!     and write the headers for the files
!

       call OPenItnFiles(ItsToRun, MBAL,27)

!
!      Set up the vector for sampling
!
      Ifail = 1
      EPS= 0.09d0/dble(NXparm)
      ic =maxparm

      call G05EAF(X, NxParm, VCV, IC, EPS, R, NR, IFail)

      if (Ifail .ne. 0) then
        write(Text(1), '(I2)') IFAIL
        Call ConCat(Text(1), HW(19,Language), Text(1))
        Call Screen_out_a(Text(1),10,1)

      endif



      full = .true.

      do ITS = 1, ItsToRun
!        write(*,*) Its
        Call G05EZF(Z,NxParm, R, NR, Ifail)

        if (Ifail .ne. 0) then
          write(Text(1), '(I2)') IFAIL
          Call ConCat(Text(1), HW(20,Language), Text(1))
          Call Screen_out_a(Text(1),10,1)
        endif


        call LSFUN1(NxData, NxParm, Z, FC)

!        call Tableout(1) ! to test


      call WriteIterations(Z, 27)


      enddo  ! iterations


!
!     close the *.mci files
!
      Call CloseItnFiles(27)


!
!     calculate the percentiles and create *.pbi files
!
!      call CreatePBYFiles(N_percentiles, Percentile,nmax)





      return
      end subroutine PBootstrap





! ///////////////////////////////////////////////////////////////////////

      Subroutine WriteIterations(Z, fch)

! /////////////////////////////////////////////////////////////////////////
      implicit none
      integer iyear, fch, i, age
      double precision Stock(maxyear), MeanF(maxyear), Z(maxparm)

        write(fch,200) (Z(i),i=1,NxParm)

!
!     write out the stock
!
        do iyear=1, lastyear-firstyear+1
          Stock(iyear) = CalcSSB(iyear+firstyear-1)
        enddo
        write(1,100) (stock(iyear), iyear=1,lastyear-firstyear+1)

!
!     write out the recruitments
!
        write(2,100) (N(iyear,1), iyear=1,lastyear-firstyear+1)


!
!    write out the mean fishing mortality
!

        do iyear=1, lastyear-firstyear+1
          MeanF(iyear)=0d0
          do age=LoFage, HiFage
            MeanF(iyear) = MeanF(iyear)+F(iyear,age-firstage+1)
          enddo
          MeanF(iyear)=MeanF(iyear)/dble(HiFage-LoFage+1)
        enddo
        write(3,100) (MeanF(iyear), iyear=1,lastyear-firstyear+1)

!
!     write out the catches
!
      write(4,100) (LA(iyear), iyear=1,lastyear-firstyear+1)

100   format (4X, 70(E14.6,1X))
200   format (4X, 30(E14.6,1X))
      return
      end subroutine WriteIterations

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


! ////////////////////////////////////////////////////////////////////////

       Subroutine OPenItnFiles(ItsToRun, MBAL,mcfile)

! ////////////////////////////////////////////////////////////////////////
       implicit none
       
       integer ItsToRun, mcfile
       double precision MBAL

       Open(1, file=stock_mci, status='unknown')
       write(1,*) ItsToRun, 0, lastyear-firstyear+1,MBAL, firstyear
       Open(2, file=recruits_mci, status='unknown')
       write(2,*) ItsToRun, 0, lastyear-firstyear+1,MBAL, firstyear
       Open(3, file=meanf_mci, status='unknown')
       write(3,*) ItsToRun, 0, lastyear-firstyear+1,MBAL, firstyear
       Open(4, file=yield_mci, status='unknown')
       write(4,*) ItsToRun, 0, lastyear-firstyear+1,MBAL, firstyear

       open (mcfile, file=ica_mc,status='unknown')

       return
       end subroutine OpenItnFiles

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


! ////////////////////////////////////////////////////////////////////////

       Subroutine CloseItnFiles(mcfile)

! ////////////////////////////////////////////////////////////////////////
      implicit none
      integer i, mcfile

      do i= 1,4
        close(i)
      enddo

      close(mcfile)
      return
      end subroutine CloseItnFiles

end module ica_module
