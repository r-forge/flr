                                                                                                                                    
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                              
!                                                                                                                                   
!            UNIT STATS : Routines for calculating variances, c.v.s etc.                                                            
!                                                                                                                                   
!                                                                                                                                   
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                               
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
! ////////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                   
      Subroutine  CalcStats(weighted)                                                                                               
!                                                                                                                                   
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
      implicit NONE                                                                                                                 
                                                                                                                                    
!     Calculates parameters of the distributions of the residuals                                                                   
!         from the most recent model fit. Running the objective function                                                            
!         only returns the weighted residuals in memory, but the                                                                    
!         observed and expected values for each datum will have been                                                                
!         saved to disk if the objective function has been run with                                                                 
!         writeout = .true. This programme reads that disk file.                                                                    
!                                                                                                                                   
!     The calculation of the probability associated with the chi-square values                                                      
!     is explained on p. 160 et seq. of 'Numerical Recipes'                                                                         
!                                                                                                                                   
!                                                                                                                                   
!     CALLS:       GAMMQ                                                                                                            
!                                                                                                                                   
!                                                                                                                                   
                                                                                                                                    
      Include "INDAT.INC"                                                                                                           
      Include "SEPMODEL.INC"                                                                                                        
      Include "STATS.INC"                                                                                                           
      Include "PREDIC.INC"                                                                                                          
      Include "SRR.INC"                                                                                                             
      logical weighted                                                                                                              
                                                                                                                                    
                                                                                                                                    
! ----------------------LOCAL VARIABLES--------------------------------                                                             
                                                                                                                                    
      integer  id(maxage), index,  df(maxage), year, iyear                                                                          
      integer age,iage, i                                                                                                           
      double precision  observed, expected, residual, gammq                                                                         
      double precision  cu1(maxage),cu2(maxage),cu3(maxage),            &                                                           
       cu4(maxage), s                                                                                                               
      double precision Stock(maxyear), Recruit(maxyear)                                                                             
      double precision aa, bb                                                                                                       
!                                                                                                                                   
!     note:  cu1 to cu4 accumulate the sums of the 1st to 4th powers                                                                
!       of the residuals for each type of index.                                                                                    
!                                                                                                                                   
!                                                                                                                                   
! --------------------------EXECUTABLE CODE------------------------------                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!-------------------- First the catch-at-age data against sepable model                                                             
                                                                                                                                    
      Cchi2 = 0.                                                                                                                    
      cu1(1) = 0.                                                                                                                   
      cu2(1)  = 0.                                                                                                                  
      cu3(1)  = 0.                                                                                                                  
      cu4(1)  = 0.                                                                                                                  
      NoCdata = 0                                                                                                                   
      do year = lastyear-NySep+1, lastyear                                                                                          
        iyear = year-firstyear+1                                                                                                    
        do iage= 1, lastage-firstage+1-1                                                                                            
          if (CN(iyear,iage) .gt. 0d0) then                                                                                         
            Observed = dlog(CN(iyear,iage))                                                                                         
            Expected = dlog(PredCN(iyear,iage))                                                                                     
            residual = observed-expected                                                                                            
            If(weighted) residual=residual*W(iyear,iage)                                                                            
            cu1(1)=cu1(1) + residual                                                                                                
            Cchi2 = Cchi2 + residual*residual/dabs(expected)                                                                        
            s = residual*residual                                                                                                   
            cu2(1) = cu2(1)+s                                                                                                       
            s = s*residual                                                                                                          
            cu3(1) = cu3(1)+s                                                                                                       
            s = s*residual                                                                                                          
            cu4(1) = cu4(1)+ s                                                                                                      
            NoCdata = NoCdata+1                                                                                                     
          endif                                                                                                                     
        enddo ! ages                                                                                                                
      enddo ! years                                                                                                                 
                                                                                                                                    
      dfsep  = NoCdata -  &  ! dfsep is degrees of freedom of separable model                                                        
       (Nysep+Nysep+lastage-firstage+lastage-firstage-3)                                                                            
                                                                                                                                    
                                                                                                                                    
      if (TwoSel) dfsep= dfsep - (lastage-firstage+1-3) ! if two selection patterns, lower d.f.                                     
                                                                                                                                    
      CVar = (cu2(1) )/dble(dfsep)                                                                                                  
      cu2(1) = cu2(1)/dble(NoCdata)                                                                                                 
      cu3(1) = cu3(1)/dble(NoCdata)                                                                                                 
      cu4(1) = cu4(1)/dble(NoCdata) - 3.*cu2(1)*cu2(1)                                                                              
      Cskew = sqrt(NoCdata/6.0)*cu3(1)/cu2(1)**(3.0/2.0)                                                                            
      CKurt = sqrt(NoCdata/24.0)*cu4(1)/(cu2(1)*cu2(1))                                                                             
      aa = 0.5d0*dble(dfsep)                                                                                                        
      bb = 0.5d0*Cchi2                                                                                                              
      CPchi = GAMMQ(aa,bb)                                                                                                          
                                                                                                                                    
!----------------------- Indices of spawning biomass                                                                                
                                                                                                                                    
      do index = 1, nssbix                                                                                                          
        Bchi2(index) = 0.                                                                                                           
        cu1(1) = 0.                                                                                                                 
        cu2(1) = 0.                                                                                                                 
        cu3(1) = 0.                                                                                                                 
        cu4(1) = 0.                                                                                                                 
        id(1) = 0                                                                                                                   
        do iyear = 1, lbyear-fbyear+1                                                                                               
          if (Bindex(index,iyear) .ne. -99d0) then                                                                                  
            observed=Bindex(index,iyear)                                                                                            
            expected=PredBindex(index,iyear)                                                                                        
            residual = observed-expected                                                                                            
            if (weighted) residual=residual*Blambda(index)                                                                          
            Bchi2(index)=Bchi2(index)+residual*residual/dabs(expected)                                                              
            cu1(1) = cu1(1) + residual                                                                                              
            s = residual*residual                                                                                                   
            cu2(1) = cu2(1)+s                                                                                                       
            s = s*residual                                                                                                          
            cu3(1) = cu3(1)+s                                                                                                       
            s = s*residual                                                                                                          
            cu4(1) = cu4(1)+s                                                                                                       
            id(1) = id(1)+1                                                                                                         
          endif ! not a  missing value                                                                                              
        enddo ! years                                                                                                               
                                                                                                                                    
        NoBdata(index) = id(1)                                                                                                      
        df(index)  = NoBdata(index) - QBparm(index)                                                                                 
        BVar(index) = cu2(1)/ dble(df(index))                                                                                       
        cu2(1) = cu2(1)/dble(NoBdata(index))                                                                                        
        cu3(1) = cu3(1)/dble(NoBdata(index))                                                                                        
        cu4(1) = cu4(1)/dble(NoBdata(index)) - 3.0*cu2(1)*cu2(1)                                                                    
        Bskew(index)=sqrt(NoBdata(index)/6.0)*cu3(1)/cu2(1)**(3.0/2.0)                                                              
        BKurt(index)=sqrt(NoBdata(index)/24.0)*cu4(1)/(cu2(1)*cu2(1))                                                               
        aa = 0.5*dble(df(1))                                                                                                        
        bb = 0.5*Bchi2(index)                                                                                                       
        BPchi(index) = GAMMQ(aa,bb)                                                                                                 
      enddo  ! SSB indices                                                                                                          
                                                                                                                                    
!---------------------------------  Now the age-structured indices                                                                  
                                                                                                                                    
                                                                                                                                    
      if (nageix .gt. 0 ) then                                                                                                      
        do index = 1,nageix                                                                                                         
          do age= fage(index),lage(index)                                                                                           
            iage = age-fage(index)+1                                                                                                
            Achi2(index,iage) = 0.0                                                                                                 
            id(iage) = 0                                                                                                            
            cu1(iage) = 0d0                                                                                                         
            cu2(iage) = 0d0                                                                                                         
            cu3(iage) = 0d0                                                                                                         
            cu4(iage) = 0d0                                                                                                         
          enddo                                                                                                                     
          do iyear = 1, lyear(index)-fyear(index)+1                                                                                 
            do age= fage(index),lage(index)                                                                                         
              iage = age-fage(index)+1                                                                                              
              If (Aindex(index,iyear,iage).ne.(MISSING)) then                                                                       
                observed = Aindex(index,iyear,iage)                                                                                 
                expected = PredAindex(index,iyear,iage)                                                                             
                residual = observed-expected
                if (weighted) residual=residual*Alambda(index,iage)
                Achi2(index,iage) = Achi2(index,iage)+                  &                                                           
                  residual*residual/dabs(expected)                                                                                  
                cu1(iage) = cu1(iage) + residual                                                                                    
                s = residual*residual                                                                                               
                cu2(iage) = cu2(iage)+s                                                                                             
                s = s*residual                                                                                                      
                cu3(iage) = cu3(iage)+s                                                                                             
                s = s*residual                                                                                                      
                cu4(iage) = cu4(iage)+ s                                                                                            
                id(iage) = id(iage)+1                                                                                               
!           write(*,*)'Used',index,iage+fage(index)-1,observed,id(iage)                                                             
!              else                                                                                                                 
!           write(*,*)'Rej.',index,iage+fage(index)-1,                                                                              
!     *        Aindex(index,iyear,iage),id(iage)                                                                                    
              endif ! not a missing value                                                                                           
            enddo ! ages                                                                                                            
          enddo   ! years                                                                                                           
                                                                                                                                    
          do iage=1,lage(index)-fage(index)+1 ! check data                                                                          
            if (id(iage) .eq. 0) then                                                                                               
              write (*,*) 'CALCSTATS: No data for aged index ',index,   &                                                           
              'at age ',iage+fage(index)-1                                                                                          
              stop                                                                                                                  
            endif                                                                                                                   
            if (cu2(iage) .le. 0d0) then                                                                                            
              write(*,*) 'CALCSTATS: No variance for aged index ',      &                                                           
               index,                                                   &                                                           
              'at age ', iage+fage(index)-1                                                                                         
            endif                                                                                                                   
          enddo  ! checking loop                                                                                                    
                                                                                                                                    
          do iage=1,lage(index)-fage(index)+1                                                                                       
            NoAdata(index,iage) = id(iage)                                                                                          
            df(iage)=NoAdata(index,iage)-QAparm(index)                                                                              
!            write(*,*) index,iage+fage(index)-1,cu1(iage),id(iage),                                                                
!     *      df(iage)                                                                                                               
            AVar(index,iage)=cu2(iage)/ dble(df(iage))                                                                              
                                                                                                                                    
            cu2(iage) = cu2(iage)/dble(NoAdata(index,iage))                                                                         
            cu3(iage) = cu3(iage)/dble(NoAdata(index,iage))                                                                         
            cu4(iage) = cu4(iage)/dble(NoAdata(index,iage))             &                                                           
                -3.*cu2(iage)*cu2(iage)                                                                                             
            Askew(index,iage) = sqrt(NoAdata(index,iage)/6.0)*          &                                                           
            cu3(iage)/cu2(iage)**(3.0/2.0)                                                                                          
            AKurt(index,iage) = sqrt(NoAdata(index,iage)/24.0)*         &                                                           
            cu4(iage)/(cu2(iage)*cu2(iage))                                                                                         
            aa = 0.5d0*dble(df(iage))                                                                                               
            bb = 0.5d0*Achi2(index,iage)                                                                                            
            APchi(index,iage) = GAMMQ(aa,bb)                                                                                        
          enddo ! ages                                                                                                              
        enddo ! indices                                                                                                             
      endif   ! any age-structured indices                                                                                          
                                                                                                                                    
!     The SRR data                                                                                                                  
                                                                                                                                    
      If (FitSRR) then                                                                                                              
        Call GetSRR(Stock, Recruit, NoSRRdata)                                                                                      
        cu2(1) =0d0                                                                                                                 
        do i=1,nosrrdata                                                                                                            
          Expected = dlog(PredRecruit(i))                                                                                           
          Observed = dlog(REcruit(i))                                                                                               
          residual = observed-expected                                                                                              
          if (weighted) residual=residual*SRRLambda
          cu2(1) = cu2(1) + residual*residual
        enddo
      SRRVar = cu2(1)/dble(NoSRRdata-2)  
      endif                                                                                                                         
                                                                                                                                    
                                                                                           
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                   
      DOUBLE PRECISION FUNCTION GAMMQ(A,X)                                                                                          
!                                                                                                                                   
! ////////////////////////////////////////////////////////////////////////                                                          
!    See Press et al. Numerical Recipes p. 162 (full ref given in routine BRENT)                                                    
!                                                                                                                                   
                                                                                                                                    
      double precision  A, Gamser, GLN, X, GAMMCF                                                                                   
                                                                                                                                    
      If ((X.lt.0d0).or.(A.le.0d0)) write (*,*) 'GAMMQ: Error '                                                                     
      if (X .lt. A+1d0) then                                                                                                        
         call GSER(Gamser,A,X,GLN)                                                                                                  
         GAMMQ = 1d0 - GAMSER                                                                                                       
      else                                                                                                                          
        call GCF(GAMMCF, A, X, GLN)                                                                                                 
        GAMMQ = GAMMCF                                                                                                              
      endif                                                                                                                         
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
      Subroutine Gser(GamSer, A, X, GLN)                                                                                            
!                                                                                                                                   
!//////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!     See Numerical Recipes also.                                                                                                   
                                                                                                                                    
      double precision Gamser, X, GLN, DEL, SUM, A, AP                                                                              
      double precision GAMMLN, eps                                                                                                  
      integer itmax, n                                                                                                              
                                                                                                                                    
      parameter (itmax=100,eps = 3.d-7)                                                                                             
      GLN = GAMMLN(A)                                                                                                               
      if (X .le. 0d0) then                                                                                                          
        if (x.lt.0d0) write(*,*) 'Error calculating chi2 probability'                                                               
        gamser = 0d0                                                                                                                
        return                                                                                                                      
      endif                                                                                                                         
      AP = A                                                                                                                        
      Sum = 1d0/A                                                                                                                   
      DEL = SUM                                                                                                                     
      do N = 1,Itmax                                                                                                                
        AP = AP+1d0                                                                                                                 
        del = del*X/AP                                                                                                              
        sum =sum+Del                                                                                                                
        if (abs(del) .lt. Abs(sum)*eps) goto 1                                                                                      
      enddo                                                                                                                         
      write(*,*)  'GSER: A too large, Itmax too small'                                                                              
1     gamser = sum*exp(-X+A*log(X)-GLN)                                                                                             
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
      double precision FUNCTION GAMMLN(XX)                                                                                          
!                                                                                                                                   
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!       Also from Numerical Recipes                                                                                                 
!                                                                                                                                   
      double precision cof(6), stp,half,one,fpf,x,tmp,ser                                                                           
      double precision xx
      integer j                                                                                                                     
                                                                                                                                    
      data cof, stp/76.18009173d0,-86.50532033d0,24.01409822d0,         &                                                           
        -1.231739516d0,0.120858003d-2,-.536382d-5,2.50662827465d0/                                                                  
      data half,one,fpf/0.5d0,1.0d0,5.5d0/ 
      if (xx .lt. 0) write(*,*)  'GAMMLN: XX lt 0 '                                                                                 
                                                                                                                                    
      x = xx-one                                                                                                                    
      tmp = x+fpf                                                                                                                   
      tmp =(x+half)*log(tmp) - tmp                                                                                                  
      ser = one                                                                                                                     
      do j =1,6                                                                                                                     
        x = x+one                                                                                                                   
        ser = ser+cof(j)/x                                                                                                          
      enddo                                                                                                                         
      gammln = tmp+log(stp*ser)                                                                                                     
      return                                                                                                                        
      end                                                                                                                           
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                         
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
      subroutine GCF(GAMMCF, AA, X, GLN)                                                                                            
!                                                                                                                                   
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!       From Numerical Recipes                                                                                                      
!                                                                                                                                   
      integer itmax                                                                                                                 
      double precision eps                                                                                                          
      Parameter (Itmax = 100, eps = 3.d-7)                                                                                          
      integer n                                                                                                                     
      double precision GLN,GAMMCF,AA,X,Gold,A0,A1,B0,B1,ANA,G,FAC                                                                   
      double precision GAMMLN, AN, ANF                                                                                              
                                                                                                                                    
      GLN = GAMMLN(AA)                                                                                                              
                                                                                                                                    
      Gold = 0d0                                                                                                                    
      A0 =1d0                                                                                                                       
      A1 = X                                                                                                                        
      B0 = 0d0                                                                                                                      
      B1 = 1d0                                                                                                                      
      FAC = 1d0                                                                                                                     
      do n= 1,itmax                                                                                                                 
        AN = dble(N)                                                                                                                
        ANA = AN -AA                                                                                                                
        A0 = (A1+A0*ANA)*FAC                                                                                                        
        B0 = (B1+B0*ANA)*FAC                                                                                                        
        ANF=AN*FAC                                                                                                                  
        A1=X*A0+ANF*A1                                                                                                              
        B1=X*B0+ANF*B1                                                                                                              
        IF (A1 .ne.0d0) then                                                                                                        
           FAC = 1d0/A1                                                                                                             
           G = B1*FAC                                                                                                               
           IF (ABS((G-Gold)/G) .LT. EPS) GOTO 1                                                                                     
           Gold = G                                                                                                                 
        ENDIF                                                                                                                       
      enddo                                                                                                                         
      write(*,*) 'Out of iterations in GCF.'                                                                                        
1     GAMMCF = EXP(-X+AA*DLOG(X)-GLN)*G                                                                                             
      Return                                                                                                                        
      End                                                                                                                           
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      double precision FUNCTION RECIPE_RAN(IDUM)                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
! generates uniform random numbers U(0,1)                                                                                           
! uses 3 linear congruential generators and 'shuffles' #s                                                                           
! It's portable between machines with different precision                                                                           
                                                                                                                                    
      implicit none                                                                                                                 
      integer m1,m2,m3,ia1,ia2,ia3,ic1,ic2,ic3,iff,ix1,ix2,ix3                                                                      
      integer idum,j                                                                                                                
      double precision    rm1,rm2,r                                                                                                 
                                                                                                                                    
      save                                                                                                                          
      DIMENSION R(97)                                                                                                               
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247d-6)                                                                     
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773d-6)                                                                     
      PARAMETER (M3=243000,IA3=4561,IC3=51349)                                                                                      
      DATA IFF /0/                                                                                                                  
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN                                                                                               
	IFF=1                                                                                                                              
	IX1=MOD(IC1-IDUM,M1)                                                                                                               
	IX1=MOD(IA1*IX1+IC1,M1)                                                                                                            
	IX2=MOD(IX1,M2)                                                                                                                    
	IX1=MOD(IA1*IX1+IC1,M1)                                                                                                            
	IX3=MOD(IX1,M3)                                                                                                                    
        DO  J=1,97                                                                                                                  
	  IX1=MOD(IA1*IX1+IC1,M1)                                                                                                          
	  IX2=MOD(IA2*IX2+IC2,M2)                                                                                                          
          R(J)=(dble(IX1)+dble(IX2)*RM2)*RM1                                                                                        
        enddo                                                                                                                       
	IDUM=1                                                                                                                             
      ENDIF                                                                                                                         
      IX1=MOD(IA1*IX1+IC1,M1)                                                                                                       
      IX2=MOD(IA2*IX2+IC2,M2)                                                                                                       
      IX3=MOD(IA3*IX3+IC3,M3)                                                                                                       
      J=1+(97*IX3)/M3                                                                                                               
       IF(J.GT.97.OR.J.LT.1) then                                                                                                   
       write(*,*) ' ERROR ENCOUNTERED IN RANDOM NUMBER GENERATOR'                                                                   
       STOP                                                                                                                         
       endif                                                                                                                        
      RECIPE_RAN=R(J)                                                                                                               
      R(J)=(dble(IX1)+dble(IX2)*RM2)*RM1                                                                                            
      RETURN                                                                                                                        
      END                                                                                                                           
                                                                                                                                    
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                         
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
                                                                                                                                    
                                                                                                                                    
      Double precision Function Recipe_Gasdev(IDUM)                                                                                 
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
                                                                                                                                    
!   Returns random Normal deviate N(0,1)                                                                                            
!                                                                                                                                   
!                                                                                                                                   
!   see Numerical Recipes page 203                                                                                                  
!         modified so it doesn't need to store last variate                                                                         
!                                                                                                                                   
      implicit none                                                                                                                 
      double precision V1,V2,R,FAC,Recipe_ran,Gset                                                                                  
      integer iset, idum                                                                                                            
                                                                                                                                    
                                                                                                                                    
      Data ISET /0/                                                                                                                 
                                                                                                                                    
!      If (ISET .eq. 0) then                                                                                                        
1       V1=2d0*Recipe_ran(idum)-1d0                                                                                                 
        V2=2d0*Recipe_ran(idum)-1d0                                                                                                 
        R = V1*V1+V2*V2                                                                                                             
        If (R .ge. 1d0 .or. R .eq. 0d0) goto 1                                                                                      
        FAC=DSQRT(-2d0*dlog(R)/R)                                                                                                   
        GSET = V1*FAC                                                                                                               
        Recipe_GASDEV=V2*FAC                                                                                                        
!        Iset = 1                                                                                                                   
!      else                                                                                                                         
!        Recipe_GASDEV=GSET                                                                                                         
!        Iset = 0                                                                                                                   
!      endif                                                                                                                        
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!                                                                                                                                   
