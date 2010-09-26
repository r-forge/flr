c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c            UNIT STATS : Routines for calculating variances, c.v.s etc.
c
c
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


c ////////////////////////////////////////////////////////////////////////
c
      double precision FUNCTION GAMMQ(A,X)
c
c ////////////////////////////////////////////////////////////////////////
c    See Press et al. Numerical Recipes p. 162 (full ref given in routine BRENT)
c

      double precision A, Gamser, GLN, X, GAMMCF

      If (X .lt.0. .or. A .le. 0.) write (*,*) 'GAMMQ: Error '
      if (X .lt. A+1.) then
         call GSER(Gamser,A,X,GLN)
         GAMMQ = 1.0 - GAMSER
      else
        call GCF(GAMMCF, A, X, GLN)
        GAMMQ = GAMMCF
      endif
      return 
      end


c /////////////////////////////////////////////////////////////////////////
c
      Subroutine Gser(GamSer, A, X, GLN)
c
c//////////////////////////////////////////////////////////////////////////
c
c     See Numerical Recipes also.

      double precision Gamser, X, GLN, DEL, SUM, A, AP,eps
      double precision GAMMLN
      integer itmax
      
      parameter (itmax=100,eps = 3.0d-7)
      GLN = GAMMLN(A)
      if (X .le. 0) then
        if (x.lt.0) write(*,*) 'Error calculating chi2 probability'
        gamser = 0.
        return
      endif
      AP = A
      Sum = 1./dble(A)
      DEL = SUM
      do N = 1,Itmax
        AP = AP+1.
        del = del*X/AP
        sum =sum+Del
        if (abs(del) .lt. Abs(sum)*eps) goto 1
      enddo
      write(*,*)  'GSER: A too large, Itmax too small'
1     gamser = sum*exp(-X+A*log(X)-GLN)  
      return
      end


c /////////////////////////////////////////////////////////////////////////
c
      double precision FUNCTION GAMMLN(XX)
c
c /////////////////////////////////////////////////////////////////////////
c
c       Also from Numerical Recipes
c
      double precision cof(6), stp,half,one,fpf,x,tmp,ser
      double precision xx
      integer j

      data cof, stp/76.18009173d0,-86.50532033d0,24.01409822d0,
     *  -1.231739516d0,0.120858003d-2,-.536382d-5,2.50662827465d0/
      data half,one,fpf /0.5d0,1.0d0,5.5d0/
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
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                     

c /////////////////////////////////////////////////////////////////////////
c
      subroutine GCF(GAMMCF, A, X, GLN)
c
c /////////////////////////////////////////////////////////////////////////
c
c       From Numerical Recipes
c
      Parameter (Itmax = 100, eps = 3.e-7)
      integer n
      double precision GLN,GAMMCF,A,X,Gold,A0,A1,B0,B1,ANA,G,FAC
      double precision GAMMLN

      GLN = GAMMLN(A)
      Gold = 0.
      A0 =1.
      A1 = X
      B0 = 0.
      B1 = 1.
      FAC = 1.
      do n= 1,itmax
        AN = Float(N)
        ANA = AN -A
        A0 = (A1+A0*ANA)*FAC
        B0 = (B1+B0*ANA)*FAC
        ANF=AN*FAC
        A1=X*A0+ANF*A1
        B1=X*B0+ANF*B1
        IF (A1 .ne.0.) then
           FAC = 1./A1
           G = B1*FAC
           IF (ABS((G-Gold)/G) .LT. EPS) GOTO 1
           Gold = G
        ENDIF
      enddo
      write(*,*) 'Out of iterations in GCF.'  
1     GAMMCF = EXP(-X+A*dLOG(X)-GLN)*G      
      Return
      End

