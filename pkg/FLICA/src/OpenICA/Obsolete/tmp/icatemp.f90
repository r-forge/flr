!      WINAPP 9000000,9000000 

! //////////////////////////////////////////////////////////////////////////                                                        
!                                                                                                                                   
       Program ICA                                                                                                                  
                                                                                                                                    
!//////////////////////////////////////////////////////////////////////////                                                         

!      Use MSWIN32
!      Use CLRWIN

      implicit none                                                                                                                 
      character*1 dummy
      integer tscan
      external tscan
      character*77 text(1)

      include "indat.inc"                                                                                                           
      include "sepmodel.inc"                                                                                                        
      include "labels.inc"                                                                                                          
      include "stats.inc"                                                                                                           
      include "predic.inc"                                                                                                          
      include "message1.inc"
      double precision X(maxparm), VCV(Maxparm,maxparm)
      include "message2.inc"



      call SOutputWindow('ICA 1.4 w',Ica_log)     ! Set up the output window
      language=1
      RecalculatePopulations=.true.
!      goto 100
      call ICA1
!      write(*,*) 'CAll to ICA1 finished OK'
      call ICA2(X,VCV)

!100   call readblock
      dummy=KO(1,Language)
      call Screen_in_a(HL(35,Language), dummy, KO(1,Language), Language)



      if (Tscan(dummy, KY(1,Language)) .ne.0 ) then
         dummy='c'
         call Screen_in_a(HL(42,Language), dummy, 'bBcC', Language)
         if (Tscan(dummy, 'cC') .ne. 0) then
           call PBootstrap(X,VCV)
         else
           call MCMC
         endif
      endif

      Text(1) =HL(43,Language)

      call Screen_out_a(Text, 1, 1)
      end                                                                                                                           
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      Subroutine ICA1                                                                                                               
                                                                                                                                    
!                                                                                                                                   
!     Version 1.3. Written November 1996 by K.R. Patterson                                                                          
!                                                                                                                                   
!                                                                                                                                   
!     PURPOSE :    This programme serves three functions:                                                                           
!     -------           (1) to indicate discrepancies between tuning indices                                                        
!                       (2) to initialise starting parameters for ICA2                                                              
!                       (3) To run the minimisation                                                                                 
!                                                                                                                                   
!     METHOD  :    After reading in age-structured data and weighting information,                                                  
!     ------       the programme calculates 20 separable VPAs using the Pope and Shepherd                                           
!                  algorithm over a range of Fs specified by the user.                                                              
!                  For each VPA the fit of the populations to the tuning index is                                                   
!                  calculated by simple least-squares. The values of F and the goodness                                             
!                  of fit to each index are written to a file (ICADIAG.OUT).                                                        
!                  An aggregate measure of goodness of fit is also calcualted and a simple                                          
!                  minimisation routine is used to find the F which gives the best fit to the indices.                              
!                  Parameter estimates at this minimum are written to a file (ICA.TMP)                                              
!                  which is used as to initialise the ICA2 programme.                                                               
!                                                                                                                                   
!   (UNITS are names of files which contain several routines)                                                                       
!                                                                                                                                   
!     SUBROUTINES :  UNIT 'READER'                                                                                                  
!     -----------    Reader  : Inputs age-structured data in Lowestoft                                                              
!                              format                                                                                               
!                    ReadSSBIX : Inputs SSB indices in RCT3 format                                                                  
!                    ReadAgix  : Inputs age-structured indices                                                                      
!                    TableOut  : outputs data to output text file                                                                   
!                                 and to view file                                                                                  
!                    ReadBlock  : Reads all programme variables from                                                                
!                                 a temporary file                                                                                  
!                                                                                                                                   
!                    UNIT 'PARMSET'                                                                                                 
!                    ParmSet   : Set up initial parameter estimates                                                                 
!     FUNCTIONS      Cohort    : Pope's cohort analysis                                                                             
!                    Refine    : Exact VPA calculation                                                                              
!                    CalcSSB   : Calculate spawning stock biomass in year                                                           
!                                                                                                                                   
!                    UNIT 'SEPVPA'                                                                                                  
!                    SepVPA : Separable VPA by Pope & Shepherd method                                                               
!                                                                                                                                   
!                    UNIT 'CONVPA'                                                                                                  
!                    ConVPA : Conventional VPA initiated from SepVPA,                                                               
!                             then fits  catchability models for the indices                                                        
!                             and then puts the right parameters in XBest                                                           
!                                                                                                                                   
!                    UNIT 'NSEP'                                                                                                    
!                    NSEP   : The main programm unit                                                                                
!                    Brent  : One-dimensional function minimisation                                                                 
!                    SepRES : housekeeping fuction, allows Brent to use SepRes+ConVPA                                               
!                                                                                                                                   
!                    UNIT 'WRITEBLOCK'                                                                                              
!                    WRITEBLOCK : Write all programme variables to a temporary file                                                 
!                                                                                                                                   
!                    UNIT 'OBJECT'                                                                                                  
!                    LSFUN1 : The objective function                                                                                
!                                                                                                                                   
!                    UNIT 'SRR'                                                                                                     
!                    ReadDAT   : Read any estimates of stock and recruit prior to the main data set                                 
!                    GetSRR    : Extract stock and recruit data from main dataset                                                   
!                    ApproxSRR : Approximate estimate of SRR params by linear regression                                            
!                    WriteSRRFile : SRR parameter estimates, data and residuals                                                     
!                                                                                                                                   
!    FILE ACCESS SUMMARY                                                                                                            
!    ===================                                                                                                            
!                                                                                                                                   
!      FILE DESCRIPTION          NAME     ACCESS TYPE      ACCESSED BY                                                              
!                                                           UNIT         ROUTINE                                                    
!     -- file names specified by user at run-time                                                                                   
!                                                                                                                                   
!     index of age-structured datafiles         READ         READER      READER                                                     
!     age-structured input data files           READ         READER      READER                                                     
!     survey indices of SSB                     READ         READER      READSSBIX                                                  
!     age-structured survey indices             READ         READER      READAGIX                                                   
!     historical estimates of Stock and Recruit READ         SRR         READDAT                                                    
!                                                                                                                                   
!     -- output files always have these names                                                                                       
!                                                                                                                                   
!     F and SSQ by survey index: ICADIAG.OUT    WRITE        MAIN        MAIN                                                       
!     main output file           ICA.OUT        WRITE        READER      TABLEOUT                                                   
!     file for graphic programme ICA.VIE        WRITE        READER      TABLEOUT                                                   
!     all data and parameters    ICA.TMP        WRITE        WRITEBLOCK  WRITEBLOCK                                                 
!     residuals                  ICA.RES        WRITE        OBJECT      LSFUN1                                                     
!     stock-recruit params       ICA.SRR        WRITE        SRR         WRITESRRFFILE                                              
!                                                                                                                                   
! =========================================================================================                                         
!                                                                                                                                   
!                                                                                                                                   
!     --- COMMON BLOCKS ---                                                                                                         
!                                                                                                                                   

      Use MSWIN32
      Use CLRWIN

      Include "InDat.inc"
      Include "SepModel.Inc"                                                                                                        
      Include "Stats.Inc"                                                                                                           
      Include "SRR.Inc"                                                                                                             
      Include "Predic.inc"                                                                                                          
      Include "message1.inc"                                                                                                        
                                                                                                                                    
                                                                                                                                    
!     ----------------------                                                                                                        
                                                                                                                                    
                                                                                                                                    
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
      Double Precision Brent                                                                                                        
      Double precision SepRes                                                                                                       
      External Sepres                                                                                                               
      integer Noparm, Lastsel1, lastsel2, lastparm                                                                                  
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
                                                                                                                                    
      CALL Reader               ! Read in catch at age data                                                                         
      CALL ReadAgIx             ! Read in age -structured indices                                                                   
      CALL ReadSBIx             ! Read in SSB indices                                                                               
      Call CalcSOP              ! Sum-of-Products calculation                                                                       
                                                                                                                                    
      if ( (Nageix+Nssbix) .eq. 0)  then                                                                                            
        Text(1)= HL(20,language)                                                                                                    
        Text(2)= HL(21,language)                                                                                                    
        Call Screen_out_a(Text,24,2)                                                                                                
        stop                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
      CALL ParmSet              ! Set up starting parameters & initial choices                                                      
                                                                                                                                    
                                                                                                                                    
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
      do while ((low .ge. high)  .or. (low .lt. 0.02d0))  
        Call Screen_in_r(HL(6,Language),low,max,min,language)                                                                    
                                                                                                                                    
        Call Screen_in_r(HL(7,Language),high,max,min,language)                                                                   
                                                                                                                                    
        if (low .ge. high) then                                                                                                     
          Text(1)= HL(10,Language)                                                                                                  
          Call Screen_out_A(Text, 24, 1)                                                                                            
          low = 0d0                                                                                                                 
          high = 0d0                                                                                                                
        endif                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      Text(1) = HL(11,Language)                                                                                                     
      Text(2) = ' '                                                                                                                 
      Text(3) =  '    F                  SSQ  '                                                                                     
      Text(4) =  '+--------+-------------------'                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      SSQLow = 1d10                                                                                                                 
      Flow = 0d0                                                                                                                    
                                                                                                                                    
      open (11, file = ICAdiag_out, status = 'Unknown')                                                                             
      write(11,*) 'ICA v1_3 TUNING DIAGNOSTICS FILE'                                                                                
      write(11,'(I4,4X,I4)'  ) nssbix, nageix                                                                                       
      write(11,*) 'F         SSQ'                                                                                                   
                                                                                                                                    
                                                                                                                                    
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

                                                                                                                                    
        write(11,9040) temp, SumRES,                                    &                                                           
         (Res(i), i=1,nssbix),(Res(index+nssbix),index=1,nageix)                                                                    


         write(Text(4+j),9030) temp,SumRES                                                                                          

                                                                                                                                    
                                                                                                                                    
      enddo   ! 20 separable VPAs now run; Fs and SSQs written to ICADIAG.OUT                                                       
                                                                                                                                    
      close(11)                                                                                                                     
                                                                                                                                    
      Call Screen_out_a(text,24,24)                                                                                                 
                                                                                                                                    
!     Now start one-dimensional search in F dimension for lowest SSQ,                                                               
!     starting from FLow and using the BRENT method                                                                                 
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
      Fupper = (Flow + (high-low)/19d0)                                                                                             
      Flower = (Flow -(high-low)/19d0)                                                                                              
      toler = 0.01d0                                                                                                                
      Fstart = 0d0                                                                                                                  
                                                                                                                                    
!     Solve Flow for lowest SSQ between Flower and Fupper with specified tolerance                                                  
                                                                                                                                    
      SSQLow = Brent(Flower,Flow,Fupper,SepRES,toler,Fstart)                                                                        
                                                                                                                                    
!     solution should have been found here, assuming 1 selection pattern                                                            
                                                                                                                                    
      write(ntext,100) Fstart                                                                                                       
100   format(' ',F8.3)                                                                                                              
      Call ConCat(Text(1),HL(12,Language),ntext)                                                                                    
      Call Screen_out_a(text,24,1)                                                                                                  
                                                                                                                                    
                                                                                                                                    
!     Adjust the parameter list for two selection patterns if                                                                       
!     necessary                                                                                                                     
                                                                                                                                    
                                                                                                                                    
      If (TwoSel) then                                                                                                              
        LastParm = NxParm                                                                                                           
        NoParm=NySep+2*(lastage-firstage+1-3)+ &   ! F and S params                                                                 
         (NySep + (lastage-firstage+1-1) -1)      ! fitted populations                                                              
                                                                                                                                    
        UseRec = .false.                                                                                                            
        do i = 1,NAgeix                                                                                                             
          If(Aindex(i,lastyear-fyear(i)+2,1).gt.0d0)UseRec=.True.                                                                   
                                                                                                                                    
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
!        write(*,*) 'CAlling approx srr ...'                                                                                        
        Call ApproxSRR                                                                                                              
!        write(*,*) 'Approx srr ok '                                                                                                
        Nxdata = Nxdata+ (lastyear-firstyear+1)-lag                                                                                 
        NxParm = NxParm+2                                                                                                           
        if ((a .le. 0.0) .or. (b .le. 0.0)) then                                                                                    
         Text(1)= HL(14,Language)                                                                                                   
         Call Screen_out_a(Text(1),24,1)                                                                                            
         stop                                                                                                                       
        endif                                                                                                                       
        Xbest(Nxparm-1) = dlog(a)                                                                                                   
        Xbest(Nxparm) = dlog(b)                                                                                                     
        Xlow(Nxparm-1) = -1                                                                                                         
        Xhigh(Nxparm-1) = -1                                                                                                        
        Xlow(Nxparm) = -1                                                                                                           
        XHigh(NxParm) = -1                                                                                                          
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
                                                                                                                                    
!      write(*,*) 'CAll to LSFUN1'   
      CALL LSFUN1(Nxdata,NxParm, Xbest, Resids)                                                                                     
!      write(*,*) 'CAll to LSFUN1 OK' 
      SSQ=0d0
      do i=1,Nxdata                                                                                                                 
        SSQ= SSQ+Resids(i)*Resids(i)                                                                                                
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
!     Calculate variances, chi-squareds etc.                                                                                        

      CALL CalcStats(.false.)   


!     Initiating values for the survey index weights (lambdas)
                                                                                                                                    
      do i = 1,Nssbix                                                                                                               
        Blambda(i) = CVar/BVar(i)                                                                                                   
      enddo                                                                                                                         
                                                                                                                                    
      do i = 1,Nageix                                                                                                               
       do age = fage(i),lage(i)                                                                                                     
        iage = age-fage(i)+1                                                                                                        
        Alambda(i,iage) = CVar/AVar(i,iage)                                                                                         
       enddo                                                                                                                        
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!9000  format (A40)                                                                                                                 
!9010  format (A1)                                                                                                                  
9030  format (F8.2,2X,F20.10)                                                                                                       
9040  format (F6.2,1X,40(G16.10,1X))                                                                                                
!9050  format (F10.5)                                                                                                               
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
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
      Pause 'Brent exceeded maximum iterations'                                                                                     
3     Xmin = x                                                                                                                      
      Brent = FX                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
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
                                                                                                                                    
      Include "Indat.inc"                                                                                                           
      Include "Sepmodel.inc"                                                                                                        
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
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
                                                                                                                                    
!      write(*,*) ' Estimate of F  : ',fref                                                                                         
                                                                                                                                    
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
      end                                                                                                                           
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
                                                                                                                                    

! ///////////////////////////////////////////////////////////////////////////

      Subroutine PBootstrap(X,VCV)

! ///////////////////////////////////////////////////////////////////////////
!
!   Estimate uncertainty by parametric bootstrap.
!
!
      Use MSWIN32
      Use CLRWIN

      implicit none                                                                                                                 
                                                                                                                                    
      include "indat.inc"                                                                                                           
      include "sepmodel.inc"                                                                                                        
      include "labels.inc"                                                                                                          
      include "stats.inc"                                                                                                           
      include "predic.inc"                                                                                                          
      include "message1.inc"



      double precision eps, Z(maxparm), VCV(maxparm,maxparm),X(maxparm)
      double precision R ( (maxparm+1)*(maxparm+2)/2  )
      double precision G05DDF
      integer NR,IFail,Its,iyear,iage


      double precision MBAL, MbalMin,MbalMax
      character*1 dummy
      character*80 text(2)
      integer itsToRun,Imin,i, ic, age
      integer N_percentiles, nmax,nmin

      integer tscan

      external tscan

      parameter(nmax=10)
      parameter(Imin=100)
      parameter (MbalMin=0d0,MbalMax=1d12)

      double precision percentile(nmax), FC(maxdata)


      nmin=3
      NR=(maxparm+1)*(maxparm+2)/2
      MBal =0d0



!        Call Screen_in_i(HL(36,Language),n_percentiles,nmax,nmin,Language)

      call GetPercentiles(N_percentiles, percentile, nmax)

      call Screen_in_i(HL(38,Language),ItsToRun,120000,Imin,Language)

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

      open(33, file ='INIT.TXT', Status='Unknown')


      do ITS = 1, ItsToRun
!        write(*,*) Its
        Call G05EZF(Z,NxParm, R, NR, Ifail)

        if (Ifail .ne. 0) then
          write(Text(1), '(I2)') IFAIL
          Call ConCat(Text(1), HW(20,Language), Text(1))
          Call Screen_out_a(Text(1),10,1)
        endif


        call LSFUN1(NxData, NxParm, Z, FC)


        iyear=lastyear-firstyear+1
       
        write(33,99) (N(iyear,iage) , iage=1,lastage-firstage+1),  &
                     (F(iyear,iage), iage=1,lastage-firstage+1)


99      format ( 1X,50(E24.12,1X))



!        call Tableout(1) ! to test


      call WriteIterations(Z, 27)


      enddo  ! iterations

      close(33)

      pause

!
!     close the *.mci files
!
      Call CloseItnFiles(27)


!
!     calculate the percentiles and create *.pbi files
!
      call CreatePBYFiles(N_percentiles, Percentile,nmax)





      return
      end





! ///////////////////////////////////////////////////////////////////////

      Subroutine WriteIterations(Z, fch)

! /////////////////////////////////////////////////////////////////////////
      implicit none
      include 'indat.inc'
      include 'sepmodel.inc'
      integer iyear, fch, i, age
      double precision Stock(maxyear), MeanF(maxyear), calcssb,Z(maxparm)
      external calcssb

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
      end

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


! ////////////////////////////////////////////////////////////////////////

       Subroutine OPenItnFiles(ItsToRun, MBAL,mcfile)

! ////////////////////////////////////////////////////////////////////////
       implicit none
       include 'indat.inc'

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
       end

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
      end

