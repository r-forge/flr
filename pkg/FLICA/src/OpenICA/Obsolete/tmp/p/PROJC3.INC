                                                                                                                                    
! ---------- DATA BLOCK FOR THE PROJECTIONS --------------------------                                                              
      integer MaxProYr, MaxProFlt                                                                                                   
      parameter (MaxProYr = 20)                                                                                                     
      parameter (MaxProFlt = 6)                                                                                                     
                                                                                                                                    
                                                                                                                                    
      double precision Stock(MaxProYr), VarStock(MaxProYr),             &                                                           
       FLCatch(MaxProFlt,MaxProYr)  ! Estd catches, stock sizes and and variances                                                   
      double precision VarCatch(MaxProFlt,MaxProYr)                                                                                 
      integer YtPro, NFleet                        ! years to project                                                               
      double precision CRatio(MaxProFlt,maxage)                                                                                     
      double precision NMPro(maxage), CWPro(MaxProFlt,maxage),          &                                                           
          SWPro(maxage),                                                &                                                           
          Npro(MaxProYr,maxage)                                                                                                     
      double precision VarMat(maxage), VarSW(maxage)                                                                                
      double precision MOPro(maxage), FMultPro(MaxProFlt,MaxProYr),     &                                                           
          FPro(MaxProYr,maxage)                                                                                                     
      double precision MCMO(MaxProYr,maxage),MCSW(MaxProYr,maxage)     ! Monte Carlo Stock weights & maturities                     
      double precision Discards(MaxProFlt,MaxProYr),                    &                                                           
         Retention(MaxProFlt,maxage),                                   &                                                           
         DiscWt(MaxProFlt,maxage)                                                                                                   
      double precision ConsCat(MaxProFlt,MaxProYr), MaxFmult                                                                        
      integer CurrYear                                                                                                              
      double precision Select(maxage), SRRPARM(8), BSResid(maxyear)                                                                 
      double precision RecruitScaling  

      integer ftyear                                                                                                                
      double precision TargetFM(MaxProFlt,MaxProYr),                    &                                                           
                  CVTarg(MaxProFlt,MaxProYr)
      logical UseREc1, UseRec2
      integer NxParmUsedbyICA                        
                                                                                                                                    
      COMMON /PROJ/ Stock,VarStock,FLCatch,VarCatch,                    &                                                           
       YtPro,NFleet,CRatio,                                             &                                                           
       NMPro, CWPro, SWPro, Npro, MoPro, FMultPro, FPro, ConsCat,       &                                                           
       CurrYear, Select, SRRParm, Discards, Retention, DiscWt,          &                                                           
       BSResid,  VarMat, VarSW, MaxFmult, MCMO, MCSW                    &                                                           
       ,TargetFM, CVTarg, ftyear, UseRec1, UseRec2, NxParmUsedbyICA,   & 
       RecruitScaling
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
