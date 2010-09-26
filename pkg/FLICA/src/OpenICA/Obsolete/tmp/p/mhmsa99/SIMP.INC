
c ---------- DATA BLOCK FOR THE PROJECTIONS --------------------------

      double precision SDEV(75,20), CDev(75,6,20)              ! the matrices of derivatives
      double precision Stock(20), VarStock(20), FLCatch(6,20)  ! Estd catches, stock sizes and and variances
      double precision VarCatch(6,20)
      integer YtPro, NFleet                        ! years to project                      
      double precision CRatio(6,12)
      double precision NMPro(12), CWPro(6,12), SWPro(12), Npro(20,12)
      double precision VarMat(12), VarSW(12)
      double precision MOPro(12), FMultPro(6,20), FPro(20,12)
      double precision MCMO(20,12),MCSW(20,12)     ! Monte Carlo Stock weights & maturities
      double precision Discards(6,20),Retention(6,12),DiscWt(6,12)
      double precision ConsCat(6,20), MaxFmult 
      integer CurrYear
      double precision Select(12), SRRPARM(6), BSResid(40)
      integer NoSrrData, ftyear
      double precision TargetFM(6,20), CVTarg(6,20)

      COMMON /PROJ/ SDEV,CDev,Stock,VarStock,FLCatch,VarCatch,
     * YtPro,NFleet,CRatio,
     * NMPro, CWPro, SWPro, Npro, MoPro, FMultPro, FPro, ConsCat,
     * CurrYear, Select, SRRParm, Discards, Retention, DiscWt,
     * BSResid, NoSrrData, VarMat, VarSW, MaxFmult, MCMO, MCSW,
     * TargetFM, CVTarg, ftyear



