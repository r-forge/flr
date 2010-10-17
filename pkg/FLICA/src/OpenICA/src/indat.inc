!     _____________I N P U T___D A T A___D E F I N I T I O N ____________________

!
!     This data block covers the input data
!
!
!
!
      integer maxage, maxsurvey, maxyear, maxbsurv, maxdata, maxparm, maxiters
      double precision missing

      parameter (maxage=20)  ! maximum no of ages including + gp
      parameter (maxyear=60) ! maximum no of years in analysis
      parameter (maxsurvey=15) ! maximum no of age-structured surveys
      parameter (maxbsurv=5)  ! maximum number of SSB surveys
      parameter (maxdata=2500) ! maximum no of observations 2500
      parameter (maxparm=150) ! maximum no of parameters
      parameter (maxiters=2000) ! maximum iterations in uncertainty eval.
      parameter (missing = -99.0d0) ! missing value code



      Integer firstage, lastage, firstyear, lastyear     ! range of the catch at age data
      Integer fage(maxsurvey), lage(maxsurvey), &
      fyear(maxsurvey), lyear(maxsurvey)       ! first and last years and ages of the age-structured surveys
      Integer fbyear, lbyear                             ! first and last years of the SSB surveys
      integer nageix                                     ! number of age-structured surveys (max is 5)
      Double precision Timing(maxsurvey)                         ! Timing of each of the age-structured surveys in fractions of a year
      integer nssbix                                     ! Number of SSB surveys (max is 3)
      double precision CN(maxyear,maxage ), NM(maxyear,maxage )              ! Catch Number and Natural Mortality
      double precision SW(maxyear,maxage ), MO(maxyear,maxage ), &
      LA(maxyear), CW(maxyear,maxage)                                            ! Stock Weight, Maturity Ogive and Landings
      double precision PF, PM                            ! Proportions of F and M before spawning
      double precision AINDEX(maxsurvey,maxyear,maxage )                               ! Age structured survey data indexed as (survey, year, age)
      double precision BINDEX(maxbsurv,maxyear)                                  ! SSB survey data indexed as (survey, year)
      double precision N(maxyear,maxage ), F(maxyear,maxage ) 
      logical plusgp(maxsurvey)                                  ! plugp(n) is whether the last age of the nth age-structured survey is a plus-group
      logical RecalculatePopulations
      integer QAParm(maxsurvey), QBParm(maxbsurv)                       ! number of fitted parameters for catchability relationships
      double precision  SOP(Maxyear)                                                 
      double precision Npool(maxyear)                      ! dynamic pool plus-gp Ns
                                                         !                                     1 = proportionate (linear) catchability
                                                         !                                     2 = logarithmic   (power) catchability 

      COMMON /INDAT / CN, CW, SW, MO, NM, LA, PF, PM, AINDEX,    &
      BINDEX, N, F, SOP, NPOOL, firstage, lastage, firstyear, lastyear, &
      fage, lage, fyear, lyear,  fbyear, lbyear,                &
      nageix, nssbix, Timing, plusgp,                           &
      QAParm, QBParm, RecalculatePopulations                                
!c     -----------------------------------------------------------------

!     The names of files produced by ICA
      
      character*15 ica_sen, ica_sum, ica_vie, ica_out, icacn_prd,    &
       icacn_res, icaasurv_prd, icaasurv_res, icabsurv_res,         &
       icabsurv_prd, ica_n, ica_f, ica_tmp, icadiag_out, ica_vc,    &
       ica_shr, stock_mci, recruits_mci, meanf_mci, yield_mci,      &
       stock_pbi, recruits_pbi, meanf_pbi, yield_pbi, ica_log, ica_mc

      parameter (ica_sen = 'ica.sen')  ! Aberdeen sensitivity analysis file
      parameter (ica_sum = 'ica.sum')  ! Aberdeen summary file
      parameter (ica_vie = 'ica.vie')   ! input file for ICAVIEW
      parameter (ica_out = 'ica.out')   ! Main results file
      parameter (icacn_prd = 'icacn.prd') ! fitted values for the catches at age
      parameter (icacn_res = 'icacn.res') ! residuals for the catches at age
      parameter (icaasurv_prd = 'icaasurv.prd') ! fitted values for the aged surveys 
      parameter (icaasurv_res = 'icaasurv.res') ! residuals values for the aged surveys 
      parameter (icabsurv_prd = 'icabsurv.prd') ! fitted values for the SSB surveys 
      parameter (icabsurv_res = 'icabsurv.res') ! residuals values for the SSB surveys 
      parameter (ica_n = 'ica.n') ! Population abundances
      parameter (ica_f = 'ica.f') ! Fishing mortality
      parameter (ica_tmp = 'ica.tmp') ! input file for ICPROJ
      parameter (icadiag_out = 'icadiag.out') ! diagnostics of survey divergence
      parameter (ica_shr = 'ica.shr') ! file to write the shrunk VPA
      parameter (ica_vc = 'ica.vc') ! variance covariance matrix
      parameter (ica_log = 'ica.log') ! screen output is copied to here


      ! files for the MC parametric bootstrap iterations
                                                     
      parameter (stock_mci ='stock.mci')           !  These hold the MC iterations
      parameter (recruits_mci ='recruits.mci')     !
      parameter (meanf_mci ='meanf.mci')           !
      parameter (yield_mci ='yield.mci')           !

      parameter (ica_mc ='ica.mc')                 ! MCMC file, holds the sampled parameters

      parameter (stock_pbi ='stock.pbi')           !   These hold the percentiles of the
      parameter (recruits_pbi ='recruits.pbi')     !      distributions
      parameter (meanf_pbi ='meanf.pbi')           !
      parameter (yield_pbi ='yield.pbi')           !



