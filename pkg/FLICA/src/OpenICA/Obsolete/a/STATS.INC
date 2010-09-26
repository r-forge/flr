                                                                                                                                    
! ------------------------STATS PARAMETERS-----------------------------                                                             
                                                                                                                                    
!                                                                                                                                   
!      This block holds estimates of variances, skewness, etc.                                                                      
!                                                                                                                                   
!                                                                                                                                   
      double precision CVar,Cchi2,CPchi,CCV,Cskew,CKurt                                                                             
      double precision Bvar(maxbsurv),Bchi2(maxbsurv),BPchi(maxbsurv),  &                                                           
       BCV(maxbsurv),BSkew(maxbsurv),BKurt(maxbsurv)                                                                                
      double precision Avar(maxsurvey,maxage),Achi2(maxsurvey,maxage),  &                                                           
       APchi(maxsurvey,maxage),ACV(maxsurvey,maxage),                   &                                                           
       ASkew(maxsurvey,maxage),AKurt(maxsurvey,maxage)                                                                              
      integer NoCdata, NoBdata(maxbsurv), NoAdata(maxsurvey,maxage)                                                                 
      double precision SRRVar                                                                                                       
      integer dfsep, NoSRRdata                                                                                                      
                                                                                                                                    
      COMMON /STATS / Bvar,Bchi2,BPchi,BCV,BSkew,BKurt,  &                                                           
       Cvar,CChi2,CPchi,Ccv,Cskew,CKurt,NoCdata,         &
       NoBdata, Avar,Achi2,APchi,ACV,ASkew,AKurt,        &                                                           
       NoAdata, SRRVar, NoSRRdata, dfsep                                                                                            
! ---------------------------------------------------------------------                                                             
                                                                                                                                    
!                                                                                                                                   
!    CVar, BVar, AVar - Variance of the catch-at-age observations, SSB surveys observations                                         
!                           and age-structured survey observations                                                                  
!    CChi2, BChi2, AChi2 -  Estimates of Chi-squared for each of the three data types                                               
!                                                                                                                                   
!    CPChi, BPchi, APChi -  Estimates of the probability of observing the corresponding chi-squared value                           
!                           given that the data conform to the model                                                                
!                                                                                                                                   
!    CSkew, CKurt, Bskew,                                                                                                           
!    BKurt, Askew, Akurt - Skewness and kurtosis estimates for the three data types                                                 
!                                                                                                                                   
!    NoCData, NoBdata, NoAdata - Number of observations for each data type                                                          
!                                                                                                                                   
!    CCV, BCV, ACV       - Coefficients of variation for the three data types                                                       
