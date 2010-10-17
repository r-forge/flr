                                                                                                                                    
!                                                                                                                                   
!     This block holds the fitted values from the model fit.                                                                        
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
                                                                                                                                    
                                                                                                                                    
      double precision PredCN(maxyear,maxage )                                                                                      
      double precision PredAINDEX(maxsurvey,maxyear,maxage )                               ! Age structured survey data indexed as (
      double precision PredBINDEX(maxbsurv,maxyear)       ! SSB survey data indexed as (survey, year)                               
      double precision PredRecruit(maxyear)                                                                                         
                                                                                                                                    
      COMMON /PRED / PredCN, PredAindex, PredBindex, PredRecruit                                                                    
                                                                                                                                    
