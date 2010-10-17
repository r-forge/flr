                                                                                                                                    
                                                                                                                                    
!     ------------------------ CONTROL  BLOCK------------------                                                                     
                                                                                                                                    
!                                                                                                                                   
!     This block contains parameters and conctrol flags for                                                                         
!     fitting the separable model                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      integer maxSep                                                                                                                
      parameter (MaxSep =15)                                                                                                        
                                                                                                                                    
      logical UseSep, writeout, Full                           ! UseSep - (not used)                                                
                                                               ! writeout - whether or not the objective function                   
                                                               !                                                                            should write out the residuals to disk
                                                               ! Full - whether or not the objective funtion recalculates the whole 
                                                               !         VPA, or only as far back as is currently required          
      integer NySep, RefAge, Nxdata, Nxparm                                                                                         
                                                               ! NySep - Number of years for separable model                        
                                                               ! RefAge - Reference age for fitting the separable model             
                                                               ! NxData - Number of observations                                    
                                                               ! NxParm - Number of parameters                                      
                                                                                                                                    
      integer HiFage, LoFage                                   ! Lowest and highest ages for calcualting arithmetic mean F, for     
                                                               !      output table only                                             
                                                                                                                                    
      double precision RelWt(maxage), YearWt(maxyear), TermS            ! Relative weights by age and year; selection on last true a
                                                               ! reference age                                                      
      double precision Xbest(maxparm), Xlow(maxparm),                   &                                                           
       Xhigh(maxparm)          ! Parameter list +/- standard deviation (all parameters are natural logs of F, S, N, Q, etc.)        
                                                                                                                                    
      double precision W(maxyear,maxage), ALambda(maxsurvey,maxage)     &                                                           
       , BLambda(maxbsurv)     ! Weighting matrices for catch-at-age; for aged surveys; for SSB surveys                                    
                                                                                                                                    
      logical TwoSel    ! whether to have 2 selection patterns                                                                      
      logical StepSel   ! whether change in selection is step or gradual                                                            
      integer ChangeSel ! last year for constant selection, if 2 sel patterns chosen                                                
      double precision TermS2                                                                                                       
                                                                                                                                    
       COMMON /SEPMODEL / RelWt, YearWt, Xbest,Xlow,Xhigh, W, TermS,TermS2,ALambda,BLambda, &                                              UseSep,NySep, RefAge, writeout,Full,     &                                                           
       HiFage,LoFage, TwoSel, ChangeSel, StepSel, Nxdata,Nxparm                                          

!     -----------------------------------------------------------------                                                             
