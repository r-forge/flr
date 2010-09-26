                                                                                                                                    
! ---------  STOCK AND RECRUIT DATA BLOCK -----------------------------                                                             
                                                                                                                                    
      double precision A, B              ! Parameters of the Beverton and Holt SRR                                                  
      integer lag                        ! time lag between spawning and firstage                                                   
      logical FitSRR                     ! whether to fit a stock-recruit relation                                                  
      double precision SRRlambda         ! weight for the SRR term in the objective function                                        
      integer fychosen,lychosen          ! first and last years of choice for SRR relation

      COMMON /SRR /  A, B, FitSRR, lag, SRRlambda, fychosen, lychosen
