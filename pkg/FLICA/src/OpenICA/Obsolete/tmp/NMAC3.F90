                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
!      double precision FUNCTION RECIPE_RAN(IDUM)                                                                                    
!      THIS FUNCTION IS DEFINE IN stats.f90                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
! generates uniform random numbers U(0,1)                                                                                           
! uses 3 linear congruential generators and 'shuffles' #s                                                                           
! It's portable between machines with different precision                                                                           
                                                                                                                                    
!      implicit none                                                                                                                 
!      integer m1,m2,m3,ia1,ia2,ia3,ic1,ic2,ic3,iff,ix1,ix2,ix3                                                                      
!      integer idum,j                                                                                                                
!      double precision    rm1,rm2,r                                                                                                 
                                                                                                                                    
!      save                                                                                                                          
!      DIMENSION R(97)                                                                                                               
!      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247d-6)                                                                     
!      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773d-6)                                                                     
!      PARAMETER (M3=243000,IA3=4561,IC3=51349)                                                                                      
!      DATA IFF /0/                                                                                                                  
!      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN                                                                                               
!	IFF=1                                                                                                                              
!	IX1=MOD(IC1-IDUM,M1)                                                                                                               
!	IX1=MOD(IA1*IX1+IC1,M1)                                                                                                            
!	IX2=MOD(IX1,M2)                                                                                                                    
!	IX1=MOD(IA1*IX1+IC1,M1)                                                                                                            
!	IX3=MOD(IX1,M3)                                                                                                                    
!        DO  J=1,97                                                                                                                  
!	  IX1=MOD(IA1*IX1+IC1,M1)                                                                                                          
!	  IX2=MOD(IA2*IX2+IC2,M2)                                                                                                          
!          R(J)=(dble(IX1)+dble(IX2)*RM2)*RM1                                                                                        
!        enddo                                                                                                                       
!	IDUM=1                                                                                                                             
!      ENDIF                                                                                                                         
!      IX1=MOD(IA1*IX1+IC1,M1)                                                                                                       
!      IX2=MOD(IA2*IX2+IC2,M2)                                                                                                       
!      IX3=MOD(IA3*IX3+IC3,M3)                                                                                                       
!      J=1+(97*IX3)/M3                                                                                                               
!       IF(J.GT.97.OR.J.LT.1) then                                                                                                   
!       write(*,*) ' ERROR ENCOUNTERED IN RANDOM NUMBER GENERATOR'                                                                   
!       STOP                                                                                                                         
!       endif                                                                                                                        
!      RECIPE_RAN=R(J)                                                                                                               
!      R(J)=(dble(IX1)+dble(IX2)*RM2)*RM1                                                                                            
!      RETURN                                                                                                                        
!      END                                                                                                                           
                                                                                                                                
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
