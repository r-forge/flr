! //////////////////////////////////////////////////////////////////////
            
      double precision FUNCTION RECIPE_RAN(IDUM)    
            
! ///////////////////////////////////////////////////////////////////////                   
            
! generates uniform random numbers U(0,1)           
! uses 3 linear congruential generators and 'shuffles' #s               
! It's portable between machines with different precision               
            
      implicit none             
      integer m1,m2,m3,ia1,ia2,ia3,ic1,ic2,ic3,iff,ix1,ix2,ix3          
      integer idum,j            
      double precision    rm1,rm2,r                 
            
      save  
      DIMENSION R(97)           
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247d-6)         
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773d-6)         
      PARAMETER (M3=243000,IA3=4561,IC3=51349)      
      DATA IFF /0/              
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN               
	IFF=1      
	IX1=MOD(IC1-IDUM,M1)           
	IX1=MOD(IA1*IX1+IC1,M1)        
	IX2=MOD(IX1,M2)                
	IX1=MOD(IA1*IX1+IC1,M1)        
	IX3=MOD(IX1,M3)                
        DO  J=1,97              
	  IX1=MOD(IA1*IX1+IC1,M1)      
	  IX2=MOD(IA2*IX2+IC2,M2)      
          R(J)=(dble(IX1)+dble(IX2)*RM2)*RM1        
        enddo                   
	IDUM=1     
      ENDIF 
      IX1=MOD(IA1*IX1+IC1,M1)   
      IX2=MOD(IA2*IX2+IC2,M2)   
      IX3=MOD(IA3*IX3+IC3,M3)   
      J=1+(97*IX3)/M3           
       IF(J.GT.97.OR.J.LT.1) then                   
       write(*,*) ' ERROR ENCOUNTERED IN RANDOM NUMBER GENERATOR'       
       STOP 
       endif
      RECIPE_RAN=R(J)           
      R(J)=(dble(IX1)+dble(IX2)*RM2)*RM1            
      RETURN
      END function recipe_ran 
            
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                 
            
