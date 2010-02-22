#ifndef _INC_FLVPA
#define _INC_FLVPA

#include <FLCoreClasses.hpp>
                           
#define VPA_TOL     1e-20
#define SEPVPA_TOL  1e-40
#define VPA_ITS     200     

#define _max(a,b) ((a)>(b)?(a):(b))
#define _min(a,b) ((a)<(b)?(a):(b))

class FLVPA
{
public:        
   bool InitFlag;

   FLVPA(SEXP);     
  ~FLVPA(void);
                                                        
   void Init(SEXP);

   bool isFLVPA(SEXP);

   bool    VPA(SEXP,SEXP,SEXP);
   bool    SepVPA(SEXP, SEXP);

   FLQuant  Catch,
            M,
            N,
            F;   

   SEXP Return(void);

protected: 
    int minage, maxage,  plusgrp,  
        minyr,  maxyr,
        nunits, nseasons, nareas, niters;

   bool FlagPlusGrp;

   inline double Calcf(double M, double Catch, double N, double N1)  {return (Catch - (1.0 - M/(log(N)-log(N1)))*(N-N1));}
   inline double Calcdfdx(double M, double N, double N1)             {return  (-1.0 -((log(N)-log(N1))*M -M*(N-N1)/N)/log(2*N/N1));}
   inline double NewtonRhapson(double x, double f, double dfdx)      {return (x - f/dfdx);}

   void FratioFunc(double FRatio, double F, double M, double M2, double C, double C2, double N, double *value, double *grad);
};


#endif /* _INC_FLVPA */
