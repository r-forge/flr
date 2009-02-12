/* VPA - «Short one line description»
   
   Author: Laurie Kell, CEFAS
   Maintainer: Laurie Kell
   Last Change: 02 May 2008 11:48
   $Id: FLVPA.cpp,v 1.3 2008/05/02 09:48:13 imosqueira Exp $ */


#ifdef WIN32
   #include <windows.h>
   #define SEXPDLLExport __declspec(dllexport) SEXP __cdecl    
#else
   #define SEXPDLLExport SEXP    
#endif

#include "FLVPA.hpp"
#include <R.h>

extern "C" SEXPDLLExport _FLVPA(SEXP xStock, SEXP xFitPlusGroup, SEXP xFratio, SEXP xFratioFlag)
   {
   //Input VPA Control
   FLVPA VPA(xStock);       
   
   //Run VPA
   VPA.VPA(xFitPlusGroup,xFratio,xFratioFlag);
                 
   //Display Ns and Fs 
   return VPA.Return();  
   }                 

extern "C" SEXPDLLExport FLSepVPA(SEXP xStock, SEXP xControl, SEXP xRefHarvest)
   {
   //Input VPA Control
   FLVPA VPA(xStock);       
   
   //Run Seperable VPA
   VPA.SepVPA(xControl, xRefHarvest);
                 
   //Display Ns and Fs 
   return VPA.Return();  
   }                 

//deriv((y ~ catch*(f+m)/(n*(1-exp(-f-m)))-f), c("f"), func = TRUE)
double func(double& f,double m, double c, double n)
    {
    double expr2 = c * (f + m);
    double expr5 = exp(-f - m);
    double expr7 = n * (1.0 - expr5);
    double value = expr2/expr7 - f;
    double grad  = c/expr7 - expr2 * (n * expr5)/pow(expr7,2.0) - 1.0;

    f-=value/grad;

    return(value);
    }
 
extern "C" SEXPDLLExport FLRCalcF(SEXP xM, SEXP xCatch, SEXP xN)
   {
   SEXP ReturnObject = R_NilValue;
  
   FLQuant m(xM);
   FLQuant c(xCatch);
   FLQuant n(xN);
   FLQuant f(xM);

   double _f = 0.1;

   int iAge, iYear, iUnit, iSeason, iArea, iIter;

   for (iIter = 1; iIter<=m.niters(); iIter++)
	   for (iArea = 1; iArea <= m.nareas(); iArea++)
		  for (iSeason = 1; iSeason <= m.nseasons(); iSeason++)
    		 for (iUnit = 1; iUnit <= m.nunits(); iUnit++)
      			for (iYear = m.minyr(); iYear <= m.maxyr(); iYear++)
		   		   for (iAge = m.minquant(); iAge <= m.maxquant(); iAge++)
						while (fabs(func(_f,m(iAge,iYear,iUnit,iSeason,iArea,iIter),c(iAge,iYear,iUnit,iSeason,iArea,iIter),n(iAge,iYear,iUnit,iSeason,iArea,iIter)))>1e-12)
							f(iAge,iYear,iUnit,iSeason,iArea,iIter)=_f;
					
   return f.Return();
   }

extern "C" SEXPDLLExport AdaptFunc(SEXP xStock, SEXP xFitPlusGroup, SEXP xFratio, SEXP xFratioFlag, SEXP xQ, SEXP xIndex)
   {
   //Input VPA Control
   FLVPA VPA(xStock);
       
   FLQuant  q(xQ); 
   FLQuant  index(xIndex);
 
   //Run VPA
   VPA.VPA(xFitPlusGroup,xFratio,xFratioFlag);

   //calculate objective function
   double ss = 0.0;
   int iage, iyr;   
   for (iage=q.minquant(); iage<=q.maxquant(); iage++)
     for (iyr=index.minyr(); iyr<=index.maxyr(); iyr++)
        {
        double _N = VPA.N(iage,iyr)*q(iage,q.minyr(),1,1,1,1); 
        ss += pow((index(iage,iyr)-_N)/_N,2.0);
        } 

   //end calculate objective function

   ss /= (q.minquant()-q.minquant()+1)*(index.maxquant()-index.minquant()+1);
   ss  = log(ss);

   SEXP RtnVal     = PROTECT(NEW_NUMERIC(1)); 
   REAL(RtnVal)[0] = ss;
 
   UNPROTECT(1);

   return RtnVal;
	}

FLVPA::FLVPA(SEXP x)
   {
   InitFlag    = false;
   
   if (isFLStock(x) && !InitFlag)
      Init(x);
   }

bool FLVPA::isFLVPA(SEXP x)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(x), 0));

   return strcmp(s, "FLVPA")==0;
   }

void FLVPA::Init(SEXP x)
   {

   SEXP      range = GET_SLOT(x, install("range"));
   SEXP RangeNames = GET_NAMES(range);
	  
   int n = length(RangeNames);

   minage   = maxage = 
   minyr    = maxyr  = 
   nunits   = 
   nseasons = 
   nareas   =
   niters   = 1;

   int i;

   for (i=0; i<n; i++)
      {
      const char *s = CHAR(VECTOR_ELT(RangeNames, i));

      if (      strcmp(s, "min")==0 || strcmp(s, "minage")==0 || strcmp(s, "minquant")==0)
         minage     = (short)(REAL(range)[i]);
      else  if (strcmp(s, "max")==0 || strcmp(s, "maxage")==0 || strcmp(s, "maxquant")==0)
         maxage     = (short)(REAL(range)[i]);
      else  if (strcmp(s, "plusgroup")==0)
         if (R_IsNA(REAL(range)[i]))
            FlagPlusGrp = false;
         else 
            {
            plusgrp     = (short)(REAL(range)[i]);
            FlagPlusGrp = true;
            }
        else  if (strcmp(s, "minyear")==0)
           minyr    = (short)(REAL(range)[i]);
        else  if (strcmp(s, "maxyear")==0)
           maxyr    = (short)(REAL(range)[i]);
      }

   Catch.Init(GET_SLOT(x, install("catch.n")));        
   M.Init(    GET_SLOT(x, install("m")));               
   F.Init(    GET_SLOT(x, install("harvest")));               
   N.Init(    GET_SLOT(x, install("stock.n")));               
   
   niters = M.niters();
   }

FLVPA::~FLVPA(void)      
   {
   ; //unalloc();
   }                               

SEXP FLVPA::Return(void)
   {
   SEXP x, Range;

   PROTECT(x = NEW_OBJECT(MAKE_CLASS("FLVPA")));
   Range     = PROTECT(NEW_NUMERIC(5)); 
   
   REAL(Range)[0] = minage;
   REAL(Range)[1] = maxage;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
       
   SET_SLOT(x, install("stock.n"), N.Return());
   SET_SLOT(x, install("harvest"), F.Return());
   SET_SLOT(x, install("catch.n"), Catch.Return());
   
   UNPROTECT(2);

   return x;
   }

bool FLVPA::SepVPA(SEXP xControl, SEXP xRefHarvest)
    {
    int iage, iyr, iyrcls, Iters =0, N_age, _maxage;
                                    
    double F1, Z, Z1, CumZ,
           Error,
           Error_reduction;
;   
    double *Selectivity, *OverallF, *CohortSize, **Residuals, **Observed, **H,
           *RsdlyrTotals, *RsdlageTotals;

    if (FlagPlusGrp) _maxage = maxage-1; else _maxage = maxage;

    Selectivity   =(double*)  malloc((_maxage -minage+1)*sizeof(double))  - minage;
    RsdlageTotals =(double*)  malloc((_maxage -minage+1)*sizeof(double))  - minage; 
    OverallF      =(double*)  malloc((maxyr  -minyr +1)*sizeof(double))  - minyr;
    RsdlyrTotals  =(double*)  malloc((maxyr  -minyr +1)*sizeof(double))  - minyr;
     
    CohortSize    =(double*)  malloc(((maxyr-minage) - (minyr-_maxage) + 1)*sizeof(double))  - (minyr-_maxage);
    
    Residuals     =(double**) malloc((_maxage -minage+1)*sizeof(double*)) - minage;
    Observed      =(double**) malloc((_maxage -minage+1)*sizeof(double*)) - minage;
    H             =(double**) malloc((_maxage -minage+1)*sizeof(double*)) - minage;
    
   for (int iter=1; iter<=niters; iter++)
      {
      for (iage=minage; iage <= _maxage; iage++)
        {
        Residuals[iage] =(double*) malloc((maxyr -minyr +1)*sizeof(double)) - minyr;
        Observed[iage]  =(double*) malloc((maxyr -minyr +1)*sizeof(double)) - minyr;
        H[iage]         =(double*) malloc((maxyr -minyr +1)*sizeof(double)) - minyr;
        }

      int    refage = (short) INTEGER(GET_SLOT(xControl,install("sep.age")))[0];
      double refsep = _max(REAL(GET_SLOT(xControl,install("sep.sel")))[0],0.0);
      double refF   = _max(REAL(xRefHarvest)[0],0.0);

      Selectivity[refage] = refsep;
      Selectivity[_maxage] = 1.0;
      OverallF[maxyr]     = refF;

      //Initialise
      for (iage=minage; iage < _maxage; iage++)
          Selectivity[iage] = Selectivity[refage];
      for (iyr=minyr; iyr < maxyr; iyr++)
          OverallF[iyr] = OverallF[maxyr];

      //Calc Log Catch Ratios Residuals
      for (iage=minage, Error = 0.0f; iage < _maxage; iage++)
         for (iyr=minyr; iyr < maxyr; iyr++)
            {
            Observed[iage][iyr]   = log(Catch(iage+1, iyr+1)/Catch(iage,   iyr,1,1,1,iter));

            F(iage,iyr,1,1,1,iter) = OverallF[iyr]*Selectivity[iage];
            F1          = OverallF[iyr+1]*Selectivity[iage+1];
            Z           = F(iage,iyr,1,1,1,iter) + M(iage,   iyr,1,1,1,iter);
            Z1          = F1 + M(iage+1, iyr+1);

            Residuals[iage][iyr]  = Observed[iage][iyr]
                                      - log(F1*Z*(1-exp(-Z1))*exp(-Z)/(F(iage,iyr,1,1,1,iter)*Z1*(1-exp(-Z))));

            Error += Residuals[iage][iyr]*Residuals[iage][iyr];
            }

      //Newton Rhapson to Solve for Overall F and Selectivity
      do  {
          //Sum Residuals by iyrs over iages
          for (iyr=minyr; iyr < maxyr; iyr++)
              for (iage=minage, RsdlyrTotals[iyr] = 0.0f; iage < _maxage; iage++)
                  RsdlyrTotals[iyr] += Residuals[iage][iyr];

          //Sum Residuals by iages over iyrs
          for (iage=minage; iage < _maxage; iage++)
              for (iyr=minyr, RsdlageTotals[iage] = 0.0f; iyr < maxyr; iyr++)
                  RsdlageTotals[iage] += Residuals[iage][iyr];

          //Calc New Overall F
          for (iyr=minyr; iyr < maxyr; iyr++)
              OverallF[iyr] /= exp(RsdlyrTotals[iyr] / (2.0f*(_maxage - minage + 1)));

          //Calc New Selectivity
          for (iage=minage; iage < _maxage; iage++)
              if (iage != refage)
                 Selectivity[iage] /= exp(RsdlageTotals[iage] / (2.0f*(maxyr - minyr + 1)));

          //Calc Log Catch Ratios Residuals
          for (iage=minage, Error_reduction = Error, Error = 0.0f; iage < _maxage; iage++)
              for (iyr=minyr; iyr < maxyr; iyr++)
                  {
                  F(iage,iyr,1,1,1,iter) = OverallF[iyr]*Selectivity[iage];
                  F1          = OverallF[iyr+1]*Selectivity[iage+1];
                  Z           = F(iage,iyr,1,1,1,iter) + M(iage,   iyr,1,1,1,iter);
                  Z1          = F1 + M(iage+1, iyr+1);

                  Residuals[iage][iyr]  = Observed[iage][iyr]
                                            - log(F1*Z*(1-exp(-Z1))*exp(-Z)/(F(iage,iyr,1,1,1,iter)*Z1*(1-exp(-Z))));

                  Error += Residuals[iage][iyr]*Residuals[iage][iyr];
                  }

          Error_reduction -=Error;
          Iters++;
          }
      while (Error_reduction >= 1.0e-20f && Iters < VPA_ITS);

      //Calc Cohort Size
      for (iyrcls  = minyr - _maxage; iyrcls <= maxyr  - minage; iyrcls++)
          {
          CohortSize[iyrcls] = 0.0f;
          N_age              = 0;
          CumZ               = 0.0f;

          for (iage  = _max(minage, minyr - iyrcls); iage <= _min(_maxage, maxyr - iyrcls); iage++)
              {
              Z = OverallF[iyrcls + iage]*Selectivity[iage] + M(iage, iyrcls + iage);
              H[iage][iyrcls+iage] =  OverallF[iyrcls + iage]*Selectivity[iage]/Z
                                          *(1.0f-exp(-1*Z))*exp(-CumZ);


              //OverallF[iyrcls + iage] = 1.0;
              //Selectivity[iage]       = 1.0;

              //H[iage][iyrcls+iage] = 1.0;

              CohortSize[iyrcls] += log(Catch(iage, iyrcls+iage)) - log(H[iage][iyrcls+iage]);
              CumZ += Z;
              N_age++;
              }
           CohortSize[iyrcls] = exp(CohortSize[iyrcls] / N_age);
           }

       //Calculate Fs and Ns
       for (iyrcls  = minyr - _maxage; iyrcls <= maxyr - minage;  iyrcls++)
          for (iage  = _max(minage, minyr - iyrcls); iage <= _min(_maxage, maxyr - iyrcls); iage++)
              {
              F(iage,iyrcls+iage) = OverallF[iyrcls + iage]*Selectivity[iage];
              if (iage == _max(minage, minyr - iyrcls))
                 N(iage,iyrcls+iage) = CohortSize[iyrcls];
              else
                 N(iage,iyrcls+iage) = (N(iage-1,iyrcls+iage-1)*exp(-F(iage-1,iyrcls+iage-1)-M(iage-1, iyrcls+iage-1)));
              }

       if (FlagPlusGrp)
          for (iyr=minyr; iyr <= maxyr; iyr++)
              {
              F(maxage, iyr,1,1,1,iter) = F(_maxage, iyr,1,1,1,iter);
              N(maxage, iyr,1,1,1,iter) = Catch(maxage,iyr,1,1,1,iter)*F(maxage,iyr,1,1,1,iter)/((F(maxage,iyr,1,1,1,iter)+M(maxage,iyr,1,1,1,iter)*exp(1.0-exp(-F(maxage,iyr,1,1,1,iter)-M(maxage, iyr,1,1,1,iter)))));
              }

        for (iyr  = minyr; iyr <= maxyr;  iyr++)
           for (iage  = minage; iage <= maxage; iage++)
              Catch(iage,iyr,1,1,1,iter) = N(iage,iyr,1,1,1,iter)*F(iage,iyr,1,1,1,iter)/(F(iage,iyr,1,1,1,iter)+M(iage, iyr,1,1,1,iter))*(1.0-exp(-F(iage,iyr,1,1,1,iter)-M(iage, iyr,1,1,1,iter)));
       }

      free((char * ) (Selectivity   +minage));
      free((char * ) (OverallF      +minyr));
      free((char * ) (RsdlageTotals +minage));
      free((char * ) (RsdlyrTotals  +minyr));
      free((char * ) (CohortSize    +(minyr-_maxage)));
                                    
      for (iage=minage; iage <= _maxage; iage++)
         {
         free((char*) (Residuals[iage] + minyr));
         free((char*) (Observed[ iage] + minyr));
         free((char*) (H[        iage] + minyr));
         }
      free((char * ) (Residuals+minage));
      free((char * ) (Observed +minage));
      free((char * ) (H        +minage));
      
      return TRUE;
    }                         

void FLVPA::FratioFunc(double FRatio, double F, double M, double M2, double C, double C2, double N, double *value, double *grad)
   {
   //deriv(~(F*(exp(F+M)-1.0)/(F+M))*(N-C2*(FRatio*F+M2)*(exp(-F*FRatio-M2))/(F*FRatio*(1-exp(-F*FRatio-M2)))) - C, "F")

    double expr1  = F + M;
    double expr2  = exp(expr1);
    double expr3  = expr2 - 1.0;
    double expr4  = F * expr3;
    double expr5  = expr4/expr1;
    double expr8  = C2 * (FRatio * F + M2);
    double expr12 = exp(-F * FRatio - M2);
    double expr13 = expr8 * expr12;
    double expr14 = F * FRatio;
    double expr15 = 1.0 - expr12;
    double expr16 = expr14 * expr15;
    double expr18 = N - expr13/expr16;
    double expr30 = expr12 * FRatio;
    *value  = expr5 * expr18 - C;
    *grad   = ((expr3 + F * expr2)/expr1 - expr4/(expr1*expr1)) * 
                expr18 - expr5 * ((C2 * FRatio * expr12 - expr8 * 
                expr30)/expr16 - expr13 * (FRatio * expr15 + expr14 * 
                expr30)/(expr16*expr16));
   }



bool FLVPA::VPA(SEXP xFitPlusGroup, SEXP xFRatio, SEXP xFRatioFlag)
   {
   short iage, iyr, iyrcls, Iters=0;
   double f, dfdx, Z, value, grad;         

   bool FlagFitPlusGrp;
  
   FLBool   FRatioFlag(xFRatioFlag);
   FLVector FRatio(    xFRatio);
   FlagFitPlusGrp =  (bool)LOGICAL(xFitPlusGroup)[0];

double t, t1;

   for (int iter=1; iter<=niters; iter++)
      {
      //last age
      for (iage = maxage; iage >= minage; iage--)
        {
        Z              = F(iage, maxyr,1,1,1,iter)+M(iage, maxyr,1,1,1,iter);
        t = N(iage, maxyr,1,1,1,iter);
        t1 = Catch(iage, maxyr,1,1,1,iter);
        N(iage, maxyr,1,1,1,iter) = Catch(iage, maxyr,1,1,1,iter)*Z/(F(iage, maxyr,1,1,1,iter)*(1.0-exp(-Z)));
        }

      for (iyr = maxyr; iyr >= minyr; iyr--)
        // Given terminal Fs, don't fit plus group
        if (!FRatioFlag(iyr) && !FlagFitPlusGrp)
           {
           Z                         = F(maxage, iyr,1,1,1,iter)+M(maxage, iyr,1,1,1,iter);
           N(maxage, iyr,1,1,1,iter) = Catch(maxage, iyr,1,1,1,iter)*Z/(F(maxage, iyr,1,1,1,iter)*(1.0-exp(-Z)));

           // Plus group F = last true age F or
           if (FlagPlusGrp)
              {
              F(maxage-1, iyr,1,1,1,iter) = F(maxage, iyr,1,1,1,iter);
              Z                           = F(maxage-1, iyr,1,1,1,iter)+M(maxage-1, iyr,1,1,1,iter);
              N(maxage-1, iyr,1,1,1,iter) = Catch(maxage-1, iyr,1,1,1,iter)*Z/(F(maxage-1, iyr,1,1,1,iter)*(1.0-exp(-Z)));
              }
           }
        // Terminal ages fitted using F ratio, with plusgroup
        else if (FRatioFlag(iyr) && FlagPlusGrp && iyr>minyr)
           {
           Z                             = F(maxage, iyr-1,1,1,1,iter) + M(maxage, iyr-1,1,1,1,iter);
           N(maxage,   iyr-1,1,1,1,iter) = Catch(maxage,iyr-1,1,1,1,iter)*Z/(F(maxage,iyr-1,1,1,1,iter)*(1.0-exp(-Z)));
           F(maxage-1, iyr-1,1,1,1,iter) = F(maxage-1, iyr,1,1,1,iter);

           Iters = 0;
           do
              {
              Iters++;

              FratioFunc(FRatio(iyr), F(maxage-1,iyr-1,1,1,1,iter), M(maxage-1,iyr-1,1,1,1,iter), M(maxage,iyr-1,1,1,1,iter), Catch(maxage-1,iyr-1,1,1,1,iter), Catch(maxage,iyr-1,1,1,1,iter), N(maxage,iyr,1,1,1,iter), &value, &grad);

              //Newton Rhapson
              F(maxage-1, iyr-1,1,1,1,iter) = F(maxage-1, iyr-1,1,1,1,iter) - value/grad;
              Z                  = F(maxage-1, iyr-1,1,1,1,iter) + M(maxage-1, iyr-1,1,1,1,iter);
              N(maxage-1, iyr-1,1,1,1,iter) = Catch(maxage-1, iyr-1,1,1,1,iter)*Z/(F(maxage-1, iyr-1,1,1,1,iter)*(1.0-exp(-Z)));
              }
           while (fabs(value) >= VPA_TOL && Iters <= VPA_ITS);


           F(maxage,   iyr-1,1,1,1,iter) = F(maxage-1, iyr-1,1,1,1,iter)*FRatio(iyr);
           Z                             = F(maxage, iyr-1,1,1,1,iter) + M(maxage, iyr-1,1,1,1,iter);
           N(maxage, iyr-1,1,1,1,iter)   = Catch(maxage, iyr-1,1,1,1,iter)*Z/(F(maxage, iyr-1,1,1,1,iter)*(1.0-exp(-Z)));
           }
        // Given terminal Fs, fitted plusgroup
        else if (!FRatioFlag(iyr) && FlagPlusGrp && iyr>minyr)
           {
           Iters = 0;

           double expr1, expr2, expr3, expr4, Z, PlusGrpN;

           Z                             = F(maxage, iyr-1,1,1,1,iter) + M(maxage, iyr-1,1,1,1,iter);
           N(maxage,   iyr-1,1,1,1,iter) = Catch(maxage,iyr-1,1,1,1,iter)*Z/(F(maxage,iyr-1,1,1,1,iter)*(1.0-exp(-Z)));
           F(maxage-1, iyr-1,1,1,1,iter) = F(maxage-1, iyr,1,1,1,iter);

           do
              {
              Iters++;

              PlusGrpN =N(maxage, iyr,1,1,1,iter) - N(maxage, iyr-1,1,1,1,iter)*exp(-F(maxage,iyr-1,1,1,1,iter)-M(maxage, iyr-1,1,1,1,iter));

              //deriv(~ Fval*(exp(Fval+Mval)-1.0)/(Fval+Mval)*PlusGrpN-Cval, "Fval")
              expr1 = F(maxage-1,iyr-1,1,1,1,iter) + M(maxage-1,iyr-1,1,1,1,iter);
              expr2 = exp(expr1);
              expr3 = expr2 - 1.0;
              expr4 = F(maxage-1,iyr-1,1,1,1,iter) * expr3;
              value = expr4/expr1 * PlusGrpN - Catch(maxage-1,iyr-1,1,1,1,iter);
              grad  = ((expr3 + F(maxage-1,iyr-1,1,1,1,iter) * expr2)/expr1 - expr4/(expr1*expr1)) * PlusGrpN;

              //Newton Rhapson
              F(maxage-1, iyr-1,1,1,1,iter) = (F(maxage-1, iyr-1,1,1,1,iter) - value/grad);
              Z                    = F(maxage-1, iyr-1,1,1,1,iter) + M(maxage-1, iyr-1,1,1,1,iter);
              N(maxage-1, iyr-1,1,1,1,iter) = Catch(maxage-1, iyr-1,1,1,1,iter)*Z/(F(maxage-1, iyr-1,1,1,1,iter)*(1.0-exp(-Z)));
              }
           while (fabs(value) >= VPA_TOL && Iters <= VPA_ITS);
           }
        // Terminal ages fitted using F ratio, plusgroup not fitted
        else if (FRatioFlag(iyr) && FlagPlusGrp && !FlagFitPlusGrp && iyr>minyr)
           {
           ;
           }

     //Now go for the year classes
     for (iyrcls = maxyr - minage - 1; iyrcls >= minyr - maxage - 1; iyrcls--)
        for (iage = _min(maxage - (FlagPlusGrp /*&& !FRatioFlag(iyrcls+iage)*/ ? 2 : 1), maxyr - iyrcls - 1); iage >= _max(minage, minyr - iyrcls); iage--)
           {
           iyr = iyrcls+iage;

           Iters = 0;

           double t = N(iage+1,iyr+1,1,1,1,iter)*exp(M(iage, iyr,1,1,1,iter));
           N(iage,iyr,1,1,1,iter) = _max(0.0, t + Catch(iage, iyr,1,1,1,iter) * exp(0.5*M(iage, iyr,1,1,1,iter)));

           do
              {
              Iters++;
              //do Newton Raphson to estimate N
              f    = Calcf(M(iage, iyr,1,1,1,iter), Catch(iage, iyr,1,1,1,iter), N(iage,iyr,1,1,1,iter), N(iage+1,iyr+1,1,1,1,iter));
              dfdx = Calcdfdx(M(iage, iyr,1,1,1,iter), N(iage,iyr,1,1,1,iter), N(iage+1,iyr+1,1,1,1,iter));

              //calc N
              N(iage,iyr,1,1,1,iter) = NewtonRhapson(N(iage,iyr,1,1,1,iter), f, dfdx);
              }
           while (fabs(f) >= VPA_TOL && Iters <= VPA_ITS);

           //calc F at iage
           F(iage,iyr,1,1,1,iter) =  _max(0.0,-log(N(iage+1,iyr+1,1,1,1,iter)/N(iage,iyr,1,1,1,iter)) - M(iage, iyr,1,1,1,iter));
           }
        }
        
   return TRUE;   
   }   

 extern "C" SEXPDLLExport meanQuant(SEXP x, SEXP xNYrs, SEXP xMeanNYrs, SEXP xArith, SEXP xNA_rm)
   {
   FLQuant I(x);       
   FLQuant O;  
     
   //return(x);

   //return I.Return();

   int NYrs    = (int)REAL(xNYrs)[0],
       Mn_NYrs = (int)REAL(xMeanNYrs)[0];
   
   bool NA_rm  = (bool)LOGICAL(xArith)[0],
        Arith  = (bool)LOGICAL(xNA_rm)[0];

   O.Init(I.minquant(), I.maxquant(), I.minyr(), I.maxyr()+NYrs, I.nunits(), I.nseasons(), I.nareas(), I.niters());

   int iAge, iYear, iUnit, iSeason, iArea, iIter;

   for (iAge = O.minquant(); iAge <= O.maxquant(); iAge++)
      for (iUnit = 1; iUnit <= O.nunits(); iUnit++)
	      for (iSeason = 1; iSeason <= O.nseasons(); iSeason++)
            for (iArea = 1; iArea <= O.nareas(); iArea++)
               for(iIter = 1; iIter <= O.niters(); iIter++)
    		         {
                  //fill up early yaers
	   		      for (iYear = I.minyr(); iYear <= I.maxyr(); iYear++)
	   		         O(iAge,iYear,iUnit,iSeason,iArea,iIter) = I(iAge,iYear,iUnit,iSeason,iArea,iIter);
                  
                  //calcmean
                  int n      = 0;
                  double val = 0.0;
                  for (iYear = I.maxyr()-Mn_NYrs+1; iYear <= I.maxyr(); iYear++)
                     if (!NA_rm || !R_IsNA(I(iAge,iYear,iUnit,iSeason,iArea,iIter)))
                        {
                        n++; 
	   		            val += I(iAge,iYear,iUnit,iSeason,iArea,iIter);
                        }
 
                  //Fill future
                  for (iYear = I.maxyr()+1; iYear <= I.maxyr()+NYrs; iYear++)
	   		         O(iAge,iYear,iUnit,iSeason,iArea,iIter) = n>1? val/n: R_NaN;
                  }
                
   return O.Return();  
   }                 
