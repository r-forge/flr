#define ADOLC TAPELESS
#include <adouble.h>
#include "fwdFLStock.hpp"
#include "fwd.hpp"
#include "float.h"

//#include <iostream>
//using namespace std;

double getVal(FLRConst_Target quantity, FLStock &stk,  int iyr, int iunit, int iseason, int iarea, int iter)
   {
   double value=0.0;

   switch(quantity){
       case FLRConst_F:
         value = stk.Fbar(           iyr,  iunit, iseason, iarea, iter);
         break;
       case FLRConst_Z:
         value = stk.Zbar(           iyr,  iunit, iseason, iarea, iter);
         break;
       case FLRConst_FLandings:
         value = stk.FbarLandings(   iyr,  iunit, iseason, iarea, iter);
         break;
       case FLRConst_FDiscards:
         value = stk.FbarDiscards(   iyr,  iunit, iseason, iarea, iter);
         break;
       case FLRConst_SSB:
         value = SSB(stk,            iyr-1,iunit, iseason, iarea, iter);
         break;
       case FLRConst_Biomass:
         value = stk.computeStock(   iyr+1,iunit, iseason, iarea, iter);
         break;
       case FLRConst_MnSz:
         value = stk.computeMnSz(    iyr,  iunit, iseason, iarea, iter);
         break;
       case FLRConst_Catch:
         value = stk.computeCatch(   iyr,  iunit, iseason, iarea, iter);
         break;
       case FLRConst_Landings:
         value = stk.computeLandings(iyr,  iunit, iseason, iarea, iter);
         break;
       case FLRConst_Discards:
         value = stk.computeDiscards(iyr,  iunit, iseason, iarea, iter);
         break;
       }

   return value;
   }

void project(adouble *x, adouble *func, FLStock &stk, sr &sr, double *Trgt, int iTrgt, int nrow, double *Ary, int iter)
   {
   iTrgt--;

   int iyr = (int)(Trgt)[iTrgt],
       rel = stk.minyr-1;

   if (!R_IsNA((Trgt)[iTrgt+fwdTargetPos_rel*nrow]))  
       rel = (int)(Trgt)[iTrgt+fwdTargetPos_rel*nrow];

   double min = (Ary)[(iTrgt+0*nrow+3*nrow*(iter-1))];
   double val = (Ary)[(iTrgt+1*nrow+3*nrow*(iter-1))];
   double max = (Ary)[(iTrgt+2*nrow+3*nrow*(iter-1))];
   
   FLRConst_Target quantity = (FLRConst_Target)(int)(Trgt)[iTrgt + fwdTargetPos_quantity*nrow];
   
   int iunit  =1,
       iseason=1,
       iarea  =1;

   FLQuant_adolc ad_f(stk.harvest, iyr,   iyr,   iter);
   FLQuant_adolc ad_n(stk.stock_n, iyr+1, iyr+1, iter);
	
   for (int iage=stk.minquant; iage<=stk.maxquant; iage++)
      {
      ad_f(iage,iyr,iunit,iseason,iarea,iter) = stk.harvest(iage, iyr, iunit, iseason, iarea, iter)*x[0];
           
      //numbers-at-age next year
      if (iage < stk.maxquant){
		  ad_n(iage+1,iyr+1,iunit,iseason,iarea,iter) =stk.stock_n(iage,iyr,iunit,iseason,iarea,iter)*
                                                       exp(-ad_f(  iage,iyr,iunit,iseason,iarea,iter)-
                                                       stk.m(      iage,iyr,iunit,iseason,iarea,iter));}

      if (iage == stk.plusgrp)
         ad_n(iage,  iyr+1,iunit,iseason,iarea,iter) += stk.stock_n(stk.maxquant,iyr,iunit,iseason,iarea,iter)*
                                                        exp(-ad_f(  stk.maxquant,iyr,iunit,iseason,iarea,iter)-
                                                        stk.m(      stk.maxquant,iyr,iunit,iseason,iarea,iter));
      }
   

   int SSB_yr = __min(__max(iyr-stk.minquant+1,stk.minyr),stk.maxyr);
   
   ad_n(stk.minquant,iyr+1,iunit,iseason,iarea,iter) = sr.recruits(1,iyr+1,stk.SSB(SSB_yr,iunit,iseason,iarea,iter),iter);
      
   //-------------------- Target Stuff ----------------------//  
   //min & max bounds should only occur if a target calculated in a previous step for that year 
   // target value relative to reference year
   if (rel>=stk.minyr)
      { 
      double RelVal=getVal(quantity, stk, rel, iunit, iseason, iarea, iter);

      min=min*RelVal; 
      max=max*RelVal; 
      val=val*RelVal;
      }
   
   if (R_IsNA(val)) // max and min bounds 
      { 
      val=getVal(quantity, stk, iyr, iunit, iseason, iarea, iter);

      if (!R_IsNA(min) && val<min) val = min;
      if (!R_IsNA(max) && val>max) val = max;
      }
   
   //NLSE's
   switch (quantity)
       {
   	 case FLRConst_F:
	      func[0] = Fbar(            stk,     ad_f, iyr, iunit, iseason, iarea, iter) - val;
	      break;
   	 case FLRConst_Z:
	      func[0] = Zbar(            stk,     ad_f, iyr, iunit, iseason, iarea, iter) - val;
	      break;
   	 case FLRConst_FLandings:
	      func[0] = FbarLandings(    stk,     ad_f, iyr, iunit, iseason, iarea, iter) - val;
	      break;
   	 case FLRConst_FDiscards:
	      func[0] = FbarDiscards(    stk,     ad_f, iyr, iunit, iseason, iarea, iter) - val;
	      break;
    	 case FLRConst_SSB:
	      func[0] = SSB(             stk,ad_n,ad_f, iyr, iunit, iseason, iarea, iter) - val;
	      break;
   	 case FLRConst_Biomass:
	      func[0] = computeStock(    stk,ad_n,ad_f, iyr, iunit, iseason, iarea, iter) - val;
	      break;
	    case FLRConst_Catch:
	      func[0] = computeCatch(    stk,     ad_f, iyr, iunit, iseason, iarea, iter) - val;
	      break;
	    case FLRConst_Landings:
	      func[0] = computeLandings( stk,     ad_f, iyr, iunit, iseason, iarea, iter) - val;
	      break;
	    case FLRConst_Discards:
	      func[0] = computeDiscards( stk,     ad_f, iyr, iunit, iseason, iarea, iter) - val;
	      break;
	    default:
	      func[0] = 0.0;
	      break;
       }

  double value = func[0].value();
  } 

void project(double *x, FLStock &stk, sr &sr, int iyr, int iter, bool OnlyReplaceNA, bool OnlyCalcN)
   {  
   int iunit  =1,
       iseason=1,
       iarea  =1;

   int iage;
   for (iage=stk.minquant; iage<=stk.maxquant; iage++)
      {
      stk.harvest(iage,iyr,iunit,iseason,iarea,iter) = __max(0.0,stk.harvest(iage, iyr, iunit, iseason, iarea, iter)*x[0]);
      double _fbar = stk.Fbar(iyr,iunit,iseason,iarea,iter);

      if (_fbar>10.0)
          for (int iage=stk.minquant; iage<=stk.maxquant; iage++)
              stk.harvest(iage,iyr,iunit,iseason,iarea,iter)=stk.harvest(iage,iyr,iunit,iseason,iarea,iter)*10.0/_fbar; 
     
      //numbers-at-age next year
      if (iage < stk.maxquant)
         stk.stock_n(iage+1,iyr+1,iunit,iseason,iarea,iter)  = stk.stock_n(iage,iyr,iunit,iseason,iarea,iter)*exp(-stk.harvest(iage,iyr,iunit,iseason,iarea,iter)-stk.m(iage,iyr,iunit,iseason,iarea,iter));
      if (iage == stk.plusgrp)
         stk.stock_n(iage,  iyr+1,iunit,iseason,iarea,iter) += stk.stock_n( stk.maxquant,iyr,iunit,iseason,iarea,iter)*exp(-stk.harvest(stk.maxquant,iyr,iunit,iseason,iarea,iter)-stk.m(stk.maxquant,iyr,iunit,iseason,iarea,iter));
      }
  

   int SSB_yr = __min(__max(iyr-stk.minquant+1,stk.minyr),stk.maxyr);
      
   if (!OnlyReplaceNA || (OnlyReplaceNA && R_IsNA(stk.stock_n(stk.minquant,iyr+1,iunit,iseason,iarea,iter))))    
      stk.stock_n(stk.minquant,iyr+1,iunit,iseason,iarea,iter) = sr.recruits(1,iyr+1,stk.SSB(SSB_yr,iunit,iseason,iarea,iter),iter);

   if (!OnlyCalcN)
      for (iage=stk.minquant; iage<=stk.maxquant; iage++)
         {
         double z    =  stk.m(         iage, iyr, iunit, iseason, iarea, iter) + stk.harvest(   iage, iyr, iunit, iseason, iarea, iter),
                ctch =  stk.discards_n(iage, iyr, iunit, iseason, iarea, iter) + stk.landings_n(iage, iyr, iunit, iseason, iarea, iter);
         
         stk.discards_n( iage, iyr, iunit, iseason, iarea, iter)=stk.discards_n( iage, iyr, iunit, iseason, iarea, iter)/ctch;
         stk.landings_n( iage, iyr, iunit, iseason, iarea, iter)=stk.landings_n( iage, iyr, iunit, iseason, iarea, iter)/ctch;
          
         stk.catch_n( iage, iyr, iunit, iseason, iarea, iter) =
             stk.stock_n( iage, iyr, iunit, iseason, iarea, iter)*
             stk.harvest( iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z));

         stk.discards_n( iage, iyr, iunit, iseason, iarea, iter)=stk.discards_n( iage, iyr, iunit, iseason, iarea, iter)*stk.catch_n( iage, iyr, iunit, iseason, iarea, iter);
         stk.landings_n( iage, iyr, iunit, iseason, iarea, iter)=stk.landings_n( iage, iyr, iunit, iseason, iarea, iter)*stk.catch_n( iage, iyr, iunit, iseason, iarea, iter);
	      }
   } 

SEXP fwd_adolc_FLStock(SEXP xStk, SEXP xTrgt, SEXP xAry, SEXP xYrs, SEXP xSRModel,SEXP xSRParam,SEXP xSRResiduals,SEXP xMult)    
    {
	SEXP Err = PROTECT(NEW_NUMERIC(1)); 

    //Set Stock 
    FLStock stk(xStk); 

    //Set SRR
    sr sr;
	REAL(Err)[0]=1;
    if (!sr.Init(1, xYrs, stk.niters)) 
       return Err;
    REAL(Err)[0]=2;
    if (!sr.Init(1, xSRModel, xSRParam, xSRResiduals, xMult)) 
       return Err;
    
    // check target object
    REAL(Err)[0]=3;
    if (!isMatrix(xTrgt) || !isNumeric(xTrgt))
      return Err;

    // check target min/max/value object
    REAL(Err)[0]=4;
    if (!isArray(xAry) || !isNumeric(xAry))
      return Err;

    SEXP TrgtDims = GET_DIM(xTrgt);

    REAL(Err)[0]=5;
    if (LENGTH(TrgtDims) != 2 || INTEGER(TrgtDims)[1] != 9) 
       return Err;
  
    SEXP AryDims = GET_DIM(xAry);

    REAL(Err)[0]=6;
    if (LENGTH(AryDims) != 3 || INTEGER(AryDims)[0] != INTEGER(TrgtDims)[0] || 
                                INTEGER(AryDims)[1] != 3                    ||
                                INTEGER(AryDims)[2] != stk.niters) 
       return Err;

    double *Trgt = NUMERIC_POINTER(xTrgt);
    double *Ary  = NUMERIC_POINTER(xAry);

    //ADol-C stuff
    int i, n=1;

    double  *depen,    *indep,   *r, **jac;
    adouble *depen_ad, *indep_ad;
    
    depen    = new   double[n];
    indep    = new   double[n];
    r        = new   double[n];
    jac      = new  double*[n];
    depen_ad = new  adouble[n];
    indep_ad = new  adouble[n];
    
    for (i=0; i<n; i++)
       {
       jac[i]   = new double[n];
       indep[i] = 1.0;
       }    

    int iTrgt = 0, 
        iter  = 0,
        nrow  = (int)INTEGER(TrgtDims)[0];

    //get N at start of year
    for (iter=1; iter<=stk.niters; iter++)
       project(indep, stk, sr, sr.minyear()-1 ,iter, TRUE, TRUE);
    
    int iTape = 1, _Tape;
    for (iTrgt=1; iTrgt<=(int)(INTEGER(TrgtDims)[0]); iTrgt++)
       for (iter=1; iter<=stk.niters; iter++)
          {
          FLRConst_Target quantity = (FLRConst_Target)(int)(Trgt)[iTrgt-1 + fwdTargetPos_quantity*nrow];
          
          if (quantity ==999) //FLRConst_F)
              {
              int    iYr  =(int)(Trgt)[iTrgt-1 + 0*nrow];
              double _fbar=stk.Fbar(iYr,1,1,1,iter);

              double min_ = (Ary)[(iTrgt+fwdTargetPos_min*nrow+3*nrow*(iter-1))];
              double val  = (Ary)[(iTrgt+fwdTargetPos_val*nrow+3*nrow*(iter-1))];
              double max_ = (Ary)[(iTrgt+fwdTargetPos_max*nrow+3*nrow*(iter-1))];

              if (!R_IsNA((Trgt)[iTrgt+fwdTargetPos_rel*nrow]))  
                 {
                 int    rel   = (int)(Trgt)[iTrgt+fwdTargetPos_rel*nrow];
                 double RelVal=getVal(quantity, stk, rel, 1, 1, 1, iter);

                 min_ *= RelVal;
                 val  *= RelVal;
                 max_ *= RelVal;
                 }

              if (!R_IsNA(max_) && _fbar>max_) val=max_; else
              if (!R_IsNA(min_) && _fbar<min_) val=min_;

              val/=_fbar;

              project(&val,stk,sr,(int)(Trgt)[iTrgt-1 + 0*nrow], iter);
              }
          else
              {
              _Tape = 0; //iTrgt % ++iTape + 1;
          
              for (i=0; i<n; i++)
                 indep[i]=1.0;
    
               // Taping the computation of the jacobian 
               trace_on(_Tape);

               // marking independent variables 
               for (i=0; i<n; i++)
                  indep_ad[i] <<= indep[i];

              project(indep_ad,depen_ad,stk,sr,Trgt,iTrgt,nrow,Ary,iter);

              // marking dependent variables 
              for (i=0; i<n; i++)
                 depen_ad[i] >>= depen[i];

              trace_off(_Tape);

              //jacobian(tag,n,n,indep,jac);
              r[0]=1.0;
              function(_Tape,n,n,indep,r);
              int NIters=0;
	           while (norm(r,n) > 1e-12 && norm(indep,n) < 100 && NIters++<50)
	              {
	              jac_solv(_Tape,n,indep,r,0,2);

	              for (i=0; i<n; i++)
		              indep[i] -= r[i];	   

	              function(_Tape,n,n,indep,r);
                 }         
        
              project(indep, stk, sr, (int)(Trgt)[iTrgt-1 + 0*nrow], iter);
              }
           }

    delete[] depen;
    delete[] indep;
    delete[] r;
    delete[] depen_ad;
    delete[] indep_ad;
    
    for (i=0; i<n; i++)
       delete[] jac[i];
    delete[] jac;

    return stk.Return();
    }    

adouble computeStock(FLStock &stk, FLQuant_adolc &n, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   adouble val = 0.0;

   for (int iage= stk.minquant; iage<= stk.maxquant; iage++)
      {
      double mass;
      adouble deadeds;
     
         int _age = __min(iage+1,__max(stk.plusgrp,stk.maxquant));
         
         deadeds= exp(-stk.m(iage, iyr, iunit, iseason, iarea, iter)
                      -f(    iage, iyr, iunit, iseason, iarea, iter));
         
         mass   = stk.stock_wt(_age, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter);
      
      val += stk.stock_n(iage,iyr,iunit,iseason,iarea,iter)*deadeds*mass;
      }

   val +=            n(stk.minquant, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter)*
          stk.stock_wt(stk.minquant, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter);
      
   return val;
   }  

adouble SSB(FLStock &stk, FLQuant_adolc &n, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter) 
   {
   adouble val = 0.0, ssb = 0.0;

   for (int iage=stk.minquant; iage<=stk.maxquant; iage++)
      {
      adouble survivors;
        
      // if spawning before any fishing then project to end of year
      // but natural mortality might still occur
      if (stk.harvest_spwn(iage,iyr,iunit,iseason,iarea,iter) == 0)
         {
         double mass = stk.stock_wt(iage, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter)*
                       stk.mat(     iage, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter);
      
         survivors = exp(-stk.m(     iage, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter)*
                          stk.m_spwn(iage, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter));
         
         val = n(iage,iyr+1,iunit,iseason,iarea,iter)*mass*survivors;
         }
      else
         {
         double mass = stk.stock_wt(iage, iyr, iunit, iseason, iarea, iter)*
                       stk.mat(     iage, iyr, iunit, iseason, iarea, iter);
        
         survivors = exp(-stk.m(iage, iyr, iunit, iseason, iarea, iter)*stk.m_spwn(      iage, iyr, iunit, iseason, iarea, iter)
                         -f(    iage, iyr, iunit, iseason, iarea, iter)*stk.harvest_spwn(iage, iyr, iunit, iseason, iarea, iter));
           
         val = stk.stock_n(iage,iyr,iunit,iseason,iarea,iter)*mass*survivors;
         }

      if (val>0.0) ssb +=val;
      }

   return ssb;
   }  
                             
adouble computeCatch(FLStock &stk, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   adouble RtnVal = 0.0, 
           val    = 0.0;

double t1, t2;

   for (int iage= stk.minquant; iage<= stk.maxquant; iage++)
      {
      adouble z = stk.m(iage, iyr, iunit, iseason, iarea, iter) + f(iage, iyr, iunit, iseason, iarea, iter);

      val = stk.stock_n( iage, iyr, iunit, iseason, iarea, iter)*
            f(           iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z))*
            stk.catch_wt(iage, iyr, iunit, iseason, iarea, iter);

t1=val.value();
t2=RtnVal.value();

      RtnVal += val;
      }

   return RtnVal;
   } 

adouble computeDiscards(FLStock &stk, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   adouble val = 0.0;

   for (int iage= stk.minquant; iage<= stk.maxquant; iage++)
      {
      adouble z = stk.m(iage, iyr, iunit, iseason, iarea, iter) + f(iage, iyr, iunit, iseason, iarea, iter);

      val += stk.discards_n( iage, iyr, iunit, iseason, iarea, iter)/
   		   (stk.landings_n( iage, iyr, iunit, iseason, iarea, iter)+
             stk.discards_n( iage, iyr, iunit, iseason, iarea, iter))* 
             stk.stock_n(    iage, iyr, iunit, iseason, iarea, iter)*
             f(              iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z))*
             stk.discards_wt(iage, iyr, iunit, iseason, iarea, iter);
 
     }

   return val;
   } 

adouble computeLandings(FLStock &stk, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   adouble val = 0.0;

   for (int iage= stk.minquant; iage<= stk.maxquant; iage++)
      {
      adouble z = stk.m(iage, iyr, iunit, iseason, iarea, iter) + f(iage, iyr, iunit, iseason, iarea, iter);

      val += stk.landings_n( iage, iyr, iunit, iseason, iarea, iter)/
   		   (stk.landings_n( iage, iyr, iunit, iseason, iarea, iter)+
             stk.discards_n( iage, iyr, iunit, iseason, iarea, iter))* 
             stk.stock_n(    iage, iyr, iunit, iseason, iarea, iter)*
             f(              iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z))*
             stk.landings_wt(iage, iyr, iunit, iseason, iarea, iter);

double t=val.value(); 
     }

   return val;
   } 

adouble Fbar(FLStock &stk, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   adouble val = 0.0;

   for (int iage=stk.minfbar; iage<=stk.maxfbar; iage++)
      val += f(iage, iyr, iunit, iseason, iarea, iter);

   return val/(stk.maxfbar-stk.minfbar+1);
   }                               

adouble Zbar(FLStock &stk, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   adouble val = 0.0;

   for (int iage= stk.minfbar; iage<= stk.maxfbar; iage++)
      val += f(    iage, iyr, iunit, iseason, iarea, iter)+
             stk.m(iage, iyr, iunit, iseason, iarea, iter);

   return val/(stk.maxfbar-stk.minfbar+1);
   }                               

adouble FbarLandings(FLStock &stk, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   adouble val = 0.0;
double t;

   for (int iage= stk.minfbar; iage<= stk.maxfbar; iage++){
      val += f(             iage, iyr, iunit, iseason, iarea, iter)*
             stk.landings_n(iage, iyr, iunit, iseason, iarea, iter)/
             stk.catch_n(   iage, iyr, iunit, iseason, iarea, iter);
      
      t=val.value();
      }

   return val/(stk.maxfbar-stk.minfbar+1);
   }                               

adouble FbarDiscards(FLStock &stk, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   adouble val = 0.0;

   for (int iage= stk.minfbar; iage<= stk.maxfbar; iage++)
      val += f(             iage, iyr, iunit, iseason, iarea, iter)*
             stk.discards_n(iage, iyr, iunit, iseason, iarea, iter)/
             stk.catch_n(   iage, iyr, iunit, iseason, iarea, iter);

   return val/(stk.maxfbar-stk.minfbar+1);
   }   

double SSB(FLStock &stk, int iyr, int iunit, int iseason, int iarea, int iter) 
   {
   double val = 0.0, ssb = 0.0;

   for (int iage=stk.minquant; iage<=stk.maxquant; iage++)
      {
      double survivors;
        
      // if spawning before any fishing then project to end of year
      // but natural mortality might still occur
      if (stk.harvest_spwn(iage,iyr,iunit,iseason,iarea,iter) == 0)
         {
         double mass = stk.stock_wt(iage, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter)*
                       stk.mat(     iage, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter);
      
         survivors = exp(-stk.m(     iage, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter)*
                          stk.m_spwn(iage, __min(iyr+1,stk.maxyr), iunit, iseason, iarea, iter));
         
         val = stk.stock_n(iage,iyr+1,iunit,iseason,iarea,iter)*mass*survivors;
         }
      else
         {
         double mass = stk.stock_wt(iage, iyr, iunit, iseason, iarea, iter)*
                       stk.mat(     iage, iyr, iunit, iseason, iarea, iter);
        
         survivors = exp(-stk.m(      iage, iyr, iunit, iseason, iarea, iter)*stk.m_spwn(      iage, iyr, iunit, iseason, iarea, iter)
                         -stk.harvest(iage, iyr, iunit, iseason, iarea, iter)*stk.harvest_spwn(iage, iyr, iunit, iseason, iarea, iter));
           
         val = stk.stock_n(iage,iyr,iunit,iseason,iarea,iter)*mass*survivors;
         }

      if (val>0.0) ssb +=val;
      }

   return ssb;
   }  

