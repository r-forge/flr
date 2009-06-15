#ifndef _INC_fwdFLStock
#define _INC_fwdFLStock

#include "flc_adolc.hpp"
#include "flc.hpp"
#include "FLCoreClasses_pointer.hpp"

double norm(double *, int);

double getVal(FLRConst_Target quantity, FLStock &stk,  int iyr, int iunit, int iseason, int iarea, int iter);

void project(adouble *x, adouble *func, FLStock  &stk, sr &sr, double *Trgt, int iTrgt, int nrow, int iter=1);
void project(double  *x,                FLStock  &stk, sr &sr, int nrow, int iter=1, bool OnlyReplaceNA=FALSE, bool OnlyCalcN=FALSE);

adouble computeStock(   FLStock &stk, FLQuant_adolc &n, FLQuant_adolc &f, int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);      
adouble SSB(            FLStock &stk, FLQuant_adolc &n, FLQuant_adolc &f, int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);
adouble computeCatch(   FLStock &stk, FLQuant_adolc &f,                   int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);
adouble computeDiscards(FLStock &stk, FLQuant_adolc &f,                   int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);
adouble computeLandings(FLStock &stk, FLQuant_adolc &f,                   int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);
adouble Zbar(           FLStock &stk, FLQuant_adolc &f,                   int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);
adouble Fbar(           FLStock &stk, FLQuant_adolc &f,                   int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);
adouble FbarLandings(   FLStock &stk, FLQuant_adolc &f,                   int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);
adouble FbarDiscards(   FLStock &stk, FLQuant_adolc &f,                   int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);
adouble MnSz(           FLStock &stk, FLQuant_adolc &n,                   int iyr, int iunit=1, int iseason=1, int iarea=1, int iter=1);

double SSB(         FLStock &stk, int iyr, int iunit, int iseason, int iarea, int iter);

SEXP  fwd_adolc_FLStock(SEXP xStk,             SEXP xTrgt,SEXP xAry,SEXP xYrs,SEXP xSRModel,SEXP xSRParam,SEXP xSRResiduals,SEXP xMult);    
SEXP _fwd_adolc_FLBiol( SEXP xBiol,SEXP xFleet,SEXP xTrgt,SEXP xAry,SEXP xYrs,SEXP xSRModel,SEXP xSRParam,SEXP xSRResiduals,SEXP xMult);    

#endif /* _INC_fwdFLStock */



