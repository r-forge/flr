#ifndef _INC_fwdFLStock
#define _INC_fwdFLStock

#include "flc_adolc.hpp"
#include "flc.hpp"
//#include "FLCoreClasses_pointer.hpp"

double norm(double *, int);

double getVal(FLRConst_Target quantity, FLStock &stk,  int iyr, int iunit, int iseason, int iarea, int iter);

void project(adouble *x, adouble *func, sr &sr, double *Trgt, int iTrgt, int nrow, double *Ary, int iter);
void project(double  *x,                FLStock  &stk, sr &sr, int iyr, int iunit, int iseason, int iarea, int iter, bool OnlyReplaceNA=FALSE, bool OnlyCalcN=FALSE);

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

class fwdStk 
{
public:     
	fwdStk(void);
	~fwdStk(void);

    SEXP Init(SEXP xStk, SEXP xYrs, SEXP xSRModel,SEXP xSRParam,SEXP xSRResiduals,SEXP xMult,SEXP xAvail);    

	void InitAvail(SEXP x); 

	double getVal(FLRConst_Target quantity,  int iyr, int iunit, int iseason, int iarea, int iter);

	void project(double  *x,                int iyr, int iunit, int iseason, int iarea, int iter, bool OnlyReplaceNA=FALSE, bool OnlyCalcN=FALSE);
    void project(adouble *x, adouble *func, double *Trgt, int iTrgt, int nrow, double *Ary, int iter);

	adouble computeStock(   FLQuant_adolc &n, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter);      
	adouble SSB(            FLQuant_adolc &n, FLQuant_adolc &f, int iyr, int iunit, int iseason, int iarea, int iter);
	adouble computeCatch(   FLQuant_adolc &f,                   int iyr, int iunit, int iseason, int iarea, int iter);
	adouble computeDiscards(FLQuant_adolc &f,                   int iyr, int iunit, int iseason, int iarea, int iter);
	adouble computeLandings(FLQuant_adolc &f,                   int iyr, int iunit, int iseason, int iarea, int iter);
	adouble Zbar(           FLQuant_adolc &f,                   int iyr, int iunit, int iseason, int iarea, int iter);
	adouble Fbar(           FLQuant_adolc &f,                   int iyr, int iunit, int iseason, int iarea, int iter);
	adouble FbarLandings(   FLQuant_adolc &f,                   int iyr, int iunit, int iseason, int iarea, int iter);
	adouble FbarDiscards(   FLQuant_adolc &f,                   int iyr, int iunit, int iseason, int iarea, int iter);
	adouble MnSz(           FLQuant_adolc &n,                   int iyr, int iunit, int iseason, int iarea, int iter);

	double SSB(                                                 int iyr, int iunit, int iseason, int iarea, int iter);

    SEXP run(SEXP xTrgt, SEXP xAry) ;   
protected:        
	FLStock stk;
	FLQuant avail;

	sr sr;
    };

#endif /* _INC_fwdFLStock */



