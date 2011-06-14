#ifndef _INC_flrica_io
#define _INC_flrica_io

const int DIMS_NAges     =  20;	    // maximum age
const int DIMS_NYrs      = 100;	    // maximum no of years 
const int DIMS_NSurveys  =  20;
const int DIMS_NBSurveys =   2;
const int DIMS_NParm     = 150;

#include "Rdefines.h"
#include "Rinternals.h"
#include "definitionsICA.hpp"


//CPUE Flags
typedef struct 
   {
   short  n;
   bool   Use[DIMS_NSurveys+DIMS_NBSurveys];
   bool   Biomass[DIMS_NSurveys+DIMS_NBSurveys];
   bool   PlusGroup[DIMS_NSurveys+DIMS_NBSurveys];
   double Cor[DIMS_NSurveys+DIMS_NBSurveys];
   char   Type[DIMS_NSurveys+DIMS_NBSurveys];
   char   Pos[DIMS_NSurveys+DIMS_NBSurveys];
   }FLTypeCPUEFlags;

typedef enum tagConstSel 
	{
   FLcKnown        = 1,
   FLcPartialCatch = 2,
   FLcCPUEbyAge    = 3
	} FLConstSel;

typedef enum tagConstUnits 
	{
   FLcBiomass = 1,
   FLcNumber  = 2
	} FLConstUnits;


//Matrix FLQuant Input
bool         InputFLQuantY(SEXP, double [DIMS_NYrs], int , int );
bool         InputFLQuantAY(SEXP, double [DIMS_NAges][DIMS_NYrs], int , int , int , int );
bool         InputFLQuantAY(SEXP, double [DIMS_NAges][DIMS_NYrs], int *, int *, int *, int *);
double       InputFLQuantMeanVal(SEXP);

//Matrix FLQuant Output
SEXP         CreateFLQuantY( double [DIMS_NYrs], int , int );
SEXP         CreateFLQuantAY(double [DIMS_NAges][DIMS_NYrs], int , int , int , int );

//General R stuff
int          NElemList(SEXP);

//Get Types and/or flags
short        GetCPUEType(SEXP);
FLConstUnits GetType(SEXP);
short        GetCPUEModel(SEXP);
bool         IsBiomassIndex(SEXP, int i);

//FR Class IO
bool         InputStock(char *);
bool         InputCPUE(SEXP, SEXP, FLTypeCPUEFlags &);
bool         InputControl(SEXP, FLTypeCPUEFlags &);
SEXP         CreateFLICA(SEXP, FLTypeCPUEFlags &);
SEXP         ReturnControl(FLTypeCPUEFlags &);
SEXP         ReturnIndex(   short,FLTypeCPUEFlags &);
SEXP         ReturnIndexRes(short,FLTypeCPUEFlags &);
SEXP         ReturnIndexHat(short,FLTypeCPUEFlags &);
void         ReturnDiagnostics(SEXP *);

//Check classes
bool isFLQuant(SEXP);
bool isFLICA(SEXP);
bool isFLICAControl(SEXP);
bool isFLStock(SEXP);
bool isFLStocks(SEXP);
bool isFLCPUE(SEXP);
bool isFLCPUEs(SEXP);

//Fortran
extern "C" bool PARMINIT(void);

#endif /* _INC_flica_rio */
