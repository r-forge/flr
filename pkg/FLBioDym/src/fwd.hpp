#ifndef _INC_fwd
#define _INC_fwd

#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "FLCoreClasses.hpp"
//#include "flc.hpp"

#ifdef WIN32
   #define SEXPDLLExport __declspec(dllexport) SEXP __cdecl
#else
   #define SEXPDLLExport SEXP
#endif

double sp(FLRConstBD model,  double stock,  double *params);
extern "C" SEXPDLLExport fwdFLBioDym(SEXP xBD, SEXP xCatch, SEXP xHarvest, SEXP xPE, SEXP xMult);

#endif /* _INC_fwd */

//class FLBioDym 
//{
//public:        
//
//    FLFLBioDym(void);      
//    FLFLBioDym(SEXP);      
//   ~FLFLBioDym(void);      
// 
//    void Init(SEXP);      
//    SEXP Return(void);
//    SEXP ReturnRange(void);      
// 
//   FLQuant catch_, 
//           stock, 
//           harvest; 
// 
//    int minyr,    maxyr,
//        nunits,
//        nseasons,
//        nareas,
//        niters;
// 
//    double sp(           int, int, int, int, int);
//    
// protected: 
//    bool InitFlag;
//    void unalloc(void);      
//    };

