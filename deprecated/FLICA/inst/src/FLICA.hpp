#ifndef _INC_flica
#define _INC_flica

#ifdef WIN32
   #include <windows.h>
   #define SEXPDLLExport __declspec(dllexport) SEXP __cdecl    
   #define VOIDDLLExport __declspec(dllexport) VOID __stdcall    
#else
   #define SEXPDLLExport SEXP    
   #define VOIDDLLExport VOID
#endif

#include "Rdefines.h"
#include "Rinternals.h"
#include "FLICA_RIO.hpp"


extern "C" VOIDDLLExport ExecuteCallback(long cbAddress); 
extern "C" SEXPDLLExport FLVersion(void);
extern "C" SEXPDLLExport FLICA(SEXP xStock, SEXP xCPUE, SEXP xControl); 

bool InputStock(SEXP);
bool InputCPUE(SEXP);
bool InputControl(SEXP);
SEXP CreateFLICA(void);

extern "C" void ICAWRAPPER(void);
extern "C" void ICAWRAPPER2(void);
extern "C" void ICA_INIT(void);
extern "C" void ICA_STOCK_INPUT(void);
extern "C" void ICA_CPUE_INPUT(void);
extern "C" void ICA_CONTROL_INPUT(void);
extern "C" void ICA_RUN(void);
extern "C" void ICA_OUTPUT(void);
extern "C" void ICA_APPROXSRR(void);
 

#endif /* _INC_flica */
