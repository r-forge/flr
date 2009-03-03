/*
 * init.c = 
 *
 * Author : Iago Mosqueira <imosqueira@suk.azti.es> AZTI Tecnalia 
 * $Id$
 *
 */

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#ifdef WIN32
   #include <windows.h>
   #define SEXPDLLExport __declspec(dllexport) SEXP __cdecl    
#else
   #define SEXPDLLExport SEXP    
#endif

extern SEXPDLLExport Adolc_gr_tapeless(SEXP xX);
extern SEXPDLLExport ypr(SEXP xbrp, SEXP xSR);
extern SEXPDLLExport spr(SEXP xbrp, SEXP xSR);
extern SEXPDLLExport hcrYield(SEXP xbrp, SEXP xSR, SEXP xPar, SEXP xFbar);
extern SEXPDLLExport computeRefpts(SEXP xbrp, SEXP xref, SEXP xSR, SEXP xPar);
extern SEXPDLLExport brp(SEXP xbrp, SEXP xref, SEXP xSR, SEXP xPar);
extern SEXPDLLExport landings_n(SEXP xbrp, SEXP xSR, SEXP xPar);
extern SEXPDLLExport discards_n(SEXP xbrp, SEXP xSR, SEXP xPar);
extern SEXPDLLExport stock_n(SEXP xbrp, SEXP xSR, SEXP xPar);
extern SEXPDLLExport equilibrium(SEXP xbrp, SEXP xSR, SEXP xPar);

static const R_CallMethodDef callMethods[] = {
        {"Adolc_gr_tapeless", (DL_FUNC) &Adolc_gr_tapeless, 1},
        {"ypr", (DL_FUNC) &ypr, 3},
        {"spr", (DL_FUNC) &spr, 3},
        {"hcrYield", (DL_FUNC) &hcrYield, 4},
        {"computeRefpts", (DL_FUNC) &computeRefpts, 4},
        {"brp", (DL_FUNC) &brp, 4},
        {"landings_n", (DL_FUNC) &landings_n, 3},
        {"discards_n", (DL_FUNC) &discards_n, 3},
        {"stock_n", (DL_FUNC) &stock_n, 3},
        {"equilibrium", (DL_FUNC) &equilibrium, 3},
        {NULL, NULL, 0}
};

void
     R_init_foo(DllInfo *info)
     {
        R_registerRoutines(info, NULL, callMethods, NULL, NULL);
        R_useDynamicSymbols(info, FALSE);
        R_RegisterCCallable("FLBRP", "Adolc_gr_tapeless", (DL_FUNC) Adolc_gr_tapeless);
        R_RegisterCCallable("FLBRP", "ypr", (DL_FUNC) &ypr);
        R_RegisterCCallable("FLBRP", "spr", (DL_FUNC) &spr);
        R_RegisterCCallable("FLBRP", "hcrYield", (DL_FUNC) &hcrYield);
        R_RegisterCCallable("FLBRP", "computeRefpts", (DL_FUNC) &computeRefpts);
        R_RegisterCCallable("FLBRP", "brp", (DL_FUNC) &brp);
        R_RegisterCCallable("FLBRP", "landings_n", (DL_FUNC) &landings_n);
        R_RegisterCCallable("FLBRP", "discards_n", (DL_FUNC) &discards_n);
        R_RegisterCCallable("FLBRP", "stock_n", (DL_FUNC) &stock_n);
        R_RegisterCCallable("FLBRP", "equilibrium", (DL_FUNC) &equilibrium);
     }
