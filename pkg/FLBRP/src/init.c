/*
 * init.c = 
 *
 * Author : Iago Mosqueira <imosqueira@suk.azti.es> AZTI Tecnalia 
 * $Id: $
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
extern SEXPDLLExport hcrYield(SEXP xbrp, SEXP xSR, SEXP xFbar);
extern SEXPDLLExport computeRefpts(SEXP xbrp, SEXP xref, SEXP xSR);
extern SEXPDLLExport brp(SEXP xbrp, SEXP xref, SEXP xSR);
extern SEXPDLLExport landings_n(SEXP xbrp, SEXP xSR);
extern SEXPDLLExport discards_n(SEXP xbrp, SEXP xSR);
extern SEXPDLLExport stock_n(SEXP xbrp, SEXP xSR);

static const R_CallMethodDef callMethods[] = {
        {"Adolc_gr_tapeless", (DL_FUNC) &Adolc_gr_tapeless, 1},
        {"ypr", (DL_FUNC) &ypr, 2},
        {"spr", (DL_FUNC) &spr, 2},
        {"hcrYield", (DL_FUNC) &hcrYield, 3},
        {"computeRefpts", (DL_FUNC) &computeRefpts, 3},
        {"brp", (DL_FUNC) &brp, 3},
        {"landings_n", (DL_FUNC) &landings_n, 2},
        {"discards_n", (DL_FUNC) &discards_n, 2},
        {"stock_n", (DL_FUNC) &stock_n, 2},
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
        R_RegisterCCallable("FLBRP", "brp", (DL_FUNC) &landings_n);
     }
