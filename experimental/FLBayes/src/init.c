/*
 * init.c = 
 * SQLiteFL/src/init.c
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas
 * $Id: init.c 363 2009-10-20 12:40:39Z imosqueira $
 *
 */

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "exports.h"

static const R_CallMethodDef callMethods[] = {
        {"insertFLComp", (DL_FUNC) &insertFLComp, 4},
        {NULL, NULL, 0}
};

void
     R_init_foo(DllInfo *info)
     {
        R_registerRoutines(info, NULL, callMethods, NULL, NULL);
        R_useDynamicSymbols(info, FALSE);
        R_RegisterCCallable("SQLiteFL", "insertFLComp", (DL_FUNC) insertFLComp);
     }

