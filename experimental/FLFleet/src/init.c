/*
 * init.c = 
 * SQLiteFL/src/init.c
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas
 * $Id: init.c 913 2011-03-22 10:20:01Z imosqueira $
 *
 */

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "exports.h"

static const R_CallMethodDef callMethods[] = {
        {"asFLQuant", (DL_FUNC) &asFLQuant, 2},
        {NULL, NULL, 0}
};

void
     R_init_foo(DllInfo *info)
     {
        R_registerRoutines(info, NULL, callMethods, NULL, NULL);
        R_useDynamicSymbols(info, FALSE);
        R_RegisterCCallable("FLFLeet", "asFLQuant", (DL_FUNC) asFLQuant);
     }

