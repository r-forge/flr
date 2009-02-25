/*
 * init.c = 
 * SQLiteFL/src/init.c
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas
 * $Id: init.c,v 1.4 2009/02/20 15:44:39 imosqueira Exp $
 *
 */

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "exports.h"

static const R_CallMethodDef callMethods[] = {
        {"insertFLComp", (DL_FUNC) &insertFLComp, 4},
        {"updateFLComp", (DL_FUNC) &updateFLComp, 11},
        {"selectFLComp", (DL_FUNC) &selectFLComp, 2},
        {"selectSlotFLComp", (DL_FUNC) &selectSlotFLComp, 3},
        {"sqliteVersion", (DL_FUNC) &sqliteVersion, 0},
        {NULL, NULL, 0}
};

void
     R_init_foo(DllInfo *info)
     {
        R_registerRoutines(info, NULL, callMethods, NULL, NULL);
        R_useDynamicSymbols(info, FALSE);
        R_RegisterCCallable("SQLiteFL", "insertFLComp", (DL_FUNC) insertFLComp);
        R_RegisterCCallable("SQLiteFL", "updateFLComp", (DL_FUNC) updateFLComp);
        R_RegisterCCallable("SQLiteFL", "selectFLComp", (DL_FUNC) selectFLComp);
        R_RegisterCCallable("SQLiteFL", "selectSlotFLComp", (DL_FUNC) selectSlotFLComp);
        R_RegisterCCallable("SQLiteFL", "sqliteVersion", (DL_FUNC) sqliteVersion);
     }

