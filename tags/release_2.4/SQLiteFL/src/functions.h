/*
 * $Id: functions.h 134 2009-03-03 14:25:24Z imosqueira $
 */

#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include "sqlite/sqlite3.h"

int checkFLCompTables(sqlite3 *db, const char *name);
int finalizeAllstmt(sqlite3 *db);
SEXP getSlotFLComp(sqlite3 *db, const char *name, const char *slot);
SEXP getMetaFLComp(sqlite3 *db, const char *name, const char *field);
SEXP getRangeFLComp(sqlite3 *db, const char *name);
SEXP getFromSlotFLComp(sqlite3 *db, const char *name, const char *slot, const char *select, int *dimSelect);
SEXP getRangeElement(SEXP range, const char *str);
