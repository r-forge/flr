/*
 * exports.h
 * SQLiteFL/src/exports.h
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas, UK
 * $Id$
 *
 */

#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include "sqlite/sqlite3.h"

/* insert.c */
SEXP insertFLQ(SEXP Rname, SEXP Rflq, SEXP Rdbname);
SEXP insertFLComp(SEXP Rname, SEXP Rflc, SEXP Rsnames, SEXP Rdbname);

/* update.c */
SEXP updateFLComp(SEXP Rdbname, SEXP Rname, SEXP Rstmt, SEXP Rvalue, SEXP Rdims,
    SEXP Ri, SEXP Rj, SEXP Rk, SEXP Rl, SEXP Rm, SEXP Rn);

/* select.c */
SEXP selectSlotFLComp(SEXP Rname, SEXP Rdbname, SEXP Rslot);
SEXP selectFLComp(SEXP Rname, SEXP Rdbname);
SEXP selectFromFLComp(SEXP Rname, SEXP Rdbname, SEXP Rselect);
SEXP selectFromSlotFLComp(SEXP Rdbname, SEXP Rname, SEXP Rslot, SEXP Rselect,
    SEXP RdimSelect, SEXP Rquant);

/* methods.c */
SEXP sqliteVersion(void);
SEXP summaryFLComp(SEXP Rname, SEXP Rdbname);
