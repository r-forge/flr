/*
 * methods.c
 * SQLiteFL/src/methods.c
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas, UK
 * $Id: methods.c,v 1.12 2009/01/09 15:26:26 imosqueira Exp $
 *
 */

#include <string.h>
#include "exports.h"
#include "functions.h"

/* Function SEXP sqliteVersion(void) {{{ */
SEXP sqliteVersion(void)
{
  int len;
  SEXP Res;
  len = strlen(sqlite3_libversion());
  PROTECT(Res = allocVector(STRSXP, 1));
  SET_STRING_ELT(Res, 0, mkChar(sqlite3_libversion()));
  UNPROTECT(1);
  return(Res);
} /* }}} */

/* Function SEXP summaryFLComp(SEXP Rdbname, SEXP Rname) {{{ */
SEXP summaryFLComp(SEXP Rdbname, SEXP Rname)
{
	SEXP Rval, Range, NamesR;
	PROTECT(Rval = allocVector(INTSXP, 1));

  int rc, i;
  sqlite3 *db;
  sqlite3_stmt *stmt;
  const char *tail;
  char *sql;

  INTEGER(Rval)[0] = 1;

  /* OPEN */
  rc = sqlite3_open(CHAR(STRING_ELT(Rdbname, 0)), &db);
  /* Can db be opened? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    UNPROTECT(2);
    return (Rval);
  }

  /* GET class of object */
  sql = sqlite3_mprintf("SELECT value FROM %q_meta WHERE field = 'class';", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(1);
    return (Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(1);
    return (Rval);
  }
  /* PRINT */
  Rprintf("%s%s%s", "An object of class \"", sqlite3_column_text(stmt, 0), "\" ");
  Rprintf("%s%s%s\n\n", "stored in database \"", CHAR(STRING_ELT(Rdbname, 0)), "\"");

  /* PRINT name */
  Rprintf("Name: %s\n", CHAR(STRING_ELT(getMetaFLComp(db, CHAR(STRING_ELT(Rname, 0)), "name"), 0)));
  
  /* PRINT desc */
  Rprintf("Description: %s\n", CHAR(STRING_ELT(getMetaFLComp(db, CHAR(STRING_ELT(Rname, 0)), "desc"), 0)));
  
  /* PRINT range */
  Range = getRangeFLComp(db, CHAR(STRING_ELT(Rname, 0)));
  NamesR = GET_NAMES(Range);
  Rprintf("%s\t", "Range:");
  for (i=0; i < GET_LENGTH(Range); i++) {
    if(strcmp(CHAR(STRING_ELT(NamesR, i)), "plusgroup") == 0)
      Rprintf("%s\t", "pgroup");
    else
      Rprintf("%s\t", CHAR(STRING_ELT(NamesR, i)));
  }
  Rprintf("\n");
  Rprintf("\t");
  for (i=0; i < GET_LENGTH(Range); i++) {
    Rprintf("%.0f\t", REAL(Range)[i]);
  }
  Rprintf("\n");

  /* PRINT quant */
  Rprintf("Quant: %s\n\n", CHAR(STRING_ELT(getMetaFLComp(db, CHAR(STRING_ELT(Rname, 0)), "quant"), 0)));
  
  /* GET slot names */
  sql = sqlite3_mprintf("SELECT * FROM %q_slots;", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(1);
    return (Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(1);
    return (Rval);
  }
  while(rc == SQLITE_ROW) {
    /* PRINT slots in */
    Rprintf("%-14s: [ %i %i %i %i %i %i ], units = %s\n", sqlite3_column_text(stmt, 1), sqlite3_column_int(stmt, 3), sqlite3_column_int(stmt, 4), sqlite3_column_int(stmt, 5), sqlite3_column_int(stmt, 6), sqlite3_column_int(stmt, 7), sqlite3_column_int(stmt, 8), sqlite3_column_text(stmt, 2));
    rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(1);
      return (Rval);
    }
  }
  sqlite3_free(sql);
  
  /* FINALIZE stmt */
  rc = finalizeAllstmt(db);

  /* CLOSE */
  rc = sqlite3_close(db);
  if(rc != SQLITE_OK) {
    Rprintf("%s", sqlite3_errmsg(db));
  }

  INTEGER(Rval)[0] = 0;
	UNPROTECT(1);


	return (Rval);
} /* }}} */

/* Function SEXP dimsFLComp(SEXP Rdbname, SEXP Rname) {{{ */
SEXP dimsFLComp(SEXP Rdbname, SEXP Rname)
{
	SEXP Rval, Range, NamesR, Value;
	PROTECT(Rval = allocVector(VECSXP, 11));
	PROTECT(NamesR = allocVector(STRSXP, 11));
	PROTECT(Value = allocVector(INTSXP, 1));

  int rc;
  sqlite3 *db;
  sqlite3_stmt *stmt;
  const char *tail;
  char *sql;

  /* OPEN */
  rc = sqlite3_open(CHAR(STRING_ELT(Rdbname, 0)), &db);
  /* Can db be opened? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    UNPROTECT(2);
    return (Rval);
  }

  /* quant, min, max, (plusgroup), years, minyear, maxyear, unit, season, area, iter */
  /* quant */
  SET_VECTOR_ELT(Rval, 0, getMetaFLComp(db, CHAR(STRING_ELT(Rname, 0)), "quant"));
  SET_STRING_ELT(NamesR, 0, mkChar("quant"));

  /* use range for other dims */
  Range = getRangeFLComp(db, CHAR(STRING_ELT(Rname, 0)));
  
  /* min */
  SET_VECTOR_ELT(Rval, 1, getRangeElement(Range, "min"));
  SET_STRING_ELT(NamesR, 1, mkChar("min"));

  /* max */
  SET_VECTOR_ELT(Rval, 2, getRangeElement(Range, "max"));
  SET_STRING_ELT(NamesR, 2, mkChar("max"));

  /* plusgroup */
  SET_VECTOR_ELT(Rval, 3, getRangeElement(Range, "plusgroup"));
  SET_STRING_ELT(NamesR, 3, mkChar("plusgroup"));
  
  /* minyear */
  SET_VECTOR_ELT(Rval, 5, getRangeElement(Range, "minyear"));
  SET_STRING_ELT(NamesR, 5, mkChar("minyear"));

  /* maxyear */
  SET_VECTOR_ELT(Rval, 6, getRangeElement(Range, "maxyear"));
  SET_STRING_ELT(NamesR, 6, mkChar("maxyear"));

  /* GET  year, unit, season, area, iter */
  sql = sqlite3_mprintf("SELECT year, unit, season, area, iter FROM %q_slots;", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(3);
    return (Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%s", "HERE");
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(3);
    return (Rval);
  }

  /* year */
  INTEGER(Value)[0] = sqlite3_column_int(stmt, 0);
  SET_VECTOR_ELT(Rval, 4, Value);
  SET_STRING_ELT(NamesR, 4, mkChar("year"));
  
  /* unit */
  INTEGER(Value)[0] = sqlite3_column_int(stmt, 1);
  SET_VECTOR_ELT(Rval, 7, Value);
  SET_STRING_ELT(NamesR, 7, mkChar("unit"));

  /* season */
  INTEGER(Value)[0] = sqlite3_column_int(stmt, 2);
  SET_VECTOR_ELT(Rval, 8, Value);
  SET_STRING_ELT(NamesR, 8, mkChar("season"));

  /* area */
  INTEGER(Value)[0] = sqlite3_column_int(stmt, 3);
  SET_VECTOR_ELT(Rval, 9, Value);
  SET_STRING_ELT(NamesR, 9, mkChar("area"));

  /* iter */
  INTEGER(Value)[0] = sqlite3_column_int(stmt, 4);
  SET_VECTOR_ELT(Rval, 10, Value);
  SET_STRING_ELT(NamesR, 10, mkChar("iter"));

  /* FINALIZE stmt */
  rc = finalizeAllstmt(db);

  /* CLOSE */
  rc = sqlite3_close(db);
  if(rc != SQLITE_OK) {
    Rprintf("%s", sqlite3_errmsg(db));
  }

  setAttrib(Rval, R_NamesSymbol, NamesR);

	UNPROTECT(3);
	return (Rval);
} /* }}} */
