/*
 * functions.c = 
 * SQLiteFL/src/functions.c
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas, UK
 * $Id: functions.c,v 1.10 2009/01/08 11:23:48 imosqueira Exp $
 *
 */

#include "functions.h"

/* Function int checkFLCompTables(sqlite3 *db, const char *name) {{{ */
int checkFLCompTables(sqlite3 *db, const char *name)
{

  int rc;
  char *sql;
  sqlite3_stmt *stmt;
  const char *tail, *tname;

  /* CHECK if the four table names are present */
  /* SELECT table names */
  sql = sqlite3_mprintf("SELECT name FROM sqlite_master WHERE type='table' AND name LIKE '%q\_%q' ORDER by name;", name, "%");
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i\n", 888);
    return(rc);
  }
  rc = sqlite3_step(stmt);
  /* Can statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    return(rc);
  }
  while(rc == SQLITE_ROW) {
    /* strcmp */
    tname = sqlite3_column_text(stmt, 0);
    if(strcmp(strrchr(tname, '_'), "_data") != 0 & strcmp(strrchr(tname, '_'), "_slots") != 0 & strcmp(strrchr(tname, '_'), "_meta") != 0 & strcmp(strrchr(tname, '_'), "_range") != 0)
    {
     return(300);
    }
    rc = sqlite3_step(stmt);
  }
  return(SQLITE_OK);
} /* }}} */

/* Function SEXP getSlotFLComp(sqlite3 *db, const char *name, const char *slot) {{{ */
SEXP getSlotFLComp(sqlite3 *db, const char *name, const char *slot)
{
	SEXP Quant, Rval, dim, v, names, dimnames, d1, d2, d3, d4, d5, d6, units;
  PROTECT(Rval = allocVector(INTSXP, 1));       
  PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));
  PROTECT(dim = allocVector(INTSXP, 6));       
  PROTECT(dimnames = allocVector(VECSXP, 6));

  int rc, i;
  sqlite3_stmt *stmt;
  const char *tail;
  char *sql;

  INTEGER(Rval)[0] = 1;
  
  /* SELECT dims */
  sql = sqlite3_mprintf("SELECT quant, year, unit, season, area, iter FROM %q_slots WHERE slot = '%q';", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    UNPROTECT(4);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    return (Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(4);
    return(Rval);
  }
  for (i=0; i<6; i++) {
    INTEGER(dim)[i] = sqlite3_column_int(stmt, i);
  }
  /* SELECT dimnames */
  PROTECT(d1 = allocVector(STRSXP, INTEGER(dim)[0]));
  PROTECT(d2 = allocVector(STRSXP, INTEGER(dim)[1]));
  PROTECT(d3 = allocVector(STRSXP, INTEGER(dim)[2]));
  PROTECT(d4 = allocVector(STRSXP, INTEGER(dim)[3]));
  PROTECT(d5 = allocVector(STRSXP, INTEGER(dim)[4]));
  PROTECT(d6 = allocVector(STRSXP, INTEGER(dim)[5]));
  
  /* quant */
  sql = sqlite3_mprintf("SELECT DISTINCT quant FROM %q_data WHERE slot = '%q' ORDER BY rowID;", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d1, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(10);
      return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 0, d1);
  sqlite3_free(sql);

  /* year */
  sql = sqlite3_mprintf("SELECT DISTINCT year FROM %q_data WHERE slot = '%q' ORDER BY rowID;", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n",rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d2, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 1, d2);
  sqlite3_free(sql);

  /* unit */
  sql = sqlite3_mprintf("SELECT DISTINCT unit FROM %q_data WHERE slot = '%q' ORDER BY rowID;", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d3, i++, mkChar(sqlite3_column_text(stmt, 0)));
    rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(10);
      return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 2, d3);
  sqlite3_free(sql);

  /* season */
  sql = sqlite3_mprintf("SELECT DISTINCT season FROM %q_data WHERE slot = '%q' ORDER BY rowID;", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d4, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(10);
      return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 3, d4);
  sqlite3_free(sql);

  /* area */
  sql = sqlite3_mprintf("SELECT DISTINCT area FROM %q_data WHERE slot = '%q' ORDER BY rowID;", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d5, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(10);
      return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 4, d5);
  sqlite3_free(sql);

  /* iter */
  sql = sqlite3_mprintf("SELECT DISTINCT iter FROM %q_data WHERE slot = '%q' ORDER BY rowID;", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d6, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  }
  SET_VECTOR_ELT(dimnames, 5, d6);
  sqlite3_free(sql);

  /* dimnames names */
  PROTECT(names = allocVector(STRSXP, 6));
  sql = sqlite3_mprintf("SELECT value FROM %q_meta WHERE field = 'quant';", name);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(11);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(11);
    return(Rval);
  }
  SET_STRING_ELT(names, 0, mkChar(sqlite3_column_text(stmt, 0)));
  SET_STRING_ELT(names, 1, mkChar("year"));
  SET_STRING_ELT(names, 2, mkChar("unit"));
  SET_STRING_ELT(names, 3, mkChar("season"));
  SET_STRING_ELT(names, 4, mkChar("area"));
  SET_STRING_ELT(names, 5, mkChar("iter")); 

  setAttrib(dimnames, R_NamesSymbol, names);

  /* units */
  PROTECT(units = allocVector(STRSXP, 1));
  sql = sqlite3_mprintf("SELECT units FROM %q_slots WHERE slot = '%q';", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(12);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(12);
    return(Rval);
  }
  SET_STRING_ELT(units, 0, mkChar(sqlite3_column_text(stmt, 0)));

  /* data */
  PROTECT(v = Rf_allocArray(REALSXP, dim)); 
  sql = sqlite3_mprintf("SELECT data FROM %q_data WHERE slot = '%q' ORDER BY rowId;", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(13);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(13);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    if(sqlite3_column_type(stmt, 0) == SQLITE_NULL)
      REAL(v)[i] = R_NaReal;
    else
      REAL(v)[i] = sqlite3_column_double(stmt, 0);
    rc = sqlite3_step(stmt);
    /* Can SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(13);
      return(Rval);
    }
    i++;
  }

  setAttrib(v, R_DimNamesSymbol, dimnames);
  Quant = R_do_slot_assign(Quant, install(".Data"), v);
  SET_ATTR(Quant, install("units"), units);

  UNPROTECT(13);
	return (Quant);
} /* }}} */

/* Function SEXP getMetaFLComp(sqlite3 *db, const char *name, cons char *field)   {{{*/
SEXP getMetaFLComp(sqlite3 *db, const char *name, const char *field)
{

  SEXP Rval;
  PROTECT(Rval = allocVector(INTSXP, 1));       

  int rc;
  sqlite3_stmt *stmt;
  const char *tail;
  char *sql;

  INTEGER(Rval)[0] = 1;

  /* GET name */ 
  sql = sqlite3_mprintf("SELECT value FROM %q_meta WHERE field = '%q';", name, field);
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
  UNPROTECT(1);
  return(mkString(sqlite3_column_text(stmt, 0)));
} /* }}} */

/* Function SEXP getRangeFLComp(sqlite3 *db, const char *name)   {{{*/
SEXP getRangeFLComp(sqlite3 *db, const char *name)
{

  SEXP Rval, Range, NamesR;
  PROTECT(Rval = allocVector(INTSXP, 1));       

  int rc, lenr, i;
  sqlite3_stmt *stmt;
  const char *tail;
  char *sql;

  INTEGER(Rval)[0] = 1;

  /* GET range */
  /* Get length of range*/
  sql = sqlite3_mprintf("SELECT count(*) FROM %q_range;", name);
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
  /* Set variable for range length */
  lenr = sqlite3_column_int(stmt, 0);

  /* Create range and names vectors */
  PROTECT(Range = allocVector(REALSXP, lenr));
  PROTECT(NamesR = allocVector(STRSXP, lenr));

  /* Assign names and values to vectors */
  sql = sqlite3_mprintf("SELECT field, value FROM %q_range;", name);
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
  i = 0;
  while(rc == SQLITE_ROW) {
    /* SET name and value in range vectors */
    SET_STRING_ELT(NamesR, i, mkChar(sqlite3_column_text(stmt, 0)));
    REAL(Range)[i] = sqlite3_column_double(stmt, 1);
    rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(3);
      return (Rval);
    }
    i++;
  }
  sqlite3_free(sql);

  setAttrib(Range, R_NamesSymbol, NamesR);
  
  UNPROTECT(3);
  return(Range);
} /* }}} */

/* Function int finalizeAllstmt(sqlite3 *db)  {{{*/
int finalizeAllstmt(sqlite3 *db)
{
  int rc;
  sqlite3_stmt *stmt;
    
  while((stmt = sqlite3_next_stmt(db, 0))!=0)
  {
    rc = sqlite3_finalize(stmt);
    if(rc != SQLITE_OK) {
      Rprintf("%s", sqlite3_errmsg(db));
    }
  }
  return(rc);
} /* }}} */

/* Function SEXP getFromSlotFLComp(sqlite3 *db, const char *name, const char *slot) {{{ */
SEXP getFromSlotFLComp(sqlite3 *db, const char *name, const char *slot, const char *select, int *dimSelect)
{
	SEXP Quant, Rval, dim, v, names, dimnames, d1, d2, d3, d4, d5, d6, units;
  PROTECT(Rval = allocVector(INTSXP, 1));       
  PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));
  PROTECT(dim = allocVector(INTSXP, 6));       
  PROTECT(dimnames = allocVector(VECSXP, 6));

  int rc, i;
  sqlite3_stmt *stmt;
  const char *tail;
  char *sql;

  INTEGER(Rval)[0] = 1;
  
  /* SELECT dims */
  sql = sqlite3_mprintf("SELECT quant, year, unit, season, area, iter FROM %q_slots WHERE slot = '%q';", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    UNPROTECT(4);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    return (Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(4);
    return(Rval);
  }
  for (i=0; i<6; i++) {
    /* MODIFY dim as per selection */
    if (dimSelect[i] != 0)
      INTEGER(dim)[i] = dimSelect[i];
    /* or ALLOCATE value from sql */
    else
      INTEGER(dim)[i] = sqlite3_column_int(stmt, i);
  }

  /* SELECT dimnames */
  PROTECT(d1 = allocVector(STRSXP, INTEGER(dim)[0]));
  PROTECT(d2 = allocVector(STRSXP, INTEGER(dim)[1]));
  PROTECT(d3 = allocVector(STRSXP, INTEGER(dim)[2]));
  PROTECT(d4 = allocVector(STRSXP, INTEGER(dim)[3]));
  PROTECT(d5 = allocVector(STRSXP, INTEGER(dim)[4]));
  PROTECT(d6 = allocVector(STRSXP, INTEGER(dim)[5]));

  /* quant */
  sql = sqlite3_mprintf("SELECT DISTINCT quant FROM %q_data WHERE slot = '%q' %q ORDER BY rowID;", name, slot, select);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d1, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(10);
      return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 0, d1);
  sqlite3_free(sql);

  /* year */
  sql = sqlite3_mprintf("SELECT DISTINCT year FROM %q_data WHERE slot = '%q' %q ORDER BY rowID;", name, slot, select);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n",rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d2, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 1, d2);
  sqlite3_free(sql);

  /* unit */
  sql = sqlite3_mprintf("SELECT DISTINCT unit FROM %q_data WHERE slot = '%q' %q ORDER BY rowID;", name, slot, select);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d3, i++, mkChar(sqlite3_column_text(stmt, 0)));
    rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(10);
      return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 2, d3);
  sqlite3_free(sql);

  /* season */
  sql = sqlite3_mprintf("SELECT DISTINCT season FROM %q_data WHERE slot = '%q' %q ORDER BY rowID;", name, slot, select);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d4, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(10);
      return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 3, d4);
  sqlite3_free(sql);

  /* area */
  sql = sqlite3_mprintf("SELECT DISTINCT area FROM %q_data WHERE slot = '%q' %q ORDER BY rowID;", name, slot, select);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d5, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(10);
      return(Rval);
    }
  }
  SET_VECTOR_ELT(dimnames, 4, d5);
  sqlite3_free(sql);

  /* iter */
  sql = sqlite3_mprintf("SELECT DISTINCT iter FROM %q_data WHERE slot = '%q' %q ORDER BY rowID;", name, slot, select);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can quant SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    SET_STRING_ELT(d6, i++, mkChar(sqlite3_column_text(stmt, 0)));
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(10);
    return(Rval);
  }
  }
  SET_VECTOR_ELT(dimnames, 5, d6);
  sqlite3_free(sql);

  /* dimnames names */
  PROTECT(names = allocVector(STRSXP, 6));
  sql = sqlite3_mprintf("SELECT value FROM %q_meta WHERE field = 'quant';", name);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(11);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(11);
    return(Rval);
  }
  SET_STRING_ELT(names, 0, mkChar(sqlite3_column_text(stmt, 0)));
  SET_STRING_ELT(names, 1, mkChar("year"));
  SET_STRING_ELT(names, 2, mkChar("unit"));
  SET_STRING_ELT(names, 3, mkChar("season"));
  SET_STRING_ELT(names, 4, mkChar("area"));
  SET_STRING_ELT(names, 5, mkChar("iter")); 

  setAttrib(dimnames, R_NamesSymbol, names);

  /* units */
  PROTECT(units = allocVector(STRSXP, 1));
  sql = sqlite3_mprintf("SELECT units FROM %q_slots WHERE slot = '%q';", name, slot);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(12);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can quant SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(12);
    return(Rval);
  }
  SET_STRING_ELT(units, 0, mkChar(sqlite3_column_text(stmt, 0)));

  /* data */
  PROTECT(v = Rf_allocArray(REALSXP, dim)); 
  sql = sqlite3_mprintf("SELECT data FROM %q_data WHERE slot = '%q' %q ORDER BY rowId;", name, slot, select);
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can SELECT statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(13);
    return(Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can SELECT statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(13);
    return(Rval);
  }
  i = 0;
  while(rc == SQLITE_ROW) {
    if(sqlite3_column_type(stmt, 0) == SQLITE_NULL)
      REAL(v)[i] = R_NaReal;
    else
      REAL(v)[i] = sqlite3_column_double(stmt, 0);
    rc = sqlite3_step(stmt);
    /* Can SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(13);
      return(Rval);
    }
    i++;
  }

  setAttrib(v, R_DimNamesSymbol, dimnames);
  Quant = R_do_slot_assign(Quant, install(".Data"), v);
  SET_ATTR(Quant, install("units"), units);

  UNPROTECT(13);
	return (Quant);
} /* }}} */

/* SEXP getRangeElement(SEXP range, const char *str)  {{{ */ 
SEXP getRangeElement(SEXP range, const char *str)
{
  SEXP elmt = allocVector(REALSXP, 1), names = getAttrib(range, R_NamesSymbol);
  int i;
     
  for (i = 0; i < length(range); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      REAL(elmt)[0] = REAL(range)[i];
      break;
    }
  return elmt;
} /*  }}} */

