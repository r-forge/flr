/*
 * select.c = 
 * SQLiteFL/src/select.c
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas, UK
 * $Id: select.c 363 2009-10-20 12:40:39Z imosqueira $
 *
 */

#include "exports.h"
#include "functions.h"

/* Function SEXP selectSlotFLComp(SEXP Rdbname, SEXP Rname, SEXP Rslot) {{{ */
SEXP selectSlotFLComp(SEXP Rdbname, SEXP Rname, SEXP Rslot)
{

  int rc;
	SEXP Rval, Quant;
	PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = 1;

  sqlite3 *db;
  sqlite3_stmt *stmt;
  char *sql;

  Rname = AS_CHARACTER(Rname);
  Rslot = AS_CHARACTER(Rslot);

  /* OPEN */
  rc = sqlite3_open(CHAR(STRING_ELT(Rdbname, 0)), &db);
  /* Can db be opened? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    UNPROTECT(1);
    return (Rval);
  }

  /* check tables are OK */
  rc = checkFLCompTables(db, CHAR(STRING_ELT(Rname, 0)));
  /* are tables OK */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n", rc, "Table structure is incorrect");
    UNPROTECT(1);
    return (Rval);
  }

  /* get slot */
  PROTECT(Quant = (SEXP) getSlotFLComp(db, CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(Rslot, 0))));

  /* FINALIZE */
  while((stmt = sqlite3_next_stmt(db, 0))!=0)
  {
    rc = sqlite3_finalize(stmt);
    if(rc != SQLITE_OK) {
      Rprintf("%s", sqlite3_errmsg(db));
    }
  }

  /* CLOSE */
  rc = sqlite3_close(db);
  if(rc != SQLITE_OK) {
    Rprintf("%s", sqlite3_errmsg(db));
  }

	UNPROTECT(2);

	return (Quant);
} /* }}} */

/* Function SEXP selectFromSlotFLComp(SEXP Rdbname, SEXP Rname, SEXP Rslot, SEXP Rselect, SEXP RdimSelect) {{{ */
SEXP selectFromSlotFLComp(SEXP Rdbname, SEXP Rname, SEXP Rslot, SEXP Rselect, SEXP RdimSelect, SEXP Rquant)
{

  int rc;
	SEXP Rval, Quant;
	PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = 1;

  sqlite3 *db;
  sqlite3_stmt *stmt;
  const char *quant;
  char *sql;

  Rname = AS_CHARACTER(Rname);
  Rslot = AS_CHARACTER(Rslot);
  Rselect = AS_CHARACTER(Rselect);
  Rquant = AS_CHARACTER(Rquant);

  /* OPEN */
  rc = sqlite3_open(CHAR(STRING_ELT(Rdbname, 0)), &db);
  /* Can db be opened? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    UNPROTECT(1);
    return (Rval);
  }

  /* CHECK tables are OK */
  rc = checkFLCompTables(db, CHAR(STRING_ELT(Rname, 0)));
  /* are tables OK */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n", rc, "Table structure is incorrect");
    UNPROTECT(1);
    return (Rval);
  }

  /* CHECK quant matches */
  quant = CHAR(STRING_ELT(getMetaFLComp(db, CHAR(STRING_ELT(Rname, 0)), "quant"), 0));
  if(strlen(CHAR(STRING_ELT(Rquant, 0))) > 0)
    if(strcmp(CHAR(STRING_ELT(Rquant, 0)), quant) != 0)
    {
      Rprintf("%s\n", "Dimension name for 'quant' dimension do not match");
      UNPROTECT(1);
      return(Rval);
    }

  /* get slot */
  PROTECT(Quant = (SEXP) getFromSlotFLComp(db, CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(Rslot, 0)), CHAR(STRING_ELT(Rselect, 0)), INTEGER(RdimSelect)));

  /* FINALIZE */
  while((stmt = sqlite3_next_stmt(db, 0))!=0)
  {
    rc = sqlite3_finalize(stmt);
    if(rc != SQLITE_OK) {
      Rprintf("%s", sqlite3_errmsg(db));
    }
  }

  /* CLOSE */
  rc = sqlite3_close(db);
  if(rc != SQLITE_OK) {
    Rprintf("%s", sqlite3_errmsg(db));
  }

	UNPROTECT(2);

	return (Quant);
} /* }}} */

/* Function SEXP selectFLComp(SEXP Rdbname, SEXP Rname) {{{ */
SEXP selectFLComp(SEXP Rdbname, SEXP Rname)
{
	SEXP Rval, Object, Quant;
	PROTECT(Rval = allocVector(INTSXP, 1));
  PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

  int rc;
  sqlite3 *db;
  sqlite3_stmt *stmt;
  const char *tail;
  const unsigned char *class, *slot;
  char *sql;

  /* OPEN */
  rc = sqlite3_open(CHAR(STRING_ELT(Rdbname, 0)), &db);
  /* Can db be opened? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    UNPROTECT(2);
    return (Rval);
  }

  /* check tables are OK */
  rc = checkFLCompTables(db, CHAR(STRING_ELT(Rname, 0)));
  /* are tables OK */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n", rc, "Table structure is incorrect");
    UNPROTECT(2);
    return (Rval);
  }

  /* GET class of object */
  sql = sqlite3_mprintf("SELECT value FROM \"%q_meta\" WHERE field = 'class';", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(2);
    return (Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(2);
    return (Rval);
  }
  class = sqlite3_column_text(stmt, 0);

  /* CREATE empty object */
  PROTECT(Object = NEW_OBJECT(MAKE_CLASS((char *)class)));

  /* GET slot names */
  sql = sqlite3_mprintf("SELECT slot FROM \"%q_slots\";", CHAR(STRING_ELT(Rname, 0)));
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
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(3);
    return (Rval);
  }
  while(rc == SQLITE_ROW) {
    /* SET slot in Object */
    slot = sqlite3_column_text(stmt, 0);
    Quant = (SEXP) getSlotFLComp(db, CHAR(STRING_ELT(Rname, 0)), (char *) slot);
    SET_SLOT(Object, install((char *) slot), Quant);
    rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(3);
      return (Rval);
    }
  }
  sqlite3_free(sql);

  /* GET name slot */
  SET_SLOT(Object, install("name"), getMetaFLComp(db, CHAR(STRING_ELT(Rname, 0)), "name"));
  
  /* GET desc slot */
  SET_SLOT(Object, install("desc"), getMetaFLComp(db, CHAR(STRING_ELT(Rname, 0)), "desc"));
  
  /* GET range */
  SET_SLOT(Object, install("range"), getRangeFLComp(db, CHAR(STRING_ELT(Rname, 0))));

  /* FINALIZE stmt */
  rc = finalizeAllstmt(db);

  /* CLOSE */
  rc = sqlite3_close(db);
  if(rc != SQLITE_OK) {
    Rprintf("%s", sqlite3_errmsg(db));
  }

	UNPROTECT(3);

	return (Object);
} /* }}} */

/* Function SEXP selectFromFLComp(SEXP Rdbname, SEXP Rname) {{{ */
SEXP selectFromFLComp(SEXP Rdbname, SEXP Rname, SEXP Rselect)
{
	SEXP Rval, Object, Quant;
	PROTECT(Rval = allocVector(INTSXP, 1));
  PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

  int rc;
  sqlite3 *db;
  sqlite3_stmt *stmt;
  const char *tail;
  const unsigned char *class, *slot;
  char *sql;

  /* OPEN */
  rc = sqlite3_open(CHAR(STRING_ELT(Rdbname, 0)), &db);
  /* Can db be opened? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    UNPROTECT(2);
    return (Rval);
  }

  /* check tables are OK */
  rc = checkFLCompTables(db, CHAR(STRING_ELT(Rname, 0)));
  /* are tables OK */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n", rc, "Table structure is incorrect");
    UNPROTECT(2);
    return (Rval);
  }

  /* GET class of object */
  sql = sqlite3_mprintf("SELECT value FROM \"%q_meta\" WHERE field = 'class';", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_prepare(db, sql, -1, &stmt, &tail);
  /* Can statement be prepared? */
  if(rc != SQLITE_OK) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(2);
    return (Rval);
  }
  rc = sqlite3_step(stmt);
  /* Can statement be run? */
  if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(2);
    return (Rval);
  }
  class = sqlite3_column_text(stmt, 0);

  /* CREATE empty object */
  PROTECT(Object = NEW_OBJECT(MAKE_CLASS((char *)class)));

  /* GET slot names */
  sql = sqlite3_mprintf("SELECT slot FROM \"%q_slots\";", CHAR(STRING_ELT(Rname, 0)));
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
    Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    UNPROTECT(3);
    return (Rval);
  }
  while(rc == SQLITE_ROW) {
    /* SET slot in Object */
    slot = sqlite3_column_text(stmt, 0);
    /* 
     * TODO
    Quant = (SEXP) getFromSlotFLComp(db, CHAR(STRING_ELT(Rname, 0)), (char *) slot);
    SET_SLOT(Object, install((char *) slot), Quant);
    PROTECT(Quant = (SEXP) getFromSlotFLComp(db, CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(Rslot, 0)), CHAR(STRING_ELT(Rselect, 0)), INTEGER(RdimSelect)));
    */

    rc = sqlite3_step(stmt);
    /* Can quant SELECT statement be run? */
    if(rc != SQLITE_DONE & rc != SQLITE_ROW) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_finalize(stmt);
      sqlite3_close(db);
      UNPROTECT(3);
      return (Rval);
    }
  }
  sqlite3_free(sql);

  /* GET name slot */
  SET_SLOT(Object, install("name"), getMetaFLComp(db, CHAR(STRING_ELT(Rname, 0)), "name"));
  
  /* GET desc slot */
  SET_SLOT(Object, install("desc"), getMetaFLComp(db, CHAR(STRING_ELT(Rname, 0)), "desc"));
  
  /* GET range */
  SET_SLOT(Object, install("range"), getRangeFLComp(db, CHAR(STRING_ELT(Rname, 0))));

  /* FINALIZE stmt */
  rc = finalizeAllstmt(db);

  /* CLOSE */
  rc = sqlite3_close(db);
  if(rc != SQLITE_OK) {
    Rprintf("%s", sqlite3_errmsg(db));
  }

	UNPROTECT(3);

	return (Object);
} /* }}} */
