/*
 * update.c = 
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas, UK
 * Last Change: 07 Oct 2009 15:40
 * $Id$
 *
 */

#include "exports.h"
#include "functions.h"
#include <time.h>

/* Function SEXP updateFLComp(SEXP Rdbname, SEXP Rname, ...) {{{ */
SEXP updateFLComp(SEXP Rdbname, SEXP Rname, SEXP Rstmt, SEXP Rvalue, SEXP Rdims,
    SEXP Ri, SEXP Rj, SEXP Rk, SEXP Rl, SEXP Rm, SEXP Rn)
  {
  SEXP Rval = R_NilValue;
  PROTECT(Rval = allocVector(INTSXP, 1));
  Rname = AS_CHARACTER(Rname);
    
  int rc, i, v;
  int lenv, leni, lenj, lenk, lenl, lenn, lenm, dims[6];
  sqlite3 *db;
  sqlite3_stmt *stmt;
  const char *tail;
  char *sql;

  for(i=0; i<6; i++)
    dims[i] = INTEGER(Rdims)[i];

  INTEGER(Rval)[0] = 1;

  lenv = LENGTH(Rvalue);
  leni = LENGTH(Ri);
  lenj = LENGTH(Rj);
  lenk = LENGTH(Rk);
  lenl = LENGTH(Rl);
  lenm = LENGTH(Rm);
  lenn = LENGTH(Rn);

  /* OPEN */
  rc = sqlite3_open(CHAR(STRING_ELT(Rdbname, 0)), &db);
  /* Can db be opened? */
  if(rc != SQLITE_OK)
  {
     Rprintf("%i: %s\n", rc, sqlite3_errmsg(db));
     UNPROTECT(1);
     return (Rval);
  }

  /* BEGIN */
  rc = sqlite3_exec(db, "PRAGMA synchronous=OFF;", NULL, NULL, NULL);
  rc = sqlite3_exec(db, "PRAGMA temp_store = MEMORY;", NULL, NULL, NULL);
  /* Can PRAGMA statement be executed? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n", rc, sqlite3_errmsg(db));
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  rc = sqlite3_exec(db, "BEGIN TRANSACTION;", NULL, NULL, NULL);
  /* Can BEGIN statement be executed? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n", rc, sqlite3_errmsg(db));
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
 
  /* PREPARE STMT */
  sql = sqlite3_mprintf("%q", CHAR(STRING_ELT(Rstmt, 0)));
  rc = sqlite3_prepare_v2(db, sql, (int)strlen(sql), &stmt, &tail);
  /* Can UPDATE statement be prepared? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  
  for (v=0; v < lenv; v++)
  {
    sqlite3_reset(stmt);

    /* BIND value */
    rc=sqlite3_bind_double(stmt, 1, REAL(Rvalue)[v]);
    /* Can range values be bound to statement? */
    if(rc != SQLITE_OK) {
      Rprintf("%s", sqlite3_errmsg(db));
      sqlite3_close(db);
      UNPROTECT(1);
      return (Rval);
    }
    /* BIND quant */
    if(dims[0] > 0)
    {
      rc=sqlite3_bind_text(stmt, dims[0] + 1, CHAR(STRING_ELT(Ri, v%leni)), -1, NULL);
      /* Can dims be bound to statement? */
      if(rc != SQLITE_OK) {
         Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
         sqlite3_close(db);
         UNPROTECT(1);
         return (Rval);
      }
    }
    
    /* BIND year */
    if(dims[1] > 0)
    {
     rc=sqlite3_bind_text(stmt, dims[1] + 1, CHAR(STRING_ELT(Rj, v%lenj)), -1, NULL);
      /* Can dims be bound to statement? */
      if(rc != SQLITE_OK) {
         Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
         sqlite3_close(db);
         UNPROTECT(1);
         return (Rval);
      }
    }
    
    /* BIND unit */
    if(dims[2] > 0)
    {
     rc=sqlite3_bind_text(stmt, dims[2] + 1, CHAR(STRING_ELT(Rk, v%lenk)), -1, NULL);
      /* Can dims be bound to statement? */
      if(rc != SQLITE_OK) {
         Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
         sqlite3_close(db);
         UNPROTECT(1);
         return (Rval);
      }
    }

    /* BIND season */
    if(dims[3] > 0)
    {
     rc=sqlite3_bind_text(stmt, dims[3] + 1, CHAR(STRING_ELT(Rl, v%lenl)), -1, NULL);
      /* Can dims be bound to statement? */
      if(rc != SQLITE_OK) {
         Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
         sqlite3_close(db);
         UNPROTECT(1);
         return (Rval);
      }
    }
    
    /* BIND area */
    if(dims[4] > 0)
    {
     rc=sqlite3_bind_text(stmt, dims[4] + 1, CHAR(STRING_ELT(Rm, v%lenm)), -1, NULL);
      /* Can dims be bound to statement? */
      if(rc != SQLITE_OK) {
         Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
         sqlite3_close(db);
         UNPROTECT(1);
         return (Rval);
      }
    }

    /* BIND iter */
    if(dims[5] > 0)
    {
     rc=sqlite3_bind_text(stmt, dims[5] + 1, CHAR(STRING_ELT(Rn, v%lenn)), -1, NULL);
      /* Can dims be bound to statement? */
      if(rc != SQLITE_OK) {
         Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
         sqlite3_close(db);
         UNPROTECT(1);
         return (Rval);
      }
    }

    /* STEP */
    rc=sqlite3_step(stmt);
    /* Can step be carried out? */
    if(rc != SQLITE_DONE) {
       Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
       sqlite3_close(db);
       UNPROTECT(1);
       return (Rval);
    }
  }
  
  /* COMMIT ... */
  sqlite3_exec(db, "COMMIT;", NULL, NULL, NULL);

  /* INDEX */
  sql = sqlite3_mprintf("CREATE UNIQUE INDEX %q.data ON %q_data (slot,quant,year);", CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(Rdbname, 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
 
  /* FINALIZE */
  while((stmt = sqlite3_next_stmt(db, 0))!=0)
  {
    rc = sqlite3_finalize(stmt);
    if(rc != SQLITE_OK) {
      Rprintf("%s", sqlite3_errmsg(db));
    }
  }

  /* CLOSE */
  while((stmt = sqlite3_next_stmt(db, 0))!=0)
  {
    sqlite3_finalize(stmt);
  }
  rc = sqlite3_close(db);
  if(rc != SQLITE_OK) {
    Rprintf("%s", sqlite3_errmsg(db));
  }
  
  INTEGER(Rval)[0] = 0;
  UNPROTECT(1);
  return (Rval);
  } /* }}} */
