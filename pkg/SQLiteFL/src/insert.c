/*
 * insert.c
 * SQLiteFL/src/insert.c
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas, UK 
 * $Id$
 *
 */

#include "exports.h"
#include "functions.h"
#include <time.h>

/* Function SEXP insertFLQ(SEXP Rname, SEXP Rflq, SEXP Rdbname) {{{ */
// TODO Fix according to insertFLComp
SEXP insertFLQ(SEXP Rname, SEXP Rflq, SEXP Rdbname)
{
  /* VARIABLES */
	SEXP Rval = R_NilValue;
	PROTECT(Rval = allocVector(INTSXP, 1));
  Rname = AS_CHARACTER(Rname);
  
  int rc, i, j, k, l, m, n;
  int leni, lenj, lenk, lenl, lenm, lenn;
  sqlite3 *db;
  sqlite3_stmt *stmt;
  const char *tail, *dname;
  char *sql;

  /* OPEN */
  rc = sqlite3_open(CHAR(STRING_ELT(Rdbname, 0)), &db);
  /* Can db be opened? */
  if(rc != SQLITE_OK) {
    sqlite3_close(db);
    INTEGER(Rval)[0] = 1;
    Rprintf("Error: %i\n", INTEGER(Rval)[0]);
    return (Rval);
  }

  /* BEGIN */
  rc = sqlite3_exec(db, "BEGIN TRANSACTION;", NULL, NULL, NULL);
  rc = sqlite3_exec(db, "PRAGMA synchronous=OFF;", NULL, NULL, NULL);

  /* DROP TABLES */
  sql = sqlite3_mprintf("DROP TABLE IF EXISTS \"%q_meta\";", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  /* Can meta table be dropped? */
  if(rc != SQLITE_OK) {
    sqlite3_exec(db, "ROLLBACK TRANSACTION;", NULL, NULL, NULL);
    sqlite3_close(db);
    INTEGER(Rval)[0] = 2;
    Rprintf("Error: %i\n", INTEGER(Rval)[0]);
    return (Rval);
  }
  sqlite3_free(sql);

  sql = sqlite3_mprintf("DROP TABLE IF EXISTS \"%q_data\";", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  /* Can data table be dropped? */
  if(rc != SQLITE_OK) {
    sqlite3_exec(db, "ROLLBACK TRANSACTION;", NULL, NULL, NULL);
    sqlite3_close(db);
    INTEGER(Rval)[0] = 3;
    Rprintf("%i", INTEGER(Rval)[0]);
    return (Rval);
  }
  sqlite3_free(sql);

  /* CREATE meta TABLE */
  sql = sqlite3_mprintf("CREATE TABLE \"%q_meta\" ('rowId' INTEGER NOT NULL PRIMARY KEY, 'field' CHAR DEFAULT 'quant', 'value' CHAR DEFAULT 'quant');", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, 0, 0, 0);
  /* Can meta table be created? */
  if( rc ) {
    sqlite3_exec(db, "ROLLBACK TRANSACTION;", NULL, NULL, NULL);
    sqlite3_close(db);
    INTEGER(Rval)[0] = 4;
    Rprintf("%i", INTEGER(Rval)[0]);
    return (Rval);
  }
  sqlite3_free(sql);

  /* CREATE data TABLE */
  sql = sqlite3_mprintf("CREATE TABLE \"%q_data\" ('rowId' INTEGER NOT NULL PRIMARY KEY, 'quant' CHAR DEFAULT 'all', 'year' INTEGER DEFAULT '1' CHECK (year > 0), 'unit' CHAR DEFAULT 'unique', 'season' CHAR DEFAULT 'all', 'area' CHAR DEFAULT 'unique', 'iter' INTEGER DEFAULT '1' CHECK (iter > 0), 'data' DOUBLE DEFAULT '0.0', UNIQUE (quant, year, unit, season, area, iter));", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, 0, 0, 0);
  /* Can data table be created? */
  if( rc ) {
    sqlite3_exec(db, "ROLLBACK TRANSACTION;", NULL, NULL, NULL);
    sqlite3_close(db);
    INTEGER(Rval)[0] = 5;
    Rprintf("%i", INTEGER(Rval)[0]);
    return (Rval);
  }
  sqlite3_free(sql);

  /* VARS */
  SEXP Quant     = PROTECT(GET_SLOT(Rflq, install(".Data"))),
        units    = PROTECT(GET_SLOT(Rflq, install("units"))),
        dims     = GET_DIM(Quant),
        dimnames = GET_DIMNAMES(Quant),
        names    = GET_NAMES(dimnames);
  
  /* INSERT meta */
  /* quant */
  sql = sqlite3_mprintf("INSERT INTO \"%q_meta\" (field, value) VALUES ('quant', '%q');", CHAR(STRING_ELT(Rname, 0)), CHAR(VECTOR_ELT(names, 0)));
  sqlite3_exec(db, sql, 0, 0, 0);
  /* Can quant in meta be inserted? */
  if(rc) {
    sqlite3_exec(db, "ROLLBACK TRANSACTION;", NULL, NULL, NULL);
    sqlite3_close(db);
    INTEGER(Rval)[0] = 5;
    Rprintf("%i", INTEGER(Rval)[0]);
    return (Rval);
  }
  sqlite3_free(sql);
  /* units */
  sql = sqlite3_mprintf("INSERT INTO \"%q_meta\" (field, value) VALUES ('units', '%q');", CHAR(STRING_ELT(Rname, 0)), CHAR(VECTOR_ELT(units, 0)));
  sqlite3_exec(db, sql, 0, 0, 0);
  if(rc) {
    sqlite3_exec(db, "ROLLBACK TRANSACTION;", NULL, NULL, NULL);
    sqlite3_close(db);
    INTEGER(Rval)[0] = 5;
    Rprintf("%i", INTEGER(Rval)[0]);
    return (Rval);
  }
  sqlite3_free(sql);

  /* data ESTATEMENT */
  sql = sqlite3_mprintf("INSERT INTO \"%q_data\" (quant, year, unit, season, area, iter, data) VALUES (?,?,?,?,?,?,?);", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_prepare_v2(db, sql, (int)strlen(sql), &stmt, &tail);
  if(rc != SQLITE_OK) {
    Rprintf("PREPARE %i\n", rc);
  }
  
  leni = INTEGER(dims)[0];
  lenj = INTEGER(dims)[1];
  lenk = INTEGER(dims)[2];
  lenl = INTEGER(dims)[3];
  lenm = INTEGER(dims)[4];
  lenn = INTEGER(dims)[5];
  
  /* BIND values */
  for (i = 0; i < leni; i++){
    for (j = 0; j < lenj; j++){
      for (k = 0; k < lenk; k++){
        for (l = 0; l < lenl; l++){
          for (m = 0; m < lenm; m++){
            for (n = 0; n < lenn; n++){
              /* RESET */
              sqlite3_reset(stmt);
              /* BIND quant (1) */
              dname = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 0), i));
              rc=sqlite3_bind_text(stmt, 1, dname, -1, NULL);
              /* BIND year (2) */
              dname = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 1), j));
              rc=sqlite3_bind_text(stmt, 2, dname, -1, NULL);
              /* BIND unit (3) */
              dname = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 2), k));
              rc=sqlite3_bind_text(stmt, 3, dname, -1, NULL);
              /* BIND season (4) */
              dname = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 3), l));
              rc=sqlite3_bind_text(stmt, 4, dname, -1, NULL);
              /* BIND area (5) */
              dname = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 4), m));
              rc=sqlite3_bind_text(stmt, 5, dname, -1, NULL);
              /* BIND iter (6) */
              dname = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 5), n));
              rc=sqlite3_bind_text(stmt, 6, dname, -1, NULL);
              /* BIND data (7) */
              rc=sqlite3_bind_double(stmt, 7, REAL(Rflq)[i+(leni*j)+(leni*lenj*k)+
                  (leni*lenj*lenk*l)+(leni*lenj*lenk*lenl*m)+
                  (leni*lenj*lenk*lenl*lenm*n)]);
              /* STEP */
              rc=sqlite3_step(stmt);
              /* Can values be inserted? */
              if(rc != SQLITE_DONE) {
                sqlite3_exec(db, "ROLLBACK TRANSACTION;", NULL, NULL, NULL);
                sqlite3_close(db);
                INTEGER(Rval)[0] = 5;
                Rprintf("%i", INTEGER(Rval)[0]);
                return (Rval);
              }
            }
          }
        }
      }
    }
  }
  
  sqlite3_finalize(stmt);

  /* COMMIT ... */
  sqlite3_exec(db, "COMMIT;", NULL, NULL, NULL);

  /* CLOSE */
  sqlite3_close(db);
  
	UNPROTECT(3);

	return (Rval);
} /* }}} */

/* Function SEXP insertFLComp(SEXP Rdbname, SEXP Rname, SEXP Rflc, SEXP Rsnames) {{{ */
SEXP insertFLComp(SEXP Rdbname, SEXP Rname, SEXP Rflc, SEXP Rsnames, SEXP Rstring)
  {
  SEXP Rval = R_NilValue;
  PROTECT(Rval = allocVector(INTSXP, 1));
  Rname = AS_CHARACTER(Rname);
  Rsnames = AS_CHARACTER(Rsnames);
  Rstring = AS_CHARACTER(Rstring);
    
  int rc, i, j, k, l, m, n, r, s, t;
  int leni, lenj, lenk, lenl, lenm, lenn;
  int lens=length(Rsnames);
  int lent=length(Rstring);
  int lenr=LENGTH(GET_SLOT(Rflc, install("range")));
  sqlite3 *db;
  sqlite3_stmt *stmt;
  const char *tail, *dname;
  char *sql;

  INTEGER(Rval)[0] = 1;

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
 
  /* DROP TABLES */
  sql = sqlite3_mprintf("DROP TABLE IF EXISTS \"%q_meta\";", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  /* Can meta table be dropped? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
 
  sql = sqlite3_mprintf("DROP TABLE IF EXISTS \"%q_slots\";", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  /* Can slots table be dropped? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
 
  sql = sqlite3_mprintf("DROP TABLE IF EXISTS \"%q_data\";", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  /* Can data table be dropped? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
 
  sql = sqlite3_mprintf("DROP TABLE IF EXISTS \"%q_range\";", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  /* Can range table be dropped? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
 
  /* CREATE meta TABLE */
  sql = sqlite3_mprintf("CREATE TABLE \"%q_meta\" ('rowId' INTEGER NOT NULL PRIMARY KEY, 'field' CHAR DEFAULT 'quant', 'value' CHAR DEFAULT 'quant');", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, 0, 0, 0);
  /* Can meta table be created? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
 
  /* CREATE slots TABLE */
  sql = sqlite3_mprintf("CREATE TABLE \"%q_slots\" ('rowId' INTEGER NOT NULL PRIMARY KEY, 'slot' CHAR, 'units' CHAR DEFAULT 'NA', 'quant' INTEGER DEFAULT '1', 'year' INTEGER DEFAULT '1', 'unit' INTEGER DEFAULT '1', 'season' INTEGER DEFAULT '1', 'area' INTEGER DEFAULT '1', 'iter' INTEGER DEFAULT '1', UNIQUE (slot));", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, 0, 0, 0);
  /* Can slots table be created? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
 
  /* CREATE data TABLE */
  sql = sqlite3_mprintf("CREATE TABLE \"%q_data\" ('rowId' INTEGER NOT NULL PRIMARY KEY, 'slot' CHAR, 'quant' CHAR DEFAULT 'all', 'year' CHAR DEFAULT '1' CHECK (year > 0), 'unit' CHAR DEFAULT 'unique', 'season' CHAR DEFAULT 'all', 'area' CHAR DEFAULT 'unique', 'iter' INTEGER DEFAULT '1' CHECK (iter > 0), 'data' DOUBLE DEFAULT '0.0', UNIQUE (slot, quant, year, unit, season, area, iter));", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, 0, 0, 0);
  /* Can data table be created? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
 
  /* CREATE range TABLE */
  sql = sqlite3_mprintf("CREATE TABLE \"%q_range\" ('rowId' INTEGER NOT NULL PRIMARY KEY, 'field' CHAR DEFAULT 'quant', 'value' INTEGER DEFAULT '0');", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_exec(db, sql, 0, 0, 0);
  /* Can range table be created? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
 
  /* name and desc */
  sql = sqlite3_mprintf("INSERT INTO \"%q_meta\" (field, value) VALUES ('name', '%q');", CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(GET_SLOT(Rflc, install("name")), 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  /* Can name in meta be inserted? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
 
  sql = sqlite3_mprintf("INSERT INTO \"%q_meta\" (field, value) VALUES ('desc', '%q');", CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(GET_SLOT(Rflc, install("desc")), 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  /* Can desc in meta be inserted? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);
  
  /* class */
  sql = sqlite3_mprintf("INSERT INTO \"%q_meta\" (field, value) VALUES ('class', '%q');", CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(GET_CLASS(Rflc), 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  /* Can class in meta be inserted? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  sqlite3_free(sql);

  /* string slots, if any */
  if(lent > 1) {
    sql = sqlite3_mprintf("INSERT INTO \"%q_meta\" (field, value) VALUES (?,?);", CHAR(STRING_ELT(Rname, 0)));
    rc = sqlite3_prepare_v2(db, sql, (int)strlen(sql), &stmt, &tail);
    /* Can INSERT@meta statement be prepared? */
    if(rc != SQLITE_OK) {
       Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
       sqlite3_close(db);
       UNPROTECT(1);
       return (Rval);
    }
  
    for (t=0; t < lent; t++) {
      sqlite3_reset(stmt);

      /* BIND slot name (1) */
      dname = CHAR(STRING_ELT(Rstring, t));
      rc=sqlite3_bind_text(stmt, 1, dname, -1, NULL);
      /* Can slot names be bound to statement? */
      if(rc != SQLITE_OK) {
        Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
        sqlite3_close(db);
        UNPROTECT(1);
        return (Rval);
      }
      /* BIND slot value (2) */
Rprintf("%s\n", STRING_ELT(GET_SLOT(Rflc, install(CHAR(STRING_ELT(Rstring, t)))), 0));
/*      dname = CHAR(STRING_ELT(GET_SLOT(Rflc, install(CHAR(STRING_ELT(Rstring, t)))), 0)); */
      rc=sqlite3_bind_text(stmt, 2, dname, -1, NULL);
      /* Can slot values be bound to statement? */
      if(rc != SQLITE_OK) {
        Rprintf("%s", sqlite3_errmsg(db));
        sqlite3_close(db);
        INTEGER(Rval)[0] = 14;
        UNPROTECT(1);
        return (Rval);
      }
      rc=sqlite3_step(stmt);
      /* Can step be carried out? */
      if(rc != SQLITE_DONE) {
        Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
        sqlite3_close(db);
        UNPROTECT(1);
        return (Rval);
      }
    }
    sqlite3_finalize(stmt);
    sqlite3_free(sql);
  }
 

  /* range */
  sql = sqlite3_mprintf("INSERT INTO \"%q_range\" (field, value) VALUES (?,?);", CHAR(STRING_ELT(Rname, 0)));
  rc = sqlite3_prepare_v2(db, sql, (int)strlen(sql), &stmt, &tail);
  /* Can INSERT@range statement be prepared? */
  if(rc != SQLITE_OK) {
     Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
     sqlite3_close(db);
     UNPROTECT(1);
     return (Rval);
  }
  
  for (r=0; r < lenr; r++)
  {
    sqlite3_reset(stmt);
    /* BIND range name (1) */
    dname = CHAR(STRING_ELT(GET_NAMES(GET_SLOT(Rflc, install("range"))), r));
    rc=sqlite3_bind_text(stmt, 1, dname, -1, NULL);
    /* Can range names be bound to statement? */
    if(rc != SQLITE_OK) {
       Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
       sqlite3_close(db);
       UNPROTECT(1);
       return (Rval);
    }
    /* BIND range value (2) */
    rc=sqlite3_bind_double(stmt, 2, REAL(GET_SLOT(Rflc, install("range")))[r]);
    /* Can range values be bound to statement? */
    if(rc != SQLITE_OK) {
      Rprintf("%s", sqlite3_errmsg(db));
      sqlite3_close(db);
      INTEGER(Rval)[0] = 14;
      UNPROTECT(1);
      return (Rval);
    }
    rc=sqlite3_step(stmt);
    /* Can step be carried out? */
    if(rc != SQLITE_DONE) {
       Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
       sqlite3_close(db);
       UNPROTECT(1);
       return (Rval);
    }
  }
  sqlite3_finalize(stmt);
  sqlite3_free(sql);
  
  /* FLQuant SLOTS */
  for (s = 0; s < lens; s++){
 
    /* VARS */
    SEXP sQuant   = PROTECT(GET_SLOT(Rflc, install(CHAR(STRING_ELT(Rsnames, s)))));
    SEXP Quant    = PROTECT(GET_SLOT(sQuant, install(".Data"))),
         units    = PROTECT(GET_SLOT(sQuant, install("units"))),
         dims     = GET_DIM(sQuant),
         dimnames = GET_DIMNAMES(sQuant),
         names    = GET_NAMES(dimnames);

    /* INSERT meta */
    if (s==0)
    {
      /* quant */
      sql = sqlite3_mprintf("INSERT INTO \"%q_meta\" (field, value) VALUES ('quant', '%q');", CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(names, 0)));
      rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
      /* Can quant in meta be inserted? */
      if(rc != SQLITE_OK) {
        Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
        sqlite3_close(db);
        UNPROTECT(1);
        return (Rval);
      }
      sqlite3_free(sql);
    }
  
    /* slots */
    sql = sqlite3_mprintf("INSERT INTO \"%q_slots\" (slot, units, quant, year, unit, season, area, iter) VALUES ('%q', '%q', '%i', '%i', '%i', '%i', '%i', '%i');", CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(Rsnames, s)), CHAR(STRING_ELT(units, 0)), INTEGER(dims)[0], INTEGER(dims)[1], INTEGER(dims)[2], INTEGER(dims)[3], INTEGER(dims)[4], INTEGER(dims)[5]);
    rc = sqlite3_exec(db, sql, 0, 0, 0);
    /* Can slots be inserted? */
    if(rc != SQLITE_OK) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_close(db);
      UNPROTECT(1);
      return (Rval);
    }
    sqlite3_free(sql);
  
    /* data ESTATEMENT */
    sql = sqlite3_mprintf("INSERT INTO \"%q_data\" (slot, quant, year, unit, season, area, iter, data) VALUES (?,?,?,?,?,?,?,?);", CHAR(STRING_ELT(Rname, 0)));
    rc = sqlite3_prepare_v2(db, sql, (int)strlen(sql), &stmt, &tail);
    if(rc != SQLITE_OK) {
      Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
      sqlite3_close(db);
      UNPROTECT(1);
      return (Rval);
    }

    leni = INTEGER(dims)[0];
    lenj = INTEGER(dims)[1];
    lenk = INTEGER(dims)[2];
    lenl = INTEGER(dims)[3];
    lenm = INTEGER(dims)[4];
    lenn = INTEGER(dims)[5];
    
    /* BIND values */
    for (n = 0; n < lenn; n++){
      for (m = 0; m < lenm; m++){
        for (l = 0; l < lenl; l++){
          for (k = 0; k < lenk; k++){
            for (j = 0; j < lenj; j++){
              for (i = 0; i < leni; i++){
                /* RESET */
                sqlite3_reset(stmt);
                /* BIND slot (1) */
                dname = CHAR(STRING_ELT(Rsnames, s));
                rc=sqlite3_bind_text(stmt, 1, dname, -1, NULL);
                /* BIND quant (2) */
                dname = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 0), i));
                rc=sqlite3_bind_text(stmt, 2, dname, -1, NULL);
                /* BIND year (3) */
                dname = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 1), j));
                rc=sqlite3_bind_text(stmt, 3, dname, -1, NULL);
                /* BIND unit (4) */
                dname = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 2), k));
                rc=sqlite3_bind_text(stmt, 4, dname, -1, NULL);
                /* BIND season (5) */
                dname = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 3), l));
                rc=sqlite3_bind_text(stmt, 5, dname, -1, NULL);
                /* BIND area (6) */
                dname = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 4), m));
                rc=sqlite3_bind_text(stmt, 6, dname, -1, NULL);
                /* BIND iter (7) */
                dname = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 5), n));
                rc=sqlite3_bind_text(stmt, 7, dname, -1, NULL);
                /* BIND data (8) */
                rc = sqlite3_bind_double(stmt, 8, REAL(Quant)[i+(leni*j)+(leni*lenj*k)+
                    (leni*lenj*lenk*l)+(leni*lenj*lenk*lenl*m)+
                    (leni*lenj*lenk*lenl*lenm*n)]);
                /* STEP */
                rc = sqlite3_step(stmt);
                /* Can values be inserted? */
                if(rc != SQLITE_DONE) {
                  Rprintf("%i: %s\n %s\n", rc, sqlite3_errmsg(db), sql);
                  sqlite3_close(db);
                  UNPROTECT(1);
                  return (Rval);
                }
              }
            }
          }
        }
      }
    }
    UNPROTECT(3);
  }
 
  /* COMMIT ... */
  sqlite3_exec(db, "COMMIT;", NULL, NULL, NULL);

  /* INDEX */
  sql = sqlite3_mprintf("CREATE UNIQUE INDEX %q.data ON %q_data (slot,quant,year);", CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(Rdbname, 0)));
  rc = sqlite3_exec(db, sql, NULL, NULL, NULL);
  sql = sqlite3_mprintf("CREATE UNIQUE INDEX %q.slots ON %q_slots (slot);", CHAR(STRING_ELT(Rname, 0)), CHAR(STRING_ELT(Rdbname, 0)));
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
