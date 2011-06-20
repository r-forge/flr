/*
 * select.c = 
 * SQLiteFL/src/select.c
 *
 * Author : Iago Mosqueira <iago.mosqueira-sanchez@jrc.ec.europa.eu> EC JRC, Ispra
 * $Id: select.c 363 2009-10-20 12:40:39Z imosqueira $
 *
 */

#include "exports.h"

SEXP asFLQuant(SEXP Rdf, SEXP Rquant)
{
  /* df: data, quant, year, unit, season, area, iter*/

  SEXP Rval, Quant;
  int i;
	
  PROTECT(Rval = allocVector(INTSXP, 1));
  INTEGER(Rval)[0] = 1;
  
  /* output FLQuant */
  PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));
  
  /* column names */
  SEXP colnames = GET_NAMES(Rdf);
  
  /* check colnames are right */
  /* check what colnames exist */

  /* No. of rows */

  /* */
  SEXP rownames = GET_ROWNAMES(Rdf);
  SEXP units = GET_ATTR(Rdf, mkChar("units"));
  SEXP dim = LENGTH(GET_DIM(Rdf));

  SEXP vec = getListElement(Rdf, mkChar("year"));

  for (i=0; i<6; i++) {
 //   Rprintf("%s", CHAR(STRING_ELT(colnames, i)));
  }

  /* array */

  /* fill in array */


  /* get dim & dimnames */

  /*  */
  /*  */
  /*  */
  /*  */
  UNPROTECT(2);

  return(vec);
}

