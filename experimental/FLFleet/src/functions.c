/*
 * select.c = 
 * SQLiteFL/src/select.c
 *
 * Author : Iago Mosqueira <iago.mosqueira-sanchez@jrc.ec.europa.eu> EC JRC, Ispra
 * $Id: select.c 363 2009-10-20 12:40:39Z imosqueira $
 *
 */

#include "functions.h"

/* unique: return unique values from data.frame column (vector) */

/* getListElement */
SEXP getListElement(SEXP list, const char *str)
{
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);

  for (R_len_t i = 0; i < length(list); i++) {
	  if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
	    elmt = VECTOR_ELT(list, i);
	    break;
    }
	}
  return elmt;
}

/* checkNamesFLQ*/
int checkNamesFLQ(SEXP names)
{
  int i, j, val=0;
  SEXP validnames = NEW_CHARACTER(7);
  SET_ELEMENT(validnames, 1, mkChar("year"));
  SET_ELEMENT(validnames, 2, mkChar("unit"));

  /* quant, year, unit, season, are, iter, data */
  for(i=0; i<length(names); i++) {
    Rprintf("%i", 1);
  }


  return(val)
}
