/*
 * exports.h
 * SQLiteFL/src/exports.h
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas, UK
 * $Id: exports.h 913 2011-03-22 10:20:01Z imosqueira $
 *
 */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <string.h>

/*  */
SEXP asFLQuant(SEXP Rdf, SEXP Rquant);
