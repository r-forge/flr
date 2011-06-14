/*
 * exports.h
 * SQLiteFL/src/exports.h
 *
 * Author : Iago Mosqueira <iago.mosqueira@cefas.co.uk> Cefas, UK
 * $Id: exports.h 363 2009-10-20 12:40:39Z imosqueira $
 *
 */

#include <R.h>
#include <Rdefines.h>

/* ricker.c */
double rifunc(double *,double *,double,double,double,double *,int);
SEXP rickerBayes(SEXP Rrec, SEXP Rssb, SEXP Rpriors, SEXP Rinit, SEXP RnIter, SEXP Rburnin, SEXP Rthin, SEXP Rchains, SEXP Rmvar);
