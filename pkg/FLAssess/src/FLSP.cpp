 /*
 * FLSP.cpp.c = C functions for calculating the Likelihood when fitting the SP model
 *
 * Author : Finlay Scott <finaly.scott@cefas.co.uk>
 *
 * Last Change: 21 May 2008 16:25
 * $Id: FLSP.cpp,v 1.3 2008/06/20 11:42:31 imosqueira Exp $
 *
 */

#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

#ifdef WIN32
   #define SEXPDLLExport __declspec(dllexport) SEXP __cdecl
#else
   #define SEXPDLLExport SEXP
#endif

// The Pellatom function
extern "C" SEXPDLLExport PellatomC(SEXP xCatch, SEXP xdelta, SEXP xmpar, SEXP xParam)
{
// xParam[0] = Q, [1] = r, [2] = K
   SEXP biomass = R_NilValue;
   PROTECT(biomass = Rf_duplicate(xCatch));
   REAL(biomass)[0] = REAL(xdelta)[0]; // load first year biomass with delta
   int i;
   for (i=1; i<LENGTH(xCatch); i++)
   {
      REAL(biomass)[i] = REAL(biomass)[i-1] + REAL(xParam)[1] * REAL(biomass)[i-1] * (1 - pow(REAL(biomass)[i-1],REAL(xmpar)[0]-1)) - REAL(xCatch)[i-1]/REAL(xParam)[2];
      if(REAL(biomass)[i] <= 0) REAL(biomass)[i] = 1e-9;
   }
   for (i=0; i<LENGTH(xCatch); i++)
      REAL(biomass)[i] = REAL(biomass)[i] * REAL(xParam)[0];

   UNPROTECT(1);
   return biomass;
}

// Another Pellatom function - called by LogLik function below
void pellatom(SEXP xCatch, double *biomass, double* Para, double mpar, double delta)
{
// Para[0] = Q, [1] = r, [2] = K - different order to pellatom function above
   biomass[0] = delta;
   int i;
   for (i=1; i<LENGTH(xCatch); i++)
   {
      biomass[i] = biomass[i-1] + Para[1] * biomass[i-1] * (1 - pow(biomass[i-1],mpar-1)) - REAL(xCatch)[i-1]/Para[2];
      if(biomass[i] <= 0) biomass[i] = 1e-9;
   }
   for (i=0; i<LENGTH(xCatch); i++)
      biomass[i] = biomass[i] * Para[0];
}


// Returns the log likelihood - used by MLE in R
extern "C" SEXPDLLExport loglPellaTomC(SEXP xParam, SEXP xmpar,SEXP xdelta,
  SEXP xCatch, SEXP xIndex)
// xParam[0] = Q, [1] = r, [2] = K, [3] =  sigma2
{
   SEXP xll = R_NilValue;
   PROTECT(xll = NEW_NUMERIC(1));
   int i = 0;
   const double pi = 3.14159265359;
   double ll = 0;
   double mpar = REAL(xmpar)[0];
   double delta = REAL(xdelta)[0];
   double* mean = new double[LENGTH(xCatch)];
   double* Para = new double[4];
   for (i=0; i<4; i++)
     Para[i] = REAL(xParam)[i];

   pellatom(xCatch, mean, Para, mpar, delta);

   double frontbit = 1 / (pow(2*pi,0.5)*sqrt(Para[3]));

   for (i = 0; i<LENGTH(xCatch) ; i++)
      ll += log(frontbit) +(-1* pow(log(REAL(xIndex)[i]/mean[i]),2) / (2 * Para[3]));

   REAL(xll)[0] = ll;

   delete[] mean;
   delete[] Para;

   UNPROTECT(1);
   return xll;
}
