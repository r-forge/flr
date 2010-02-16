/*
 * rickerBayes.c = Bayesian Ricker stock-recruit using Metropolis-within-Gibbs
 *            sampling.
 *
 * Authors : Richard Hillary <rich.hillary@csiro.au> CSIRO
 *           Iago Mosqueira <iago.mosqueira2cefas.co.uk> Cefas
 *
 * $Id: rickerBayes.c,v 1.4 2006/07/04 12:31:04 rmh1977 Exp $
 * 
 */

#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>
#include "bayes.h"

/* function prototypes */

double rifunc(double *,double *,double,double,double,double *,int);

/* Function SEXP rickerBayes(Rrec, Rssb, Rpriors, RnIter, Rchains, Rmvar) {{{ */
SEXP rickerBayes(SEXP Rrec, SEXP Rssb, SEXP Rpriors, SEXP Rinit, SEXP RnIter, SEXP Rburnin, SEXP Rthin, SEXP Rchains, SEXP Rmvar)
{
	/* initialise variables */
	SEXP Rval = R_NilValue;
	SEXP Rlist = R_NilValue;
	GetRNGstate(); 

	int i=0,c,t;
	int ni = INTEGER(RnIter)[0]*INTEGER(Rthin)[0];
	int nj = 3;
	int T = LENGTH(Rrec);
	int nc = INTEGER(Rchains)[0];
	int nt = INTEGER(Rthin)[0];
	int nb = INTEGER(Rburnin)[0];
	int ntemp,nk;
	
	double *R,*S,*priors;
	double lalpha,beta,sigmar;
	double pival1, pival2, acp[1];
	double dummy1,dummy2,res,bnew;
	double accpt, uvar;
	double bvar = REAL(Rmvar)[0];

	PROTECT(Rlist = NEW_LIST(nc));

	/* data */

	R = REAL(Rrec);
	S = REAL(Rssb);
	priors = REAL(Rpriors);

	/* chains */
	for (c=0;c<nc;c++) {

	ntemp = (int)(ni/nt);
	nk=0;
	PROTECT(Rval = allocMatrix(REALSXP, ntemp, nj));

	/* initialize chain */
	
	for (;;) {
	  lalpha=REAL(Rinit)[0];
	  lalpha=log(lalpha)+rnorm(0,0.01);
	  beta=REAL(Rinit)[1]+runif(0.0,0.5);
	  sigmar =REAL(Rinit)[2] + rnorm(0,0.01);
	  if (beta > 0. && sigmar > 0.) break;
	}
	
	/* chain */
	acp[0]=0.;
	for (i=0; i<ni+nb; i++) {

		/* update lalpha using the conditional posterior */
		
    		for(res=0.,t=0;t<T;t++) res += log(R[t])-log(S[t])+S[t]*beta;
    		dummy1=(priors[0]/priors[1]+res/sigmar)/(1.0/priors[1]+(double)T/sigmar);
    		dummy2=1.0/(1.0/priors[1]+(double)T/sigmar);
    		lalpha=rnorm(dummy1,sqrt(dummy2));
	  
		/* update beta using random-walk Metropolis step */
	
    		for(;;) {
			bnew = beta + rnorm(0.0,sqrt(bvar));
			if(bnew > 0.0) break;
		}
    
    		/* acceptance probability */
    
		pival1 = rifunc(R,S,lalpha,beta,sigmar,priors,T);
		pival2 = rifunc(R,S,lalpha,bnew,sigmar,priors,T);
		accpt = pival2-pival1 < 0. ? pival2-pival1 : 0.;
    
		/* generate a random U[0,1] variate */ 
    
		uvar=log(runif(0.0,1.0));
    
		if(accpt > uvar) {
			if(i >= nb)
				acp[0]++;
			beta = bnew;
		}	
	 
	 
    		/* update the recruit variance 
		 * using the conditional posteriors
		 */

		res=0.;
		for(t=0;t<T;t++) res += 0.5*(log(R[t])-log(S[t])-lalpha+beta*S[t])*(log(R[t])-log(S[t])-lalpha+beta*S[t]);
    		dummy1=rgamma(priors[4]+(double)T/2.0,priors[5]/(1.0+priors[5]*res));
		sigmar=1.0/dummy1;
	
    		/* update the markov chain */
	
		if(i >= nb && i % nt == 0) {
			REAL(Rval)[nk + 0*ntemp] = exp(lalpha);
			REAL(Rval)[nk + 1*ntemp] = beta;
			REAL(Rval)[nk + 2*ntemp] = sigmar;
			nk++;
		}
	}	
	acp[0]/=(double)(ni);
	Rprintf("Acceptance probability for (beta) = %8.6f\n",acp[0]);

	SET_ELEMENT(Rlist, c, Rval);
	
	UNPROTECT(1);

	}

	PutRNGstate(); 

	UNPROTECT(1);

	return (Rlist);

} /* }}} */
