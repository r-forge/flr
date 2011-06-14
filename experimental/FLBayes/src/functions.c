/*
 * functions.c = 
 *
 * Author : Iago Mosqueira <imosqueira@suk.azti.es> AZTI Tecnalia 
 * Last Change: 22 Jan 2010 23:45
 * $Id: $
 *
 */

/* double rifunc(R, S, S0, lalpha, beta, sigmar, priors, T) {{{ */
double rifunc(double R[],double S[],double lalpha,double beta,double sigmar,double priors[],int T)
{
  int t;
  double retval,lkhd,prior;

  /* calculate the likelihood */
  
  lkhd = -0.5*(double)T*log(2.*M_PI*sigmar);
  for(t=0;t<T;t++) lkhd -= 1./(2.*sigmar)*(log(R[t])-log(S[t])-lalpha+beta*S[t])*(log(R[t])-log(S[t])-lalpha+beta*S[t]);

  /* calculate the prior */

  prior = log(dunif(beta,priors[2],priors[3],FALSE));
  
  retval = lkhd + prior;

  return retval;
} /* }}} */
