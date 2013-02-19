//=========================================================================================================================
// File:        pella.tpl
// Model:       Pella-Tomlinson model, with Binit=k*a
// Parameters:  r, k, a, p, q, s
// Fitted data: Abundance index
// Likelihood:  Log-transformed normal
// References:  Polacheck et al. (1993)
// Notes:       q and s are free parameters, to allow full uncertainty in MCMC analysis
// History:      9 Mar 2010 Arni Magnusson created, to benchmark against R optimizers
//               7 Oct 2010 Arni Magnusson improved string handling and comments
//=========================================================================================================================
// Implementation notes
//   Abundance index may not exist for all years
//   Vectors that include all years: B, C
//   Vectors that include abundance index years: I, Ifit, X
//   X links long and short vectors
//=========================================================================================================================

GLOBALS_SECTION
  #include "admodel.h"
  #include <string>
  #include <dnorm.cpp> //include functions from custom library

  using std::string;

  const double pi = 3.141592654;
  int mcmc_iteration = 0;
  int phz;    // phase
  double lb;  // lower bound
  double ub;  // upper bound

  ofstream mcmc_par("mcmc_par.csv");
  ofstream mcmc_bio("mcmc_bio.csv");
  ofstream priors("prr.txt");
  ofstream bounds("bnd.txt");

DATA_SECTION
  // Read data file
  init_int nc
  init_matrix Cdata(1,2,1,nc)  // Year | C
  init_int ni
  init_int nIdx
  init_matrix Idata(1,3,1,ni)  // Year | I
  
  // Vectors
  ivector Cyear(1,nc)
  vector      C(1,nc)

  ivector Iyear(1,ni)
  ivector   Idx(1,ni)
  vector      I(1,ni)		
  ivector     X(1,ni)  // years with abundance index: 1995 | 1998 | ...
  vector   logI(1,ni)
  
  // Constants
  number halfnlog2pi
  

  number nReg

  // Switch to control file
  !! string run_name = string(adprogram_name);
  !! if(option_match(argc,argv,"-ind") > -1){
  !!   run_name = argv[option_match(argc,argv,"-ind") + 1];
  !!   run_name = run_name.substr(0, run_name.rfind("."));}
  
  // Read control file (phase, lower, upper, init)
  !! change_datafile_name((adstring)run_name.c_str() + ".ctl");

  init_vector logr_plui(1,4)
  init_vector logk_plui(1,4)
  init_vector logp_plui(1,4)
  init_vector loga_plui(1,4)

  init_ivector qPh(1,nIdx)
  init_vector  qLo(1,nIdx)
  init_vector  qHi(1,nIdx)
  init_vector  qPr(1,nIdx)
  
  init_ivector sPh(1,nIdx)
  init_vector  sLo(1,nIdx)
  init_vector  sHi(1,nIdx)
  init_vector  sPr(1,nIdx)
 
  // Switch to prior file
  !! change_datafile_name((adstring)run_name.c_str() + ".prr");

  // Read prior file (wt, mean, sd)
  init_vector r_prior(1,4)
  init_vector k_prior(1,4)
  init_vector p_prior(1,4)
  init_vector a_prior(1,4)
  init_vector q_prior(1,4)
  init_vector s_prior(1,4)
  
PARAMETER_SECTION
  // Estimated
  !! phz = (int) logr_plui[1];
  !! lb  =       logr_plui[2];
  !! ub  =       logr_plui[3];
  init_bounded_number logr(lb,ub,phz)
  !! phz = (int) logk_plui[1];
  !! lb  =       logk_plui[2];
  !! ub  =       logk_plui[3];
  init_bounded_number logk(lb,ub,phz)
  !! phz = (int) loga_plui[1];
  !! lb  =       loga_plui[2];
  !! ub  =       loga_plui[3];
  init_bounded_number loga(lb,ub,phz)
  !! phz = (int) logp_plui[1];
  !! lb  =       logp_plui[2];
  !! ub  =       logp_plui[3];
  init_bounded_number logp(lb,ub,phz)
  
  init_bounded_number_vector logq(1,nIdx,qLo,qHi,qPh)  
  init_bounded_number_vector logs(1,nIdx,sLo,sHi,sPh)  

  // Derived
  sdreport_number r
  sdreport_number k
  sdreport_number a
  sdreport_number p
  sdreport_vector q(1,nIdx)
  sdreport_vector s(1,nIdx)
  sdreport_number Ynow
  sdreport_number Bnow
  sdreport_number Hnow
  sdreport_number MSY
  sdreport_number BMSY
  sdreport_number HMSY
  sdreport_number YMSY
  sdreport_number BBMSY
  sdreport_number HHMSY
  sdreport_number BRatio
  sdreport_number HRatio
  sdreport_number slopeB;
  sdreport_number slopeH;

  number SS
  number s_
  vector nii(1,2)
  
  number _BRatio
  number _HRatio

  number  xy  
  number  x   
  number  y 
  number  xx

  // Updated
  vector B(1,nc+1)
  vector Bfit(1,ni)
  vector Ifit(1,ni)
  vector  RSS(1,nIdx)
  
  // Report
  matrix summary(1,nc,1,7)  // Year | B | C | I | Ifit | stockHat | stock.

  // likelihood profile
  likeprof_number lpr 
  
  // Objfun
  objective_function_value neglogL

PRELIMINARY_CALCS_SECTION
  halfnlog2pi = 0.5*ni*log(2*pi);
  nReg=5;

  // Data
  Cyear = (ivector) row(Cdata,1);
  C     =           row(Cdata,2);
  
  Iyear = (ivector) row(Idata,1);
  I     =           row(Idata,2);
  Idx   = (ivector) row(Idata,3);
  logI  = log(I);

  X     = Iyear - Cyear[1] + 1;
  
  // Parameters
  logr    = logr_plui[4];
  logk    = logk_plui[4];
  loga    = loga_plui[4];
  logp    = logp_plui[4];

  for (int j=1; j<=nIdx; j++){
    logq(j) = qPr[j];
    logs(j) = sPr[j];}

  // likelihood profile 
  lpr   =mfexp(logr);
  
PROCEDURE_SECTION
  get_fit();
  get_neglogL();
  if(mceval_phase())
    write_mcmc();

  // likelihood profile 
  lpr=mfexp(logr);
  
REPORT_SECTION
  write_bounds();
  write_priors();
  summary.initialize();
  get_summary();
 
  report<<setprecision(12)
        <<"# r"      <<endl<<r      <<endl
        <<"# k"      <<endl<<k      <<endl
        <<"# b0"     <<endl<<a      <<endl
        <<"# p"      <<endl<<p      <<endl
        <<"# q"      <<endl<<q      <<endl
        <<"# s"      <<endl<<s      <<endl
        <<"# RSS"    <<endl<<RSS    <<endl
        <<"# neglogL"<<endl<<neglogL<<endl<<endl;
  report<<setprecision(12)
        <<"# Model summary"<<endl
        <<" year stock catch index hat stockHat stock."<<endl
        <<summary<<endl;

FUNCTION get_fit
  r = mfexp(logr);
  k = mfexp(logk);
  a = mfexp(loga);
  p = mfexp(logp);
 
 
  for (int j=1; j<=nIdx; j++){
    q[j]   = mfexp(logq[j]);
    s[j]   = mfexp(logs[j]);
    }
  
  B[1] = k*a;
  for(int t=1; t<=nc; t++)
    B[t+1] = sfabs(B[t] + r/p*B[t]*(1-pow(B[t]/k,p)) - C[t]);
  
  for (int j=1; j<=ni; j++)
     //Ifit[j] = B(X[j])*q(Idx[j]);
     Ifit[j] = 0.5*(B(X[j])+B(X[j]+1))*q(Idx[j]);

  Ynow =C[nc];
  Hnow =C[nc]/B[nc];
  Bnow =B[nc];
  MSY  =r*k*pow(1/(1+p),1/p+1);
  BMSY =(k*pow((1/(1+p)),(1/p)));
  HMSY =(r*k*pow((1/(1+p)),(1/p+1))/ k*pow((1/(1+p)),(1/p)));
  YMSY =C[nc]	/MSY;
  BBMSY=Bnow/BMSY;
  HHMSY=Hnow/HMSY;
  
   BRatio=0.0;
   HRatio=0.0;
  _BRatio=0.0;
  _HRatio=0.0;
  
  for (int i=nc; i>nc-3; i--){
    BRatio+=B[i];
    HRatio+=C[i]/B[i];
    
    _BRatio+=B[i-3];
    _HRatio+=C[i-3]/B[i-3];
    }
 BRatio=BRatio/_BRatio;
 HRatio=HRatio/_HRatio;

  xy=0.0; 
  x =0.0;
   y=0.0; 
  xx=0.0; 
  for (int i=nc; i>nc-nReg; i--){
    x +=i;
    xx+=i*i;  
    y +=B[i];  
    xy+=i*B[i];  
    }
  slopeB = (nReg*xy - x*y)/(nReg*xx - x*2.0);
   
  xy=0.0; 
  x =0.0;
   y=0.0; 
  xx=0.0; 
  for (int i=nc; i>nc-nReg; i--){
    x +=i;
    xx+=i*i;  
    y +=C[i]/B[i];  
    xy+=i*C[i]/B[i];  
    }
  slopeH = (nReg*xy - x*y)/(nReg*xx - x*2.0);
   

FUNCTION get_neglogL
 
  neglogL = halfnlog2pi;
  for (int j=1; j<=ni; j++){	
     s_       = mfexp(logs[Idx[j]]);
     neglogL += log(s_)   
             +  pow(log(I[j])-log(Ifit[j]),2.0)/(2*s_*s_);
     }

  //neglogL = halfnlog2pi + ni*log(s(1)) + RSS[1]/(2*s(1)*s(1));
  
  // weighted likelihood priors
  if (r_prior[1]>1) neglogL -= dnorm(r, r_prior[2], r_prior[3])/dnorm(r_prior[2], r_prior[2], r_prior[3]);
  if (k_prior[1]>1) neglogL -= dnorm(k, k_prior[2], k_prior[3])/dnorm(k_prior[2], k_prior[2], k_prior[3]);
  if (p_prior[1]>1) neglogL -= dnorm(p, p_prior[2], p_prior[3])/dnorm(p_prior[2], p_prior[2], p_prior[3]);
  if (a_prior[1]>1) neglogL -= dnorm(a, a_prior[2], a_prior[3])/dnorm(a_prior[2], a_prior[2], a_prior[3]);
  //for (i=1; i<=nIdx; i++){
  //  neglogL += q_prior[i]*dnorm(q[i],    q_prior[i,2],     q_prior[i,3]);
  //  neglogL += s_prior[i]*dnorm(s[i],    s_prior[i,2],     s_prior[i,3]);}
   

FUNCTION get_summary
  summary.colfill(1,(dvector)Cyear);
  summary.colfill(2,B);
  summary.colfill(3,C);

  for(int i=1; i<=ni; i++)  // allow missing years in abundance index
    {
    summary(X[i],4) = I[i];
    summary(X[i],5) = Ifit[i];
    summary(X[i],6) = Ifit[i]/q(Idx(i));
    summary(X[i],7) = (B[X[i]]+B[X[i]+1])/2.0;
    }

FUNCTION write_mcmc
  mcmc_iteration++;
  // Parameters
  if(mcmc_iteration == 1){
    mcmc_par<<"neglogL,r,k,b0,p,q,s"<<endl;
    mcmc_par<<neglogL<<","
          <<r      <<","
          <<k      <<","
          <<a      <<","
          <<p      <<","
          <<q      <<","
          <<s  <<endl;}
  // Biomass
  if(mcmc_iteration == 1){
    mcmc_bio<<Cyear[1];
    for(int t=2; t<=nc; t++)
      mcmc_bio<<","<<Cyear[t];
     mcmc_bio<<endl;
     }

  mcmc_bio<<B[1];
  for(int t=2; t<=nc; t++)
    mcmc_bio<<","<<B[t];
  mcmc_bio<<endl;

FUNCTION write_priors
  priors<<r_prior<<endl;
	
FUNCTION write_bounds
  bounds<<logq
        <<logs
        <<q	
        <<s<<endl;	

TOP_OF_MAIN_SECTION

  arrmblsize = 40000000L;
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(3000000);
  gradient_structure::set_CMPDIF_BUFFER_SIZE(200000);
  gradient_structure::set_MAX_NVAR_OFFSET(10000);

