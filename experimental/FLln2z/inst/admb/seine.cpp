 #include <time.h>
  char  dtstring[12];
  char  tmstring[6];
#include <admodel.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <seine.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  NBreaks.allocate("NBreaks");
  NFYear.allocate("NFYear");
  NXYear.allocate("NXYear");
 NYears = NXYear - NFYear + 1;
  ObsLength.allocate(1,NYears,"ObsLength");
  SampleSize.allocate(1,NYears,"SampleSize");
  KParm.allocate("KParm");
  LInf.allocate("LInf");
  Lc.allocate("Lc");
  zguess.allocate(1,NBreaks+1,1,2,"zguess");
  yguess.allocate(1,NBreaks,1,2,"yguess");
  sigma_init.allocate("sigma_init");
  stepsize.allocate("stepsize");
  casenum.allocate("casenum");
  zphase.allocate(1,NBreaks+1);
  yphase.allocate(1,NBreaks);
  zinit.allocate(1,NBreaks+1);
  yinit.allocate(1,NBreaks);
    zinit = column(zguess,1);
    yinit = column(yguess,1);     
	for (int iki=1; iki<=NBreaks; iki++){
	   zphase(iki)= (int) zguess(iki,2);
	   yphase(iki)= (int) yguess(iki,2);}
	zphase(NBreaks+1)= (int) zguess(NBreaks+1,2);
        struct tm *Today;
        time_t xstart;
        time(&xstart);
        Today = localtime(&xstart);
        strftime(dtstring,12,"%d_%b_%Y",Today);
        strftime(tmstring,6,"%H:%M",Today);
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  Z.allocate(1,NBreaks+1,0.001,5.0,zphase,"Z");
 double lb=NFYear;
 double ub=NXYear;
  ChangeYear.allocate(1,NBreaks,lb,ub,yphase,"ChangeYear");
  Sigma.allocate(0,2000.,2,"Sigma");
  negloglike.allocate("negloglike");
  AIC.allocate("AIC");
  #ifndef NO_AD_INITIALIZE
  AIC.initialize();
  #endif
  xs.allocate("xs");
  #ifndef NO_AD_INITIALIZE
  xs.initialize();
  #endif
  xt.allocate("xt");
  #ifndef NO_AD_INITIALIZE
  xt.initialize();
  #endif
  xn.allocate("xn");
  #ifndef NO_AD_INITIALIZE
  xn.initialize();
  #endif
  xk.allocate("xk");
  #ifndef NO_AD_INITIALIZE
  xk.initialize();
  #endif
  prod.allocate("prod");
  #ifndef NO_AD_INITIALIZE
  prod.initialize();
  #endif
  NPar.allocate("NPar");
  #ifndef NO_AD_INITIALIZE
  NPar.initialize();
  #endif
  pi.allocate("pi");
  #ifndef NO_AD_INITIALIZE
  pi.initialize();
  #endif
  pi = 3.14159265;
  NPar = 2.0 * (double) NBreaks + 2.0;
  xn = (double) NYears;
  dy.allocate(1,NBreaks,1,NYears,"dy");
  #ifndef NO_AD_INITIALIZE
    dy.initialize();
  #endif
  a.allocate(1,NBreaks+1,1,NYears,"a");
  #ifndef NO_AD_INITIALIZE
    a.initialize();
  #endif
  r.allocate(1,NBreaks+1,1,NYears,"r");
  #ifndef NO_AD_INITIALIZE
    r.initialize();
  #endif
  s.allocate(1,NBreaks+1,1,NYears,"s");
  #ifndef NO_AD_INITIALIZE
    s.initialize();
  #endif
  xnum.allocate(1,NYears,"xnum");
  #ifndef NO_AD_INITIALIZE
    xnum.initialize();
  #endif
  xden.allocate(1,NYears,"xden");
  #ifndef NO_AD_INITIALIZE
    xden.initialize();
  #endif
  xnsum.allocate(1,NYears,"xnsum");
  #ifndef NO_AD_INITIALIZE
    xnsum.initialize();
  #endif
  PredLength.allocate(1,NYears,"PredLength");
  #ifndef NO_AD_INITIALIZE
    PredLength.initialize();
  #endif
  resid.allocate(1,NYears,"resid");
  #ifndef NO_AD_INITIALIZE
    resid.initialize();
  #endif
  sqresid.allocate(1,NYears,"sqresid");
  #ifndef NO_AD_INITIALIZE
    sqresid.initialize();
  #endif
}

void model_parameters::preliminary_calculations(void)
{

  admaster_slave_variable_interface(*this);
  for (i = 1; i <= NBreaks+1; i++)
     Z(i) = zinit(i);
  for (i = 1; i <= NBreaks; i++)
     ChangeYear(i) = yinit(i); 
  Sigma = sigma_init;
}

void model_parameters::userfunction(void)
{
  calcdy();
  calcpredlength(); 
  resid = ObsLength - PredLength;
  sqresid = square(resid);
  prod = sum(elem_prod(SampleSize,sqresid));
  negloglike = xn * log(2.0 * pi) / 2.0 + xn * log(Sigma) + (0.5 / (Sigma * Sigma) * prod);
  AIC = 2.0 * negloglike + 2.0 * NPar;
}

void model_parameters::calcdy(void)
{
  for (i = 1; i <= NBreaks; i++){
     for (j = 1; j <= NYears; j++){
         k = NFYear + j - 1;
         xk = (double) k;
         dy(i,j) = xk - ChangeYear(i);
         if (dy(i,j) < 0.)
             dy(i,j) = 0.;
         if (i < NBreaks && dy(i,j) > (ChangeYear(i+1) - ChangeYear(i)))
                   dy(i,j) = ChangeYear(i+1) - ChangeYear(i);
         }}
}

void model_parameters::calcpredlength(void)
{
  for (j = 1; j <= NYears; j++){
     xnum(j) = 0.0;
     xden(j) = 0.0;
     xnsum(j) = 0.0;
     for (i = 1; i <= NBreaks + 1; i++){
        if (i == NBreaks+1)
           s(i,j) = 1.0;
        else
           s(i,j) = 1.0 - exp(-(Z(NBreaks+2-i) + KParm) * dy(NBreaks+1-i,j));
        if (i == 1){
           a(i,j) = 1.0;
           r(i,j) = 1.0;}
        else{
           xs = 0.0;
           xt = 0.0;
           for (k = 1; k < i; k++){
              xs += -(Z(NBreaks+2-k) + KParm) * dy(NBreaks+1-k,j);
              xt +=  -Z(NBreaks+2-k) * dy(NBreaks+1-k,j);}
           r(i,j) =  exp(xs);
           a(i,j) =  exp(xt);}
        if (i == NBreaks +1)
           xden(j) += a(i,j) / Z(NBreaks+2-i);
        else
           xden(j) += a(i,j) * (1.0 - exp(-Z(NBreaks+2-i) * dy(NBreaks+1-i,j)))/ Z(NBreaks+2-i);
        xnsum(j) += (1.0 - Lc / LInf) * r(i,j) * s(i,j)/ (Z(NBreaks+2-i) + KParm);
        }}
  xnum = LInf * (xden - xnsum);
  PredLength = elem_div(xnum,xden);
}

void model_parameters::report()
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
  report << "# SEINE Output"       <<endl;
  report << "# npar"               <<endl;
  report <<NPar                    <<endl;
  report << "# nbreak"             <<endl;
  report << NBreaks                <<endl;
  report << "# minyr"              <<endl;
  report << NFYear                 <<endl;
  report << "# maxyr"              <<endl;
  report << NXYear                 <<endl;
  report << "# obs"                <<endl;
  report << ObsLength(1,NYears)    <<endl;
  report << "# n"                  <<endl;
  report << SampleSize(1,NYears)   <<endl;
  report << "# k"                  <<endl;
  report << KParm <<endl;
  report << "# linf"               <<endl;
  report << LInf                   <<endl;
  report << "# lc"                 <<endl;
  report << Lc                     <<endl;
  ivector _zguess(1,NBreaks+1);
  ivector _yguess(1,NBreaks);
  report << "# zguess"             <<endl;
  _zguess = (ivector) column(zguess,1);
  report << _zguess                <<endl;
  report << "# zguess2"            <<endl;
  _zguess = (ivector) column(zguess,2);
  report << _zguess                <<endl;
  report << "# yguess"             <<endl;
  _yguess = (ivector) column(yguess,1);
  report << _yguess                <<endl;
  report << "# yguess2"            <<endl;
  _yguess = (ivector) column(yguess,2);
  report << _yguess                <<endl;
  report << "# sigma"              <<endl;
  report <<  sigma_init            <<endl;
  report << "# step"               <<endl;
  report <<  stepsize              <<endl;
  report << "# casenum"            <<endl;
  report <<  casenum               <<endl;
  report <<endl<<endl;
  report << "# aic"               <<endl;
  report << AIC                   <<endl;
  report << "# aic"               <<endl;
  report << negloglike            <<endl;
  report << "# yinit"             <<endl;
  report << yinit(i)              <<endl;
  report << "# Z"                 <<endl;
  report << Z                     <<endl;
  report << "# sigma"             <<endl;
  report<<Sigma                   <<endl;
  report << "# changeyr"          <<endl;
  report << ChangeYear            <<endl;
  report<< "# obsLen"             <<endl; 
  report<< ObsLength             <<endl; 
  report<< "# hatLen"             <<endl; 
  report<< PredLength              <<endl; 
  report <<"# Residuals"          <<endl; 
  report << resid                 <<endl; 
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
  #if defined(__GNUDOS__) || defined(DOS386) || defined(__DPMI32__)  || \
     defined(__MSVC32__)
      if (!arrmblsize) arrmblsize=150000;
  #else
      if (!arrmblsize) arrmblsize=25000;
  #endif
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
