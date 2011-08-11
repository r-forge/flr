// 
//  Gedamke and Hoenig Non Equilibrium Mortality Estimator.  Version 1. Programmed by Todd Gedamke 5/10/08
//
//  Revised Alan Seaver  6/12/08  (Version 1.1)
//
//  Sigma Phase Set = 2  7/15/08   (Version 1.2)
//

GLOBALS_SECTION
 #include <time.h>

  char  dtstring[12];
  char  tmstring[6];

DATA_SECTION

  int NYears;                         // Number of Years of Data

  init_int NBreaks;                   // Number of Breaks
  init_int NFYear;                    // First Year of Data
  init_int NXYear;                    // Last Year of Data

  !! NYears = NXYear - NFYear + 1;

  init_vector ObsLength(1,NYears);    // Observed Mean Lengths
  init_vector SampleSize(1,NYears);   // Sample Sizes

  init_number KParm;                  // VB K Parameter
  init_number LInf;                   // VB L-Infinity

  init_number Lc;                     // Critical Length - Length at first capture

  init_matrix  zguess(1,NBreaks+1,1,2);
  init_imatrix yguess(1,NBreaks,1,2);

  init_number sigma_init;
  init_number stepsize;
  init_number casenum;

  ivector  zphase(1,NBreaks+1);
  ivector  yphase(1,NBreaks);

  vector  zinit(1,NBreaks+1);
  vector  yinit(1,NBreaks);


  !!    zinit = column(zguess,1);
  !!    yinit = column(yguess,1);     
  !!	for (int iki=1; iki<=NBreaks; iki++){
  !!	   zphase(iki)= (int) zguess(iki,2);
  !!	   yphase(iki)= (int) yguess(iki,2);}
  !!	zphase(NBreaks+1)= (int) zguess(NBreaks+1,2);
  
 LOCAL_CALCS

        struct tm *Today;
        time_t xstart;


        time(&xstart);

        Today = localtime(&xstart);

        strftime(dtstring,12,"%d_%b_%Y",Today);

        strftime(tmstring,6,"%H:%M",Today);


 END_CALCS


  int i;
  int j;
  int k;


PARAMETER_SECTION

  init_bounded_number_vector Z(1,NBreaks+1,0.001,5.0,zphase);
 !! double lb=NFYear;
 !! double ub=NXYear;
  init_bounded_number_vector ChangeYear(1,NBreaks,lb,ub,yphase);

  init_bounded_number Sigma(0,2000.,2);


//  sdreport_vector std_z(1,NBreaks+1);
//  sdreport_vector std_y(1,NBreaks	);


  objective_function_value negloglike;

  number AIC;
  number xs;
  number xt;
  number xn;
  number xk;
  number prod;
  number NPar;
  number pi;
 
  !!  pi = 3.14159265;

  !!  NPar = 2.0 * (double) NBreaks + 2.0;

  !!  xn = (double) NYears;


  matrix dy(1,NBreaks,1,NYears);
  matrix  a(1,NBreaks+1,1,NYears);
  matrix  r(1,NBreaks+1,1,NYears);
  matrix  s(1,NBreaks+1,1,NYears);

  vector  xnum(1,NYears);

  vector  xden(1,NYears);
  vector  xnsum(1,NYears);

  vector PredLength(1,NYears);
  vector resid(1,NYears);
  vector sqresid(1,NYears);


PRELIMINARY_CALCS_SECTION

  for (i = 1; i <= NBreaks+1; i++)
     Z(i) = zinit(i);

  for (i = 1; i <= NBreaks; i++)
     ChangeYear(i) = yinit(i); 

  Sigma = sigma_init;

PROCEDURE_SECTION


  calcdy();
  calcpredlength(); 

  resid = ObsLength - PredLength;
  sqresid = square(resid);

  prod = sum(elem_prod(SampleSize,sqresid));

  negloglike = xn * log(2.0 * pi) / 2.0 + xn * log(Sigma) + (0.5 / (Sigma * Sigma) * prod);
  

  AIC = 2.0 * negloglike + 2.0 * NPar;


//  for (i = 1; i <= NBreaks; i++){
//     for (j = 1; j <= NYears; j++){
  
//  std_z=Z;
//  std_y=ChangeYear; 

FUNCTION calcdy

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


FUNCTION calcpredlength

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

REPORT_SECTION

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

