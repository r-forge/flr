 /////////////////////////////////////////////////////////////////////////////////////
DATA_SECTION
 /////////////////////////////////////////////////////////////////////////////////////

 // --------------- read data file --------------------------------------------------------

 // general information

 !! cout << "general information " << endl;
  init_ivector year(1,4)
  init_int year_change
  init_ivector age(1,2)
  init_int variance_scale		        		// controls how variance terms are represented (1=log scale, 2=arithmetic scale)  
  init_int variance_modify           			// + value = add annual modifiers to variance terms, - value = multiply annual modifiers by variance terms
  int year_prehistoric					      // last year of historical period (hist. period is the time span from virgin levels to when data becomes available)
  int year_modern						      // last year of modern period (when data is available)
  int nyears_modern                                   // number of years in the modern period (when F can vary from trend indicated by effort data)
  int nyears_prehistoric                              // number of years in the prehistoric period (when F varies only as function of effort since little data)
  int nyears_past					            // number of years in the prehistoric and modern periods combined
  int nyears_proj                                     // number of years to project into future
  int nyears                                          // number of years in simulation, past and future
  int n_eras                                          // number of time periods when F or q can vary from overall expectations(nyears_modern+1)
  int nyears_b4_change                                // number of years between prehistoric period and the time during the modern period when F is suspected to change (for example, when a moratoroium was instituted)
  int nages                                           // number of age classes
  int nqs                                             // (n)umber of (s)ets of (q) catchability-related parameters
  int nss                                             // (n)umber of (s)ets of (s) selectivity-related parameters
  int nids                                            // (n)umber of (s)ets of (i) index data-related parameters
 LOCAL_CALCS
  year_prehistoric  =year(2); year_modern=year(3);       
  nyears_prehistoric= year_prehistoric - year(1)+1;
  nyears_modern     = year_modern - year_prehistoric; 
  nyears_proj       = year(4) - year_modern;
  nyears_past       = nyears_prehistoric + nyears_modern;
  nyears	        = nyears_past + nyears_proj;
  if(year_change<0 || year_change>year_modern) nyears_b4_change = nyears_past;
  else nyears_b4_change=year_change-year(1);
  n_eras=nyears_modern+1;
  nages=age(2)-age(1)+1;
 END_CALCS

 // spawning information
  init_number spawn_season
  init_vector maturity(1,nages)
  init_vector fecundity_input(1,nages)

 // index (survey) information

 !! cout << "reading indices " << endl;
  init_int     n_index_series
  init_ivector index_pdf(1,n_index_series)
  init_ivector index_units(1,n_index_series)
  init_vector  index_season(1,n_index_series)
  init_ivector index_scale(1,n_index_series)
  init_ivector ivs(1,n_index_series)                  // integer vector indexing the set of variance parameters used by each index of abundance
  init_ivector iqs(1,n_index_series)                  // integer vector indexing the set of q parameters used by each index of abundance
  init_ivector iss(1,n_index_series)                  // integer vector indexing the set of selectivity parameters used by each index of abundance
  init_matrix  index_obs(1,nyears_past,1,n_index_series+1)
  init_matrix  index_cv(1,nyears_past,1,n_index_series+1)
  init_int     effort_pdf
  init_matrix  effort_inp(1,nyears_past,1,2)

 !! cout << "reading projection specifications " << endl;
  init_int    reference_selectivity                    // specifies selectivity vector to use when calculating reference points (1 = fishery vector, 2 = maturity vector
  init_number Bref                                     // specifies biomass reference point
  init_number estimate_r_dev_proj                      // determines whether to estimate recruitment deviations in projections
  init_matrix in_prj(1,nyears_proj,1,3)		       // projection specifications for F

 // --------------- read parameter file ----------------------------------------------------

 !! ad_comm::change_datafile_name("datapoor.prm");
 !! cout << "reading parameter specifications " << endl;
  init_int n_par							 // number of process parameters
  init_ivector n_sets(1,3)					 // number of sets of each type of process parameter
 !! nqs=n_sets(1); nss=n_sets(2); nids=n_sets(3);
  init_matrix par_specs(1,n_par,1,7)			 // specifications for process parameters 
  init_vector f_rho_specs(1,6)				 // specifications for f process error correlation coefficient
  init_vector f_var_specs(1,6)				 // specifications for f process error variance
  init_vector f_dev_specs(1,6)				 // specifications for f process error deviations
  init_vector r_rho_specs(1,6)				 // specifications for r process error correlation coefficient
  init_vector r_var_specs(1,6)				 // specifications for r process error variance
  init_vector r_dev_specs(1,6)				 // specifications for r process error deviations
  init_vector q_rho_specs(1,6)				 // specifications for q process error correlation coefficient
  init_vector q_var_specs(1,6)				 // specifications for q process error variance
  init_vector q_dev_specs(1,6)				 // specifications for q process error deviations

 // --------- derived variables pertaining to parameters that are constant (don't need to be differentiated)----------//

  int i
  int ie
  int n_series
  int n_par_phase
  ivector n_calls(1,1000)
  ivector npf(1,50)
  ivector nature(1,n_par);
  vector  best_guess(1,n_par);
  number  f_rho_best_guess;
  number  f_var_best_guess;
  number  f_dev_best_guess;
  number  r_rho_best_guess;
  number  r_var_best_guess;
  number  r_dev_best_guess;
  number  q_rho_best_guess;
  number  q_var_best_guess;
  number  q_dev_best_guess;
  number  F_best_guess;
  ivector iph(1,n_par);
  int     f_rho_iph;
  int     f_var_iph;
  int     f_dev_iph;
  int     r_rho_iph;
  int     r_var_iph;
  int     r_dev_iph;
  int     q_rho_iph;
  int     q_var_iph;
  int     q_dev_iph;
  int     r_dev_proj_iph;
  int     last_iph;
  ivector pdf(1,n_par);
  int     f_rho_pdf;
  int     f_var_pdf;
  int     f_dev_pdf;
  int     r_rho_pdf;
  int     r_var_pdf;
  int     r_dev_pdf;
  int     q_rho_pdf;
  int     q_var_pdf;
  int     q_dev_pdf;
  int     Trecover;
  vector  cv(1,n_par);
  number  f_rho_cv;
  number  f_var_cv;
  number  f_dev_cv;
  number  r_rho_cv;
  number  r_var_cv;
  number  r_dev_cv;
  number  q_rho_cv;
  number  q_var_cv;
  number  q_dev_cv;
  number  r_dev_proj_cv;
  number spawn_time;
  vector index_time(1,n_index_series);
  vector effort_obs(1,nyears_past);
  vector F_proj_cv(1,nyears_proj);
 LOCAL_CALCS
  cout << "reformat parameter control matrices" << endl;
  if(effort_pdf != 0) effort_obs=column(effort_inp,1); else effort_obs=1.0;
  if(effort_pdf != -1) effort_obs/=max(effort_obs);
  if(nyears_proj > 0) F_proj_cv=column(in_prj,2);
  best_guess=column(par_specs,2);  iph=ivector(column(par_specs,5)); pdf=ivector(column(par_specs,6)); cv=column(par_specs,7);  nature=ivector(column(par_specs,1)); 
  f_rho_best_guess=f_rho_specs(1); f_rho_iph=int(f_rho_specs(4));    f_rho_pdf=int(f_rho_specs(5));    f_rho_cv=f_rho_specs(6);
  f_var_best_guess=f_var_specs(1); f_var_iph=int(f_var_specs(4));    f_var_pdf=int(f_var_specs(5));    f_var_cv=f_var_specs(6);
  f_dev_best_guess=f_dev_specs(1); f_dev_iph=int(f_dev_specs(4));    f_dev_pdf=int(f_dev_specs(5));    f_dev_cv=f_dev_specs(6);
  r_rho_best_guess=r_rho_specs(1); r_rho_iph=int(r_rho_specs(4));    r_rho_pdf=int(r_rho_specs(5));    r_rho_cv=r_rho_specs(6);
  r_var_best_guess=r_var_specs(1); r_var_iph=int(r_var_specs(4));    r_var_pdf=int(r_var_specs(5));    r_var_cv=r_var_specs(6);
  r_dev_best_guess=r_dev_specs(1); r_dev_iph=int(r_dev_specs(4));    r_dev_pdf=int(r_dev_specs(5));    r_dev_cv=r_dev_specs(6);
  q_rho_best_guess=q_rho_specs(1); q_rho_iph=int(q_rho_specs(4));    q_rho_pdf=int(q_rho_specs(5));    q_rho_cv=q_rho_specs(6);
  q_var_best_guess=q_var_specs(1); q_var_iph=int(q_var_specs(4));    q_var_pdf=int(q_var_specs(5));    q_var_cv=q_var_specs(6);
  q_dev_best_guess=q_dev_specs(1); q_dev_iph=int(q_dev_specs(4));    q_dev_pdf=int(q_dev_specs(5));    q_dev_cv=q_dev_specs(6);
  F_best_guess=0.2;
  spawn_time=spawn_season/12.0; index_time=index_season/12.0;
  npf=1; for (int j=1; j<=4;j++) npf(j)=j; // constants and polynomials
  npf(5)=1; npf(6)=2; npf(7)=2; // knife-edge, logistic and gamma selectivity curves
  npf(8)=6; npf(9)=3; // Chapman-Richards and Gompertz growth curves
  npf(12)=2; // power
  for (ie=1; ie<=n_par; ie++) { lower(ie)=par_specs(ie,2); upper(ie)=par_specs(ie,3);}
  last_iph=max(iph);
  if(last_iph<f_rho_iph) last_iph=f_rho_iph; if(last_iph<f_var_iph) last_iph=f_var_iph; if(last_iph<f_dev_iph) last_iph=f_dev_iph;
  if(last_iph<r_rho_iph) last_iph=r_rho_iph; if(last_iph<r_var_iph) last_iph=r_var_iph; if(last_iph<r_dev_iph) last_iph=r_dev_iph;
  if(last_iph<q_rho_iph) last_iph=q_rho_iph; if(last_iph<q_var_iph) last_iph=q_var_iph; if(last_iph<q_dev_iph) last_iph=q_dev_iph;
  last_iph+=1;
  if((estimate_r_dev_proj<=0.000001 && estimate_r_dev_proj>=-0.000001) || nyears_proj<=0) r_dev_proj_iph=-1; else { r_dev_proj_iph=last_iph; r_dev_proj_cv=estimate_r_dev_proj; }
  cout << r_dev_proj_cv << " " << estimate_r_dev_proj << endl;
  if(nyears_b4_change<=nyears_prehistoric) f_dev_iph=-1;
 END_CALCS

 // --------- derived variables pertaining to the data that are constant (don't need to be differentiated)----------//

  vector index_avg(1,n_index_series+1)
  vector index_min(1,n_index_series+1)
  vector n_index_points(1,n_index_series+1)
  vector one_vector_age(1,nages)
  number aic
  number temp_dble
  number n_data
 LOCAL_CALCS
  cout << "Averaging and scaling index data" << endl;
  n_index_points=0.0; index_avg=0.0; index_min=1000.0;
  for (series=1; series<=n_index_series;series++) {
    for (y=1; y<=nyears_past;y++) {
      if(index_obs(y,series)>=0) {
        if(index_obs(y,series)>0.0 && index_obs(y,series)<index_min(series)) index_min(series)=index_obs(y,series);
        n_index_points(series) += 1.0 ;
      }
    }
    for (y=1; y<=nyears_past;y++) {
      if(index_pdf(series)==1 && index_obs(y,series)>=0 && index_obs(y,series)<index_min(series)) index_obs(y,series)=index_min(series)/1000.0; // no zero indices for lognormal
      if(index_obs(y,series)>=0) index_avg(series) += index_obs(y,series)/n_index_points(series); 
    }
    for (y=1; y<=nyears_past;y++) if(index_units(series)<9 && index_scale(series)>0)  index_obs(y,series) /=  index_avg(series);
  } 
  n_data=sum(n_index_points); n_series=n_index_series;
  zero=0.0; one=1.0; n_calls=0; i_zero=0; i_one=1; i_two=2; one_vector_age=one;
 END_CALCS

 /////////////////////////////////////////////////////////////////////////////////////
PARAMETER_SECTION
 // Warning: all variables in this section must be floating point, not integers
 //          integers may be declared locally by use of !! int i   etc..., but these will
 //          not apply outside the parameter section (whereas the ADMB types number, vector
 //          and matrix are global)
 /////////////////////////////////////////////////////////////////////////////////////

 // --------- specify estimated parameters --------------------------------------------------//

 // get parameter bounds
 LOCAL_CALCS
   cout << "specifying parameter bounds " << endl;
   dvector lb(1,n_par); lb=column(par_specs,3); dvector ub(1,n_par); ub=column(par_specs,4);
   double lb_f_rho; lb_f_rho=f_rho_specs(2); double ub_f_rho; ub_f_rho=f_rho_specs(3);
   double lb_f_var; lb_f_var=f_var_specs(2); double ub_f_var; ub_f_var=f_var_specs(3);
   double lb_f;     lb_f=f_dev_specs(2);     double ub_f;     ub_f=f_dev_specs(3);
   double lb_r_rho; lb_r_rho=r_rho_specs(2); double ub_r_rho; ub_r_rho=r_rho_specs(3);
   double lb_r_var; lb_r_var=r_var_specs(2); double ub_r_var; ub_r_var=r_var_specs(3);
   double lb_r;     lb_r=r_dev_specs(2);     double ub_r;     ub_r=r_dev_specs(3);
   double lb_q_rho; lb_q_rho=q_rho_specs(2); double ub_q_rho; ub_q_rho=q_rho_specs(3);
   double lb_q_var; lb_q_var=q_var_specs(2); double ub_q_var; ub_q_var=q_var_specs(3);
   double lb_q;     lb_q=q_dev_specs(2);     double ub_q;     ub_q=q_dev_specs(3);
   double lb_0;     lb_0=0.0001;             double ub_2;     ub_2=2.0;
 END_CALCS

 // set parameter vector to be estimated
 !! cout << "specifying parameters " << endl;
  init_bounded_number_vector par_est(1,n_par,lb,ub,iph)
  init_bounded_number f_rho(lb_f_rho,ub_f_rho,f_rho_iph)
  init_bounded_number f_var(lb_f_var,ub_f_var,f_var_iph)
  init_bounded_vector f_devs(nyears_prehistoric+1,nyears_b4_change,lb_f,ub_f,f_dev_iph)
  init_bounded_number r_rho(lb_r_rho,ub_r_rho,r_rho_iph)
  init_bounded_number r_var(lb_r_var,ub_r_var,r_var_iph)
  init_bounded_vector r_devs(2,n_eras,lb_r,ub_r,r_dev_iph)
  init_bounded_number q_rho(lb_q_rho,ub_q_rho,q_rho_iph)
  init_bounded_number q_var(lb_q_var,ub_q_var,q_var_iph)
  init_bounded_matrix q_devs(1,nqs,2,n_eras,lb_q,ub_q,q_dev_iph)
  init_bounded_number Fspr20(lb_0,ub_2,last_iph)
  init_bounded_number Fspr30(lb_0,ub_2,last_iph)
  init_bounded_number Fspr40(lb_0,ub_2,last_iph)
  init_bounded_number Fspr50(lb_0,ub_2,last_iph)
  init_bounded_number Fspr60(lb_0,ub_2,last_iph)
  init_bounded_vector r_devs_proj(1,nyears_proj,lb_r,ub_r,r_dev_proj_iph)

 // --------- derived variables that are functions of the parameters and therefore need derivatives ----------//

 !! cout << "declaring state variables " << endl;
  vector f_apical(1,nyears_past)
  vector r(1,nyears)
  matrix q(1,nqs,1,n_eras)

 !! cout << "state (process) expectations (deterministic part)" << endl;
  vector f_process(1,nyears_past)
  vector r_process(1,nyears_past)
  matrix q_process(1,nqs,1,n_eras)
  vector m(1,nages)
  vector w(1,nages)
  vector fecundity(1,nages)
  matrix s(1,nss,1,nages)

 !! cout << "declare observation error parameters" << endl;
  vector i_d_var(1,nids)
  number overall_var

 !! cout << "declare likelihoods and priors" << endl;
  vector index_lklhd(1,n_index_series+1)
  number f_lklhd
  number r_lklhd
  vector q_lklhd(1,nqs)
  number f_prior
  number f_hist_prior
  number m_prior
  number r_prior
  number w_prior
  number v_prior
  vector q_prior(1,nqs)
  vector s_prior(1,nss)
  vector i_d_prior(1,nids)
  number q_process_prior
  number r_process_prior
  number penalty
  number equilibrium_penalty
  number projection_penalty

 !! cout << "declare misc. temporary variables" << endl;
  number pred
  number slope0
  number sprtemp
  number yprtemp
  number yprold
  number ytemp
  number yold
  number var
  number spr0
  number survive
  number plus_age
  number spr20
  number spr30
  number spr40
  number spr50
  number spr60
  number spr01
  number sprmax
  number sprmsy
  number sprmat
  number ypr20
  number ypr30
  number ypr40
  number ypr50
  number ypr60
  number ypr01
  number yprmax
  number yprmsy
  number yprmat
  number Rspr20
  number Rspr30
  number Rspr40
  number Rspr50
  number Rspr60
  number R01
  number Rmax
  number Rmsy
  number Rmat
  number Bmsy
  number Bmat
  number Bmax
  number B01
  number Bspr20
  number Bspr30
  number Bspr40
  number Bspr50
  number Bspr60
  vector function_parameter(1,10)
  vector recruitment_parameter(1,10)
  vector f_hist_parameter(1,10)
  vector growth_parameter(1,10)
  vector s_latest(1,nages)
  vector s_equilibrium(1,nages)
  vector ssb(1,nyears)
  vector virgin_pred(1,n_index_series)
  matrix index_pred(1,nyears_past,1,n_index_series)
  matrix wbyage(1,nages,1,nyears)
  matrix f(1,nages,1,nyears)
  matrix n(1,nages+1,1,nyears+1)
  vector F_proj(1,nyears_proj)
  objective_function_value obj_func;

 !! cout << "declare standard deviation report variables" << endl;
  likeprof_number alpha
  likeprof_number nat_mort
  sdreport_number Fmsy
  sdreport_number Fmat
  sdreport_number Fmax
  sdreport_number F01
  sdreport_number Bcurrent
  sdreport_number Fcurrent
  sdreport_number BoverBspr20
  sdreport_number BoverBspr30
  sdreport_number BoverBspr40
  sdreport_number BoverBspr50
  sdreport_number BoverBspr60
  sdreport_number BoverBmsy
  sdreport_number BoverBmat
  sdreport_number BoverBmax
  sdreport_number BoverB01
  sdreport_number FoverFspr20
  sdreport_number FoverFspr30
  sdreport_number FoverFspr40
  sdreport_number FoverFspr50
  sdreport_number FoverFspr60
  sdreport_number FoverFmsy
  sdreport_number FoverFmat
  sdreport_number FoverFmax
  sdreport_number FoverF01
  sdreport_vector B(1,nyears)
  sdreport_vector BoverBref(1,nyears)
  sdreport_vector log_F_apex(1,nyears)
  likeprof_number  Bpro_5
  likeprof_number  Bpro_4
  likeprof_number  Bpro_3
  likeprof_number  Bpro_2
  likeprof_number  Bpro_1
  likeprof_number  Bpro0
  likeprof_number  Bpro1
  likeprof_number  Bpro2
  likeprof_number  Bpro3
  likeprof_number  Bpro4
  likeprof_number  Bpro5
  likeprof_number  Bpro6
  likeprof_number  Bpro7
  likeprof_number  Bpro8
  likeprof_number  Bpro9
  likeprof_number  Bpro10
  likeprof_number  Bpro11
  likeprof_number  Bpro12
  likeprof_number  Bpro13
  likeprof_number  Bpro14
  likeprof_number  Bpro15

 !! cout << "Initialize parameters" << endl;
 /////////////////////////////////////////////////////////////////////////////////////
INITIALIZATION_SECTION              
 /////////////////////////////////////////////////////////////////////////////////////
   par_est best_guess
   f_rho   f_rho_best_guess
   f_var   f_var_best_guess
   f_devs  f_dev_best_guess
   r_rho   r_rho_best_guess
   r_var   r_var_best_guess
   r_devs  r_dev_best_guess
   q_rho   q_rho_best_guess
   q_var   q_var_best_guess
   q_devs  q_dev_best_guess
   Fspr20  F_best_guess
   Fspr30  F_best_guess
   Fspr40  F_best_guess
   Fspr50  F_best_guess
   Fspr60  F_best_guess
   r_devs_proj  r_dev_best_guess

 /////////////////////////////////////////////////////////////////////////////////////
PROCEDURE_SECTION
 /////////////////////////////////////////////////////////////////////////////////////
  define_parameters();
  calculate_biomass();
  calculate_the_objective_function();
  if(mceval_phase()) outputMCMC();

 /////////////////////////////////////////////////////////////////////////////////////
 // FUNCTION SECTION
 /////////////////////////////////////////////////////////////////////////////////////

 //-----------------------------------------------------------------------------------
FUNCTION define_parameters
 // defines process parameters and computes priors
 //-----------------------------------------------------------------------------------
   int j, y, inow, i_in, ihist;

   if(n_calls(1)==1) cout << "Define parameters" << endl;
   current_ph=current_phase(); n_calls(current_ph) += 1;
   i=1;                      // counters for keeping track of fixed (i) and estimated (ie) parameters, respectively

  //-------------compute expectations of state variables----------------//

  // apical fishing mortality rate during prehistoric period
    inow=i; f_hist_prior=0.; ihist=i;
    for ( j=1; j<=npf(nature(inow)); j++) {
      function_parameter(j)=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
      if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) f_hist_prior+=neg_log_lklhd(f_hist_parameter(j),best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);
    }
    for ( y=1; y<=nyears_prehistoric; y++) f_process(y)=function_value(nature(ihist),function_parameter,effort_obs(y));

  // apical fishing mortality rate during first modern period
    inow=i; f_prior=0.;
    for ( j=1; j<=npf(abs(nature(inow))); j++) {
      function_parameter(j)=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
      if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) f_prior+=neg_log_lklhd(function_parameter(j),best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);
    }
    if(nature(inow)<=-1) {
        // compute average F over last value[nature(inow)] years prior to modern period
        pred=0;
        for ( j=nyears_prehistoric+nature(inow)+1; j<=nyears_prehistoric; j++) pred+=f_process(j);      
        pred=pred/double(-nature(inow));  
    }
    for ( y=nyears_prehistoric+1; y<=nyears_b4_change; y++) {
      if(nature(inow)<=-1) f_process(y)=pred*function_parameter(1); // fishing mortality is proportional to average mortality rate in last years of historic time period
      else                 f_process(y)=function_value(nature(inow),function_parameter,effort_obs(y)); // fishing mortality is a function of input effort
    }
     // add process errors to apical fishing mortality rates
    f_apical=f_process; f_lklhd=0.;
    if(active(f_devs)) {
      for (y=nyears_prehistoric+1; y<=nyears_b4_change;y++) {
        if(f_dev_pdf==1) f_apical(y)=f_process(y)*mfexp(f_devs(y)); else f_apical(y)=f_process(y)+f_devs(y);
      }
    }

  // expected apical fishing mortality rate after change during modern period (e.g., moratorium)
    inow=i; 
    for ( j=1; j<=npf(abs(nature(inow))); j++) {
      function_parameter(j)=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
      if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) f_prior+=neg_log_lklhd(function_parameter(j),best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);
    }
    if(nature(inow)<=-1) {
        // compute average F over last value[nature(inow)] years prior to modern period
        pred=0;
        for ( j=nyears_b4_change+nature(inow)+1; j<=nyears_b4_change; j++) pred+=f_apical(j);
        pred=pred/double(-nature(inow));  
    }
    for ( y=nyears_b4_change+1; y<=nyears_past; y++)  {
      if(nature(inow)<=-1) f_apical(y)=pred*function_parameter(1); // fishing mortality is proportional to mortality rate in last year of first modern time period
      else                 f_apical(y)=function_value(nature(inow),function_parameter,effort_obs(y)); // fishing mortality is a function of input effort
    }

  // expected natural mortality rate by age
    inow=i; m_prior=0.;
    for ( j=1; j<=npf(nature(inow)); j++) {
      function_parameter(j)=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
      if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) m_prior+=neg_log_lklhd(function_parameter(j),best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);
    }
    for ( a=1; a<=nages; a++) m(a)=function_value(nature(inow),function_parameter,double(age(1)+a)-1);

  // expected relative recruitment
    inow=i; r_prior=0.; irn=i;
    for ( j=1; j<=npf(nature(inow)); j++) {
      recruitment_parameter(j)=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
      if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) r_prior+=neg_log_lklhd(recruitment_parameter(j),best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);
    }

  // expected growth
    inow=i; w_prior=0.; iwn=i;
    for ( j=1; j<=npf(nature(inow)); j++) {
      growth_parameter(j)=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
      if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) w_prior+=neg_log_lklhd(growth_parameter(j),best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);
    }
    for ( a=1; a<=nages-1; a++) {
      w(a)=function_value(nature(i-1),growth_parameter,double(age(1)+a)-1+0.5);
      if(fecundity_input(a)>=0) fecundity(a)=fecundity_input(a); else fecundity(a)=function_value(nature(i-1),growth_parameter,double(age(1)+a)-1+spawn_time);
    }
    if(m(nages)>0) plus_age=age(2)+mfexp(-m(nages))/(1-mfexp(-m(nages))); else plus_age=2*age(2);
    w(nages)=function_value(nature(iwn),growth_parameter,plus_age+0.5);
    if(fecundity_input(nages)>=0) fecundity(nages)=fecundity_input(nages); else fecundity(nages)=function_value(nature(i-1),growth_parameter,plus_age+spawn_time);

  // virgin spawner-per recruit
    spr0=spr(maturity,fecundity,m,one_vector_age,zero,spawn_time,nages);

  // expected q
    q_prior=0.;
    for (set=1; set<=nqs; set++) {
      inow=i;
      for ( j=1; j<=npf(nature(inow)); j++) {
        function_parameter(j)=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
        if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) q_prior(set)+=neg_log_lklhd(function_parameter(j),best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);
      }
      for ( y=1; y<=n_eras; y++) {
        q_process(set,y)=function_value(nature(i-1),function_parameter,one); 
      }
    }

  // expected selectivity/vulnerability
    s_prior=0.;
    for (set=1; set<=nss; set++) {
      inow=i;
      for ( j=1; j<=npf(nature(inow)); j++) {
        function_parameter(j)=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
        if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) s_prior(set)+=neg_log_lklhd(function_parameter(j),best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);
      }
      for ( a=1; a<=nages; a++) s(set,a)=function_value(nature(i-1),function_parameter,double(age(1)+a-1));
    }

  // index observation variance
    i_d_prior=0.;
    for (set=1; set<=nids; set++) {
      i_d_var(set)=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
      if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) i_d_prior(set)+=neg_log_lklhd(i_d_var(set),best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);
    }

  // overall variance
    overall_var=get_function_parameters(i,i_in,iph(i),current_ph,par_est(i),pdf(i));
    if(best_guess(i-1)<0) i_in = -i_in; // special case for negative cv's
    if(pdf(i-1)>0 && iph(i-1)>0 && iph(i-1)<=current_ph) v_prior=neg_log_lklhd(overall_var,best_guess(i-1),one,one,zero,cv(i-1),zero,pdf(i-1),variance_scale,i_zero,i_in);


  //-------------incorporate process errors----------------//

  // priors for apical fishing mortality rate process parameters
    if(active(f_rho)) f_prior+=neg_log_lklhd(f_rho,f_rho_best_guess,one,one,zero,f_rho_cv,zero,f_rho_pdf,variance_scale,i_zero,i_in);
    if(active(f_var)) f_prior+=neg_log_lklhd(f_var,f_var_best_guess,one,one,zero,f_var_cv,zero,f_var_pdf,variance_scale,i_zero,i_in);

  // priors for recruitment process parameters
    r_process_prior=0.;
    if(active(r_rho)) r_process_prior+=neg_log_lklhd(r_rho,r_rho_best_guess,one,one,zero,r_rho_cv,zero,r_rho_pdf,variance_scale,i_zero,i_in);
    if(active(r_var)) r_process_prior+=neg_log_lklhd(r_var,r_var_best_guess,one,one,zero,r_var_cv,zero,r_var_pdf,variance_scale,i_zero,i_in);

  // priors for q process parameters
    q_process_prior=0.;
    if(active(q_rho)) q_process_prior+=neg_log_lklhd(q_rho,q_rho_best_guess,one,one,zero,q_rho_cv,zero,q_rho_pdf,variance_scale,i_zero,i_in);
    if(active(q_var)) q_process_prior+=neg_log_lklhd(q_var,q_var_best_guess,one,one,zero,q_var_cv,zero,q_var_pdf,variance_scale,i_zero,i_in);

  // historical (1) and subsequent modern-era catchability coefficients
    q=q_process; q_lklhd=0.;
    if(active(q_devs)) {
      for (set=1; set<=nqs; set++) {
        for (y=2; y<=n_eras; y++) {
          if(q_dev_pdf==1) q(set,y)=q_process(set,y)*mfexp(q_devs(set,y)); else q(set,y)=q_process(set,y)+q_devs(set,y);
        }
      }
    }

 //-----------------------------------------------------------------------------------
FUNCTION calculate_biomass
 //-----------------------------------------------------------------------------------

  if(n_calls(1)==1) cout << "Calculate biomass" << endl;
  index_pred=zero ; ssb=zero; r_process=one; r_lklhd=zero;

  // calculate_fishing_mortality on all age classes (first two selectivity sets designated for historical and modern era fisheries)
  for (y=1; y<=nyears_past; y++) {
    if(y<=nyears_prehistoric) set=1; else set=2; 
    for (a=1; a<=nages; a++) f(a,y)=f_apical(y)*s(set,a);
  }

  // initial population structure assuming population at virgin levels (process errors assumed to average OUT)
  if(n_calls(1)==1) cout << "Calculating virgin abundance" << endl;
  n(1,1)=one;
  for (a=2; a<=nages; a++) {
    n(a,1)=n(a-1,1)*mfexp(-m(a-1));
    if(a==nages) n(a,1)=n(a,1)/(one-mfexp(-m(a)));
  }

  // time trajectory of population structure
  if(n_calls(1)==1) cout << "Calculating subsequent abundance" << endl;
  for (y=1; y<=nyears_past; y++)  {
    // distinguish historical period (no process errors) from modern epoch (has process errors)
    if(y<=nyears_prehistoric) t=1; 
    else t=y-nyears_prehistoric+1;

    // update recruitment
    if(y>age(1)) r_process(y)=function_value(nature(irn),recruitment_parameter,ssb(y-age(1))); // x-year-olds in year x+1 were produced in year 1 (for which one can compute the ssb),
    if(active(r_devs) && t>1) {
      if(r_dev_pdf==1) r(y)=r_process(y)*mfexp(r_devs(t)); else r(y)=r_process(y)+r_devs(t);
    }
    else r(y)=r_process(y);
    n(1,y)=r(y);

    virgin_pred=0.0;
    for (a=1; a<=nages; a++) {
      // predicted indices
      for (series=1; series<=n_index_series; series++) {
        if(index_pdf(series)>0) {
          if(index_units(series)==1)         index_pred(y,series) +=      q(iqs(series),t)*s(iss(series),a)*n(a,y)*mfexp(-(m(a)+f(a,y))*index_time(series));
          else if(index_units(series)==2)    index_pred(y,series) += w(a)*q(iqs(series),t)*s(iss(series),a)*n(a,y)*mfexp(-(m(a)+f(a,y))*index_time(series));
          else if(index_units(series)==10) { index_pred(y,series) +=      q(iqs(series),t)*s(iss(series),a)*n(a,y)*mfexp(-(m(a)+f(a,y))*index_time(series));
                                             virgin_pred(series)  +=                       s(iss(series),a)*n(a,1)*mfexp(-(m(a))*index_time(series)); }
          else if(index_units(series)==20) { index_pred(y,series) += w(a)*q(iqs(series),t)*s(iss(series),a)*n(a,y)*mfexp(-(m(a)+f(a,y))*index_time(series));
                                             virgin_pred(series)  += w(a)*                 s(iss(series),a)*n(a,1)*mfexp(-(m(a))*index_time(series)); }
        }
      }
      // average fecundity of plus-group during spawning season
      if(a==nages) {
        w(a)=function_value(nature(iwn),growth_parameter,plus_age+0.5);
        if(fecundity_input(a)>=0) fecundity(a)=fecundity_input(a); else fecundity(a)=function_value(nature(iwn),growth_parameter,plus_age+spawn_time);
      }
      wbyage(a,y)=w(a);

      // relative spawning biomass
      ssb(y)+=maturity(a)*fecundity(a)*n(a,y)*mfexp(-(m(a)+f(a,y))*spawn_time)/spr0;

      // abundance at beginning of next year
      n(a+1,y+1)=n(a,y)*mfexp(-m(a)-f(a,y)); // t=1 in historical period, t=year in modern period
    } //age

    // plus group age and abundance
    plus_age=(age(2)*n(nages,y+1)+(plus_age+1)*n(nages+1,y+1))/(n(nages,y+1)+n(nages+1,y+1));
    n(nages,y+1) += n(nages+1,y+1);

    // scale indices
    for (series=1; series<=n_index_series; series++) 
      if(index_pdf(series)>0 && index_units(series)>9) index_pred(y,series) /= virgin_pred(series);
    
  } //year

  // Projections and equilibrium statistics based on overall selectivity during last year
  if (sd_phase) {
    if(n_calls(1)==1) cout << "starting projections" << endl;
    for (a=1; a<=nages; a++) s_latest(a)=f(a,nyears_past); 
    Fcurrent=max(s_latest); Bcurrent=ssb(nyears_past); if(Fcurrent>0) s_latest=s_latest/Fcurrent;
    for (y=1; y<=nyears_past; y++) { if(f_apical(y)>0) log_F_apex(y)=log(f_apical(y)); else log_F_apex(y)=-999; } 
    alpha=recruitment_parameter(1)+1; nat_mort=m(1); Trecover=-1;
    if(reference_selectivity==1) s_equilibrium=s_latest;
    else s_equilibrium=maturity;

    if (last_phase()) {

     // Compute equilibrium statistics
      if(n_calls(1)==1) cout << "Calculating equilibrium statistics" << endl;
      F01=goldensection(3, Fspr30, w, m, s_equilibrium, nages, maturity, fecundity, spawn_time, spr0, nature(irn),recruitment_parameter );
      Fmax=goldensection(i_one, Fspr20, w, m, s_equilibrium, nages, maturity, fecundity, spawn_time, spr0, nature(irn),recruitment_parameter );
      Fmsy=goldensection(i_two, Fspr40, w, m, s_equilibrium, nages, maturity, fecundity, spawn_time, spr0, nature(irn),recruitment_parameter );
      Fmat =goldensection(i_two, Fspr40, w, m, maturity, nages, maturity, fecundity, spawn_time, spr0, nature(irn),recruitment_parameter );
      sprmat=spr(maturity,fecundity,m,maturity,Fmat,spawn_time,nages)/spr0;
      spr01=spr(maturity,fecundity,m,s_equilibrium,F01,spawn_time,nages)/spr0;
      sprmax=spr(maturity,fecundity,m,s_equilibrium,Fmax,spawn_time,nages)/spr0;
      sprmsy=spr(maturity,fecundity,m,s_equilibrium,Fmsy,spawn_time,nages)/spr0;
      spr20=spr(maturity,fecundity,m,s_equilibrium,Fspr20,spawn_time,nages)/spr0;
      spr30=spr(maturity,fecundity,m,s_equilibrium,Fspr30,spawn_time,nages)/spr0;
      spr40=spr(maturity,fecundity,m,s_equilibrium,Fspr40,spawn_time,nages)/spr0;
      spr50=spr(maturity,fecundity,m,s_equilibrium,Fspr50,spawn_time,nages)/spr0;
      spr60=spr(maturity,fecundity,m,s_equilibrium,Fspr60,spawn_time,nages)/spr0;
      yprmat=ypr(w,m,maturity,Fmat,nages);
      ypr01=ypr(w,m,s_equilibrium,F01,nages);
      yprmax=ypr(w,m,s_equilibrium,Fmax,nages);
      yprmsy=ypr(w,m,s_equilibrium,Fmsy,nages);
      ypr20=ypr(w,m,s_equilibrium,Fspr20,nages);
      ypr30=ypr(w,m,s_equilibrium,Fspr30,nages);
      ypr40=ypr(w,m,s_equilibrium,Fspr40,nages);
      ypr50=ypr(w,m,s_equilibrium,Fspr50,nages);
      ypr60=ypr(w,m,s_equilibrium,Fspr60,nages);
      Bmat  =equilibrium_ssb(nature(irn),recruitment_parameter,sprmat);    Rmat=Bmat/sprmat;
      Bspr20=equilibrium_ssb(nature(irn),recruitment_parameter,spr20);    Rspr20=Bspr20/spr20;
      Bspr30=equilibrium_ssb(nature(irn),recruitment_parameter,spr30);    Rspr30=Bspr30/spr30;
      Bspr40=equilibrium_ssb(nature(irn),recruitment_parameter,spr40);    Rspr40=Bspr40/spr40;
      Bspr50=equilibrium_ssb(nature(irn),recruitment_parameter,spr50);    Rspr50=Bspr50/spr50;
      Bspr60=equilibrium_ssb(nature(irn),recruitment_parameter,spr60);    Rspr60=Bspr60/spr60;
      B01   =equilibrium_ssb(nature(irn),recruitment_parameter,spr01);    R01   =B01   /spr01;
      Bmax  =equilibrium_ssb(nature(irn),recruitment_parameter,sprmax);   Rmax  =Bmax  /sprmax;
      Bmsy  =equilibrium_ssb(nature(irn),recruitment_parameter,sprmsy);   Rmsy  =Bmsy  /sprmsy;
      if(Bspr20 >0) BoverBspr20 =Bcurrent/Bspr20 ; else BoverBspr20 =-9.0;
      if(Bspr30 >0) BoverBspr30 =Bcurrent/Bspr30 ; else BoverBspr30 =-9.0;
      if(Bspr40 >0) BoverBspr40 =Bcurrent/Bspr40 ; else BoverBspr40 =-9.0;
      if(Bspr50 >0) BoverBspr50 =Bcurrent/Bspr50 ; else BoverBspr50 =-9.0;
      if(Bspr60 >0) BoverBspr60 =Bcurrent/Bspr60 ; else BoverBspr60 =-9.0;
      if(B01    >0) BoverB01    =Bcurrent/B01    ; else BoverB01    =-9.0;
      if(Bmax   >0) BoverBmax   =Bcurrent/Bmax   ; else BoverBmax   =-9.0;
      if(Bmsy   >0) BoverBmsy   =Bcurrent/Bmsy   ; else BoverBmsy   =-9.0;
      if(Bmat    >0) BoverBmat    =Bcurrent/Bmat    ; else BoverBmat    =-9.0;
      if(Fspr20 >0) FoverFspr20 =Fcurrent/Fspr20 ; else FoverFspr20 =-9.0;
      if(Fspr30 >0) FoverFspr30 =Fcurrent/Fspr30 ; else FoverFspr30 =-9.0;
      if(Fspr40 >0) FoverFspr40 =Fcurrent/Fspr40 ; else FoverFspr40 =-9.0;
      if(Fspr50 >0) FoverFspr50 =Fcurrent/Fspr50 ; else FoverFspr50 =-9.0;
      if(Fspr60 >0) FoverFspr60 =Fcurrent/Fspr60 ; else FoverFspr60 =-9.0;
      if(F01    >0) FoverF01    =Fcurrent/F01    ; else FoverF01    =-9.0;
      if(Fmax   >0) FoverFmax   =Fcurrent/Fmax   ; else FoverFmax   =-9.0;
      if(Fmsy   >0) FoverFmsy   =Fcurrent/Fmsy   ; else FoverFmsy   =-9.0;
      if(Fmat    >0) FoverFmat    =Fcurrent/Fmat    ; else FoverFmat    =-9.0;

     // Compute projections
      if(n_calls(1)==1 && nyears_proj>0) cout << "Making projections" << endl;
      for (y=nyears_past+1; y<=nyears; y++)  {
        t=y-nyears_past;
        r(y)=function_value(nature(irn),recruitment_parameter,ssb(y-age(1))); // x-year-olds in year x+1 were produced in year 1 (for which one can compute the ssb),
        if(active(r_devs_proj)) { if(r_dev_pdf==1) r(y)=r(y)*mfexp(r_devs_proj(t)); else r(y)=r(y)+r_devs_proj(t); }
        n(1,y)=r(y);
        for (a=1; a<=nages; a++) {
          // average fecundity of plus-group during spawning season
          if(a==nages) {
            w(a)=function_value(nature(iwn),growth_parameter,plus_age+0.5);
            if(fecundity_input(a)>=0) fecundity(a)=fecundity_input(a); else fecundity(a)=function_value(nature(iwn),growth_parameter,plus_age+spawn_time);
          }
          wbyage(a,y)=w(a);
          if(in_prj(t,1) >= 0)        F_proj(t)=in_prj(t,1);   // note: this approach assumes there is no implementation uncertainty
          else if(in_prj(t,1) > -0.2) F_proj(t)=F01;           //       I had a hard time getting runs with long projections to converge
          else if(in_prj(t,1) > -1)   F_proj(t)=Fmat;           //       when I treated F_proj as a random variable, even with low implementation uncertainty
          else if(in_prj(t,1) > -2)   F_proj(t)=Fmsy;
          else if(in_prj(t,1) > -3)   F_proj(t)=Fmax;
          else if(in_prj(t,1) > -21)  F_proj(t)=Fspr20;
          else if(in_prj(t,1) > -31)  F_proj(t)=Fspr30;
          else if(in_prj(t,1) > -41)  F_proj(t)=Fspr40;
          else if(in_prj(t,1) > -51)  F_proj(t)=Fspr50;
          else if(in_prj(t,1) > -61)  F_proj(t)=Fspr60;
          else                        F_proj(t)=Fcurrent;
          if(F_proj(t)>0) log_F_apex(y)=log(F_proj(t)); else log_F_apex(y)=-999;
          f(a,y)=F_proj(t)*s_latest(a);
          ssb(y)+=maturity(a)*fecundity(a)*n(a,y)*mfexp(-(m(a)+f(a,y))*spawn_time)/spr0;
          n(a+1,y+1)=n(a,y)*mfexp(-m(a)-f(a,y));
        } //age
        plus_age=(age(2)*n(nages,y+1)+(plus_age+1)*n(nages+1,y+1))/(n(nages,y+1)+n(nages+1,y+1));
        n(nages,y+1) += n(nages+1,y+1);
      } //year
      B=ssb; BoverBref=-9.0;
      if(Bref > 0)                        BoverBref = B/Bref ;
      else if(Bref > -0.2 && B01    > 0)  BoverBref = B/B01    ;
      else if(Bref > -1   && Bmat   > 0)  BoverBref = B/Bmat   ;
      else if(Bref > -2   && Bmsy   > 0)  BoverBref = B/Bmsy   ;
      else if(Bref > -3   && Bmax   > 0)  BoverBref = B/Bmax   ;
      else if(Bref > -21  && Bspr20 > 0)  BoverBref = B/Bspr20 ;
      else if(Bref > -31  && Bspr30 > 0)  BoverBref = B/Bspr30 ;
      else if(Bref > -41  && Bspr40 > 0)  BoverBref = B/Bspr40 ;
      else if(Bref > -51  && Bspr50 > 0)  BoverBref = B/Bspr50 ;
      else if(Bref > -61  && Bspr60 > 0)  BoverBref = B/Bspr60 ;
      else                                BoverBref = B/Bcurrent ;
      if(Bspr30 >0) BoverBspr30 =Bcurrent/Bspr30 ; else BoverBspr30 =-9.0;
      if(Bspr40 >0) BoverBspr40 =Bcurrent/Bspr40 ; else BoverBspr40 =-9.0;
      if(Bspr50 >0) BoverBspr50 =Bcurrent/Bspr50 ; else BoverBspr50 =-9.0;
      if(Bspr60 >0) BoverBspr60 =Bcurrent/Bspr60 ; else BoverBspr60 =-9.0;
      if(B01    >0) BoverB01    =Bcurrent/B01    ; else BoverB01    =-9.0;
      if(Bmax   >0) BoverBmax   =Bcurrent/Bmax   ; else BoverBmax   =-9.0;
      if(Bmsy   >0) BoverBmsy   =Bcurrent/Bmsy   ; else BoverBmsy   =-9.0;
      for(y=nyears_past; y<=nyears; y++) if(BoverBref(y)>=1.0) {Trecover=y+year(1)-1; break;}
      Bpro_5=BoverBref(nyears_past-5);
      Bpro_4=BoverBref(nyears_past-4);
      Bpro_3=BoverBref(nyears_past-3);
      Bpro_2=BoverBref(nyears_past-2);
      Bpro_1=BoverBref(nyears_past-1);
      Bpro0=BoverBref(nyears_past);
      if(nyears_proj<1) Bpro1=-BoverBref(nyears_past); else Bpro1=BoverBref(nyears_past+1);
      if(nyears_proj<2) Bpro2=-BoverBref(nyears_past); else Bpro2=BoverBref(nyears_past+2);
      if(nyears_proj<3) Bpro3=-BoverBref(nyears_past); else Bpro3=BoverBref(nyears_past+3);
      if(nyears_proj<4) Bpro4=-BoverBref(nyears_past); else Bpro4=BoverBref(nyears_past+4);
      if(nyears_proj<5) Bpro5=-BoverBref(nyears_past); else Bpro5=BoverBref(nyears_past+5);
      if(nyears_proj<6) Bpro6=-BoverBref(nyears_past); else Bpro6=BoverBref(nyears_past+6);
      if(nyears_proj<7) Bpro7=-BoverBref(nyears_past); else Bpro7=BoverBref(nyears_past+7);
      if(nyears_proj<8) Bpro8=-BoverBref(nyears_past); else Bpro8=BoverBref(nyears_past+8);
      if(nyears_proj<9) Bpro9=-BoverBref(nyears_past); else Bpro9=BoverBref(nyears_past+9);
      if(nyears_proj<10)Bpro10=-BoverBref(nyears_past); else Bpro10=BoverBref(nyears_past+10);
      if(nyears_proj<11) Bpro11=-1; else Bpro11=BoverBref(nyears_past+11);
      if(nyears_proj<12) Bpro12=-1; else Bpro12=BoverBref(nyears_past+12);
      if(nyears_proj<13) Bpro13=-1; else Bpro13=BoverBref(nyears_past+13);
      if(nyears_proj<14) Bpro14=-1; else Bpro14=BoverBref(nyears_past+14);
      if(nyears_proj<15) Bpro15=-1; else Bpro15=BoverBref(nyears_past+15);

    }// last_phase loop
  }// sd_phase loop

 //-----------------------------------------------------------------------------------
FUNCTION calculate_the_objective_function
 //-----------------------------------------------------------------------------------
  double penalty_wt;
  if(n_calls(1)==1) cout << "Calculating objective function" << endl;
  index_lklhd=0.; obj_func=0.; penalty=0; equilibrium_penalty=0; projection_penalty=0; penalty_wt=0.001;

 // ---------------observation errors----------------\\
  for(y=1; y<=nyears_past; y++) {
    for(series=1; series<=n_index_series; series++) {
       //cout << "index " << y << " " << series << " " << index_obs(y,series) << " " << index_pred(y,series) << " " << index_cv(y,series) << endl; 
       if(index_pdf(series)>0  && index_obs(y,series)>=0)  index_lklhd(series)+=neg_log_lklhd(index_obs(y,series),index_pred(y,series),one,one,zero,i_d_var(ivs(series))*overall_var,index_cv(y,series),index_pdf(series),variance_scale,variance_modify,y);
    }
  }
  if(n_index_series>0) obj_func+=sum(index_lklhd);

 // ---------------process errors----------------\\
  if(active(r_devs)) {
    if(variance_scale==1 && r_dev_pdf==1 && r_var<zero) var=log(1.0+square(r_var));
    else if(variance_scale==1 && r_dev_pdf==1 && r_var>zero) var=r_var;
    else if(variance_scale==2 && r_dev_pdf==2 && r_var>zero) var=r_var;
    else var=get_variance(one,r_var,zero,r_dev_pdf,variance_scale,i_zero);
    r_lklhd=square(r_devs(2));
    for(t=3; t<=n_eras; t++) r_lklhd += square(r_devs(t)-r_rho*r_devs(t-1));
    r_lklhd=0.5*(r_lklhd/var+double(n_eras-1)*log(var));
    obj_func += r_lklhd;
  }
  if(active(f_devs)) {
    if(variance_scale==1 && f_dev_pdf==1 && f_var<zero) var=log(1.0+square(f_var));
    else if(variance_scale==1 && f_dev_pdf==1 && f_var>zero) var=f_var;
    else if(variance_scale==2 && f_dev_pdf==2 && f_var>zero) var=f_var;
    else var=get_variance(f_process(nyears_prehistoric+1),f_var,zero,f_dev_pdf,variance_scale,i_zero);
    f_lklhd=square(f_devs(nyears_prehistoric+1));
    for(t=nyears_prehistoric+2; t<=nyears_b4_change; t++) f_lklhd += square(f_devs(t)-f_rho*f_devs(t-1));
    f_lklhd=0.5*(f_lklhd/var+double(nyears_b4_change-nyears_prehistoric)*log(var));
    obj_func += f_lklhd;
  }

  if(active(q_devs)) {
    for (set=1; set<=nqs; set++) {
      if(variance_scale==1 && q_dev_pdf==1 && overall_var<zero) var=log(1.0+square(q_var*overall_var));
      else if(variance_scale==1 && q_dev_pdf==1 && overall_var>zero) var=q_var*overall_var;
      else if(variance_scale==2 && q_dev_pdf==2 && overall_var>zero) var=q_var*overall_var;
      else var=get_variance(q(nyears_prehistoric+1,set),q_var*overall_var,zero,q_dev_pdf,variance_scale,i_zero);
      q_lklhd(set)=square(q_devs(2,set));
      for(t=3; t<=n_eras; t++) q_lklhd(set) += square(q_devs(t,set)-q_rho*q_devs(t-1,set));
      q_lklhd(set)=0.5*(q_lklhd(set)/var+(n_eras-1)*log(var));
    }
    obj_func += sum(q_lklhd);
  }

 // ---------------Bayesian priors-------------------\\
  obj_func += m_prior+r_prior+f_prior+f_hist_prior+w_prior+v_prior+q_process_prior+r_process_prior+sum(q_prior)+sum(s_prior)+sum(i_d_prior);

 // --------------other penalties--------------------\\
  for (y=1; y<=nyears_past; y++) if(r(y)<0) penalty += square(r(y))*1000.0;
  for (y=1; y<=n_eras; y++) for (set=1; set<=nqs; set++) if(q(set,y)<0) penalty += square(q(set,y))*1000.0;
  for (a=1; a<=nages; a++) {
    if(m(a)<0) penalty += square(m(a))*1000.0;
    if(w(a)<0) penalty += square(w(a))*1000.0;
    for (set=1; set<=nss; set++) if(s(set,a)<0) penalty += square(s(set,a))*1000.0;
  }
  if(current_ph<(last_iph-1)) {
    pred= max(f_apical) ; 
    if(pred<0.1) penalty+=neg_log_lklhd(0.1,pred,one,one,zero,overall_var,zero,variance_scale,variance_scale,i_zero,y);
    if(pred>one) penalty+=neg_log_lklhd(one,pred,one,one,zero,overall_var,zero,variance_scale,variance_scale,i_zero,y);
  }
  else if(last_phase()) {
    //equilibrium_penalty+=neg_log_lklhd(0.2,spr20,one,one,zero,10*overall_var,zero,variance_scale,variance_scale,i_zero,y);
    equilibrium_penalty+=square(0.2-spr20)/penalty_wt;
    equilibrium_penalty+=square(0.3-spr30)/penalty_wt;
    equilibrium_penalty+=square(0.4-spr40)/penalty_wt;
    equilibrium_penalty+=square(0.5-spr50)/penalty_wt;
    equilibrium_penalty+=square(0.6-spr60)/penalty_wt;
    if(active(r_devs_proj)) {
      if(variance_scale==1 && r_dev_pdf==1 && r_dev_proj_cv<zero) var=log(1.0+square(r_dev_proj_cv));
      else if(variance_scale==1 && r_dev_pdf==1 && r_dev_proj_cv>zero) var=r_dev_proj_cv;
      else if(variance_scale==2 && r_dev_pdf==2 && r_dev_proj_cv>zero) var=r_dev_proj_cv;
      else var=get_variance(one,r_dev_proj_cv,zero,r_dev_pdf,variance_scale,i_zero);
      projection_penalty=square(r_devs_proj(1));
      for(t=2; t<=nyears_proj; t++) projection_penalty += square(r_devs_proj(t)-r_rho*r_devs_proj(t-1));
      projection_penalty=0.5*(projection_penalty/var+double(nyears_proj)*log(var));
    }
  }
  obj_func+=(penalty+equilibrium_penalty+projection_penalty);

 //-----------------------------------------------------------------------------------
FUNCTION outputMCMC
 //-----------------------------------------------------------------------------------
  ofstream MCMCout("MCMC.out",ios::app);
  MCMCout << alpha << " " << nat_mort << " " << Bpro_5 << " " << Bpro_4 << " "<< Bpro_3 << " "<< Bpro_2 << " " << Bpro_1 << " " ;
  MCMCout << Bpro0 << " " << Bpro1 << " "<< Bpro2 << " "<< Bpro3 << " " << Bpro4 << " " << Bpro5 << " ";
  MCMCout << Bpro6 << " "<< Bpro7 << " "<< Bpro8 << " " << Bpro9 << " " << Bpro10 << " ";
  MCMCout << Bpro11 << " "<< Bpro12 << " "<< Bpro13 << " " << Bpro14 << " " << Bpro15 << endl;
  MCMCout.close();


 /////////////////////////////////////////////////////////////////////////////////////
REPORT_SECTION   // uses regular C++ code
 /////////////////////////////////////////////////////////////////////////////////////
  n_par_phase=initial_params::nvarcalc(); // number of active parameters
  double aic=2.0*(value(obj_func-equilibrium_penalty-projection_penalty)+double(n_par_phase));
  cout << "Writing report" << endl;

  adstring label;
  if(Bref > 0)                        label = "input value   ";
  else if(Bref > -0.2 && B01    > 0)  label = "B at F0.1     ";
  else if(Bref > -1   && Bmat   > 0)  label = "B at MSYadult ";
  else if(Bref > -2   && Bmsy   > 0)  label = "B at MSYfleet ";
  else if(Bref > -3   && Bmax   > 0)  label = "B at Fmax     ";
  else if(Bref > -21  && Bspr20 > 0)  label = "B at 20% spr  ";
  else if(Bref > -31  && Bspr30 > 0)  label = "B at 30% spr  ";
  else if(Bref > -41  && Bspr40 > 0)  label = "B at 40% spr  ";
  else if(Bref > -51  && Bspr50 > 0)  label = "B at 50% spr  ";
  else if(Bref > -61  && Bspr60 > 0)  label = "B at 60% spr  ";
  else                                label = "current level ";

  report.setf(ios::right, ios::adjustfield);
  report.setf(ios::scientific, ios::floatfield);
  report << "--------------------------------------------------------------------" << endl;
  report << "LIKELIHOOD RESULTS" << endl;
  report << "--------------------------------------------------------------------" << endl;
  report << "AIC                 : " << setw(12) << setprecision(5) << aic << endl;

  if(n_data<(n_par_phase+2)) {
    report << "AICc (small sample) : " << " undefined (too few data)" << endl;
  }
  else {
    double aicc=aic+2.0*double(n_par_phase*(n_par_phase+1)/(n_data-n_par_phase-1));
    report << "AICc (small sample) : " << setw(12) << setprecision(5) << aicc << endl;
  }
  report << "               " << endl; 
  report << "OBJECTIVE FUNCTION  : " << setw(12) << setprecision(5) << obj_func << endl;
  report << " Observation errors : " << endl;
  report << "   Abundance indices: " ;
    for(series=1; series<=n_index_series-1; series++) report << setw(12) << setprecision(5) << index_lklhd(series) << " ";  
    report << setw(12) << setprecision(5) << index_lklhd(n_index_series) << endl ;
  report << " Process errors     : " << endl;
  report << "   f fishing mort.  : " << setw(12) << setprecision(5) << f_lklhd  << endl;
  report << "   r recruitment    : " << setw(12) << setprecision(5) << r_lklhd  << endl;
  report << "   q catchability   : " ;
    for(set=1; set<=nqs-1; set++) report << setw(12) << setprecision(5) << q_lklhd(set) << " ";  
    report << setw(12) << setprecision(5) << q_lklhd(nqs) << endl ;
  report << " Priors             : " << endl;
  report << "   F historical     : " << setw(12) << setprecision(5) << f_hist_prior  << endl;
  report << "   F modern period  : " << setw(12) << setprecision(5) << f_prior  << endl;
  report << "   m natural mort.  : " << setw(12) << setprecision(5) << m_prior  << endl;
  report << "   r recruitment    : " << setw(12) << setprecision(5) << r_prior  << endl;
  report << "   r process error  : " << setw(12) << setprecision(5) << r_process_prior  << endl;
  report << "   k growth         : " << setw(12) << setprecision(5) << w_prior  << endl;
  report << "   q catchability   : " ;
    for(set=1; set<=nqs-1; set++) report << setw(12) << setprecision(5) << q_prior(set) << " ";  
    report << setw(12) << setprecision(5) << q_prior(nqs) << endl ;
  report << "   q process error  : " << setw(12) << setprecision(5) << q_process_prior << endl;
  report << "   s selectivity    : " ;
    for(set=1; set<=nss-1; set++) report << setw(12) << setprecision(5) << s_prior(set) << " ";  
    report << setw(12) << setprecision(5) << s_prior(nss) << endl ;
  report << "   index variances  : ";
    for(set=1; set<=nids-1; set++) report << setw(12) << setprecision(5) << i_d_prior(set) << " ";  
    report << setw(12) << setprecision(5) << i_d_prior(nids) << endl ;
  report << "   over-all var.    : " << setw(12) << setprecision(5) << v_prior << endl;
  report << "  Penalties         : " << endl;
  report << "   out-of-bounds    : " << setw(12) << setprecision(5) << penalty << endl;
  report << "   equilibrium stats: " << setw(12) << setprecision(5) << equilibrium_penalty << endl;
  report << "   projections      : " << setw(12) << setprecision(5) << projection_penalty << endl;
  report << "               " << endl; 
  if(overall_var<zero) report << "OVERALL %CV         : " << setw(12) << setprecision(5) << -100.0*overall_var << endl;
  else                 report << "OVERALL VARIANCE    : " << setw(12) << setprecision(5) << overall_var << endl;
  report << "               " << endl; report << "               " << endl;
  report << "LIFE-TIME REPRODUCTIVE RATE: " << setw(12) << setprecision(5) << alpha << endl;
  report << "NATURAL MORTALITY RATE: " << setw(12) << setprecision(5) << nat_mort << endl;
  report << "YEAR OF RECOVERY: " << setw(5) << setprecision(0) << Trecover << endl;
  report << "               " << endl; report << "               " << endl;
  report << "NUMBER OF FUNCTION EVALUATIONS (THIS PHASE): " << setw(12) << setprecision(5) << n_calls(current_ph) << endl;
  report << "NUMBER OF FUNCTION EVALUATIONS (CUMULATIVE): " << setw(12) << setprecision(5) << sum(n_calls) << endl;
  report << "               " << endl; report << "               " << endl;

  report << "--------------------------------------------------------------------" << endl;
  report << "MANAGEMENT BENCHMARKS" << endl;
  report << "Type           F           Y/R        SSB        SPR        R" << endl;
  report << "--------------------------------------------------------------------" << endl;
  report.setf(ios::scientific, ios::floatfield);
  report << "VIRGIN   " << setw(13) << setprecision(4) << zero   << " " << zero   << " " << one     << " " << one    << " " << one    << endl;
  report << "MSY adult" << setw(13) << setprecision(4) << Fmat   << " " << yprmat << " " << Bmat    << " " << sprmat << " " << Rmat   << endl;
  report << "MSY fleet" << setw(13) << setprecision(4) << Fmsy   << " " << yprmsy << " " << Bmsy    << " " << sprmsy << " " << Rmsy   << endl;
  report << "MAX Y/R  " << setw(13) << setprecision(4) << Fmax   << " " << yprmax << " " << Bmax    << " " << sprmax << " " << Rmax   << endl;
  report << "F0.1     " << setw(13) << setprecision(4) << F01    << " " << ypr01  << " " << B01     << " " << spr01  << " " << R01    << endl;
  report << "20% SPR  " << setw(13) << setprecision(4) << Fspr20 << " " << ypr20  << " " << Bspr20  << " " << spr20  << " " << Rspr20 << endl;
  report << "30% SPR  " << setw(13) << setprecision(4) << Fspr30 << " " << ypr30  << " " << Bspr30  << " " << spr30  << " " << Rspr30 << endl;
  report << "40% SPR  " << setw(13) << setprecision(4) << Fspr40 << " " << ypr40  << " " << Bspr40  << " " << spr40  << " " << Rspr40 << endl;
  report << "50% SPR  " << setw(13) << setprecision(4) << Fspr50 << " " << ypr50  << " " << Bspr50  << " " << spr50  << " " << Rspr50 << endl;
  report << "60% SPR  " << setw(13) << setprecision(4) << Fspr60 << " " << ypr60  << " " << Bspr60  << " " << spr60  << " " << Rspr60 << endl;
  report << "               " << endl; report << "               " << endl;

  report << "--------------------------------------------------------------------" << endl;
  report << "PRESENT CONDITION OF STOCK" << endl;
  report << "Type           F          SSB" << endl;
  report << "--------------------------------------------------------------------" << endl;
  report.setf(ios::scientific, ios::floatfield);
  report << "CURRENT    " << setw(13) << setprecision(4) << Fcurrent        << " " << Bcurrent      << endl;
  report << " /MSY adult" << setw(13) << setprecision(4) << FoverFmat       << " " << BoverBmat     << endl;
  report << " /MSY fleet" << setw(13) << setprecision(4) << FoverFmsy       << " " << BoverBmsy     << endl;
  report << " /MAX Y/R  " << setw(13) << setprecision(4) << FoverFmax       << " " << BoverBmax     << endl;
  report << " /F0.1     " << setw(13) << setprecision(4) << FoverF01        << " " << BoverB01      << endl;
  report << " /20% SPR  " << setw(13) << setprecision(4) << FoverFspr20     << " " << BoverBspr20   << endl;
  report << " /30% SPR  " << setw(13) << setprecision(4) << FoverFspr30     << " " << BoverBspr30   << endl;
  report << " /40% SPR  " << setw(13) << setprecision(4) << FoverFspr40     << " " << BoverBspr40   << endl;
  report << " /50% SPR  " << setw(13) << setprecision(4) << FoverFspr50     << " " << BoverBspr50   << endl;
  report << " /60% SPR  " << setw(13) << setprecision(4) << FoverFspr60     << " " << BoverBspr60   << endl;
  report << "               " << endl; report << "               " << endl;

  report << "--------------------------------------------------------------------" << endl;
  report << "RELATIVE ABUNDANCE ESTIMATES by age" << endl;
  report << "Year" << " ";
  report.setf(ios::fixed, ios::floatfield);
  for (a=1; a<=nages-1; a++) report << setw(8) << setprecision(0) << a+age(1)-1 << "     ";
  report << setw(8) << setprecision(0) << nages+age(1)-1 << endl;
  report << "--------------------------------------------------------------------" << endl;
  for (y=1; y<=nyears; y++) {
    report.setf(ios::fixed, ios::floatfield);
    report << setw(4) << setprecision(0) << y+year(1)-1 << "  ";
    report.setf(ios::scientific, ios::floatfield);
    for (a=1; a<=nages-1; a++) report << setw(12) << setprecision(4) << n(a,y) << " ";
    report << setw(12) << setprecision(4) << n(nages,y) << endl;
  }
  report << "               " << endl; report << "               " << endl;

  report << "--------------------------------------------------------------------" << endl;
  report << "FISHING MORTALITY RATE ESTIMATES by age" << endl;
  report << "Year" << " ";
  report.setf(ios::fixed, ios::floatfield);
  for (a=1; a<=nages-1; a++) report << setw(8) << setprecision(0) << a+age(1)-1 << "     ";
  report << setw(8) << setprecision(0) << nages+age(1)-1 << endl;
  report << "--------------------------------------------------------------------" << endl;
  for (y=1; y<=nyears; y++) {
    report.setf(ios::fixed, ios::floatfield);
    report << setw(4) << setprecision(0) << y+year(1)-1 << "  ";
    report.setf(ios::scientific, ios::floatfield);
    for (a=1; a<=nages-1; a++) report << setw(12) << setprecision(4) << f(a,y) << " ";
    report << setw(12) << setprecision(4) << f(nages,y) << endl;
  }
  report << "               " << endl; report << "               " << endl;


  report << "--------------------------------------------------------------------" << endl;
  report << "RELATIVE SPAWNING BIOMASS ESTIMATES" << endl;
  report << "Year" << "      " << "Spawning biomass (B) relative to" <<endl;
  report << "Year" << "       " << "virgin level" << "       " << label << endl;
  report.setf(ios::fixed, ios::floatfield);
  report << "--------------------------------------------------------------------" << endl;
  for (y=1; y<=nyears; y++) {
    report.setf(ios::fixed, ios::floatfield);
    report << setw(4) << setprecision(0) << y+year(1)-1 << "      ";
    report.setf(ios::scientific, ios::floatfield);
    report << setw(12) << setprecision(4) << ssb(y) << "      ";
    report << setw(12) << setprecision(4) << BoverBref(y) << endl;
  }
  report << "               " << endl; report << "               " << endl;


  report << "--------------------------------------------------------------------" << endl;
  report << "INDEX (CPUE) ESTIMATES" << endl;
  report << "Series" << "  Year" << "    Observed" << "    Predicted" << "   Variance" << "    Catchability" << endl;
  report << "--------------------------------------------------------------------" << endl;
  if(n_index_series<=0) report << "  None used" << endl;
  for(series=1; series<=n_index_series; series++) {
    report.setf(ios::fixed, ios::floatfield);
    if(index_pdf(series)==0)
      report << setw(4) << setprecision(0) << series << "    " << "Not used" << endl;
    else {
      for (y=1; y<=nyears_past; y++) {
        if(y<=nyears_prehistoric) t=1; else t=y-nyears_prehistoric+1;
        report.setf(ios::fixed, ios::floatfield);
        report << setw(4) << setprecision(0) << series << "    ";
        report << setw(4) << setprecision(0) << y+year(1)-1 << "  ";
        report.setf(ios::scientific, ios::floatfield);
        if(index_obs(y,series)>=0) report << setw(12) << setprecision(4) << index_obs(y,series); else report << setw(12) << setprecision(0) << -i_one;
        report << setw(12) << setprecision(4) << index_pred(y,series);
        if(index_obs(y,series)>=0) report  << "  " << get_variance(index_pred(y,series),i_d_var(ivs(series))*overall_var,index_cv(y,series),index_pdf(series),variance_scale,variance_modify) ; else report << "            ";
        report << setw(12) << setprecision(4) << q(iqs(series),t) << endl;
      }
    }
  }
  report << "               " << endl; report << "               " << endl;

  report << "--------------------------------------------------------------------" << endl;
  report << "WEIGHT ESTIMATES by age" << endl;
  report << "Year" << " ";
  report.setf(ios::fixed, ios::floatfield);
  for (a=1; a<=nages-1; a++) report << setw(8) << setprecision(0) << a+age(1)-1 << "     ";
  report << setw(8) << setprecision(0) << nages+age(1)-1 << endl;
  report << "--------------------------------------------------------------------" << endl;
  for (y=1; y<=nyears; y++) {
    report.setf(ios::fixed, ios::floatfield);
    report << setw(4) << setprecision(0) << y+year(1)-1 << "  ";
    report.setf(ios::scientific, ios::floatfield);
    for (a=1; a<=nages-1; a++) report << setw(12) << setprecision(4) << wbyage(a,y) << " "; 
    report << setw(12) << setprecision(4) << wbyage(nages,y) << " " << endl;
  }
  report << "               " << endl; report << "               " << endl;



 /////////////////////////////////////////////////////////////////////////////////////
RUNTIME_SECTION
 /////////////////////////////////////////////////////////////////////////////////////
  convergence_criteria 1.e-2, 1.e-3, 1.e-3, 1.e-4, 1.e-6
  maximum_function_evaluations  50, 100, 200, 400, 1000

 /////////////////////////////////////////////////////////////////////////////////////
TOP_OF_MAIN_SECTION
 /////////////////////////////////////////////////////////////////////////////////////
 // set buffer sizes
 arrmblsize=500000;
 gradient_structure::set_MAX_NVAR_OFFSET(500);
 gradient_structure::set_NUM_DEPENDENT_VARIABLES(50000);

 /////////////////////////////////////////////////////////////////////////////////////
GLOBALS_SECTION
 /////////////////////////////////////////////////////////////////////////////////////
 #include <admodel.h>

  double zero, one;
  dvector lower(1,1000);
  dvector upper(1,1000);
  int ifv,imv,imd,iwv,iwd,iwn,irv,ird,irn,i_zero,i_one,i_two,current_ph,series,set,y,a,t;

 //-----------------------------------------------------------------------------------
  dvariable neg_log_lklhd(dvariable obs,dvariable pred,dvariable obs_1,dvariable pred_1,
                          dvariable rho,dvariable var,dvariable modifier,int pdf,int scale, int modify, int count)
 //-----------------------------------------------------------------------------------
  {
    int oldcount;
    dvariable answer, alph, beta;

    // compute generic negative log-likelihood formulae
    if(obs<0.0 && count>=0)
      answer=0.0; // no data or process
    else {
      oldcount=count;
      if(count<0) count = -1*count;
      switch(pdf) {
        case 1: // autocorrelated lognormal
          //cout << obs << " " << pred << " " << obs_1 << " " << pred_1 << " " << var << endl;
          if(pred<=0 && oldcount>=0) pred=1.0E-10; // negative oldcount means this variable is supposed to be negative;
          if(var<0)                     var=log(1.0+square(var)) ;      // convert cv to variance on log scale
            else if(scale==2) var=log(1.0+var/square(pred)); // convert observation variance to log scale
            else if(scale==0) var=1.0;                    // automatic equal weighting
          if(modify>0) var+=modifier; else if(modify<0) var*=modifier;
          if(var<=0) cout << "Non-positive log-scale variance: " << var << " " << modifier << endl;
          if(count==1) answer= 0.5*( square(log(obs/pred+1.0E-10))/var + log(var) );
            else       answer= 0.5*( square( log(obs/pred+1.0E-10)-rho*log(obs_1/pred_1+1.0E-10) )/var + log(var) );
 	    break;
        case 2: // autocorrelated normal
          if(var<0)                     var=square(var*pred);      // convert cv to variance on observation scale
            else if(scale==1) var=square(pred)*(mfexp(var)-1); // convert log-scale variance to observation scale
            else if(scale==0) var=1.0;                    // automatic equal weighting
          if(modify>0) var+=modifier; else if(modify<0) var*=modifier;
          if(var<=0) cout << "Non-positive variance: " << var << " " << modifier << endl;
          if(count==1) answer= 0.5*( square(obs-pred)/var + log(var) );
            else       answer= 0.5*( square( (obs-pred)-rho*(obs_1-pred_1) )/var + log(var) );
          break;
        case 3: // uniform
          if(pred>=lower(count) && pred<=upper(count)) answer= log(upper(count)-lower(count));
          else answer=1.0e+32;
          break;
        case 4: // uniform on log-scale
          if(pred>=lower(count) && pred<=upper(count)) answer= log(log(upper(count)/lower(count)));
          else answer=1.0e+32;
          break;
        case 5: // gamma
          if(var<0)                     var=square(var*pred);      // convert cv to variance on observation scale
            else if(scale==1) var=square(pred)*(mfexp(var)-1); // convert log-scale variance to observation scale
            else if(scale==0) var=1.0;                    // automatic equal weighting
          if(modify>0) var+=modifier; else if(modify<0) var*=modifier;
          if(var<=0) cout << "Non-positive variance: " << var << " " << modifier << endl;
          alph=pred*pred/var; beta=var/pred;
          if(pred>0) answer= alph*log(beta)-(alph-1)*log(obs)+obs/beta+gammln(alph);
          else answer=1.0e+32;
          break;
        case 6: // beta
          if(var<0)                     var=square(var*pred);      // convert cv to variance on observation scale
            else if(scale==1) var=square(pred)*(mfexp(var)-1); // convert log-scale variance to observation scale
            else if(scale==0) var=1.0;                    // automatic equal weighting
          var=var/square(upper(count)-lower(count));             // rescale variance to beta (0,1) scale
          if(var<=0) cout << "Non-positive variance: " << var << endl;
          pred=(pred-lower(count))/(upper(count)-lower(count));     // rescale prediction to beta (0,1) scale
          obs=(obs-lower(count))/(upper(count)-lower(count));       // rescale observation to beta (0,1) scale
          alph=(pred*pred-pred*pred*pred-pred*var)/var; beta=alph*(1/obs-1);
          if(pred>=0 && pred<=1) answer= (1-alph)*log(obs)+(1-beta)*log(1-obs)-gammln(alph+beta)+gammln(alph)+gammln(beta);
          else answer=1.0e+32;
          break;
        default: // no such pdf accomodated
          cout << "The pdf must be either 1 (lognormal) or 2 (normal)" << endl;
          cout << "Presently it is " << pdf << endl;
          exit(0);
      }
    }
    return answer;
  }

 //-----------------------------------------------------------------------------------
  dvariable get_function_parameters(int &i, int &i_in, int iph, int current_phase, dvariable best, int pdf)
 //-----------------------------------------------------------------------------------
  {
    if(pdf==3 || pdf==4 || pdf==6) i_in=i; else i_in=i_one;
    i=i+1;
    return best; 
  }
 
 //-----------------------------------------------------------------------------------
  dvariable function_value(int nature, dvar_vector par_func, dvariable obs)
 //-----------------------------------------------------------------------------------
  {
    dvariable answer;

    // constants
    if(nature==1 || nature==13 || nature==14 || nature==50)
      return par_func(1);

    // polynomial of degree nature-1
    else if( nature<5) {
      if(obs == zero) return par_func(1);
      else {
        answer=par_func(1);
        for(int j=2; j<nature; j++) answer=answer+par_func(j)*pow(obs,j-1);
        return answer+par_func(nature)*pow(obs,nature-1);  // trick to avoid calculating the derivative of the final sum twice
      }
    }

    // knife edge selectivity function
    else if( nature==5) {
      if(obs < par_func(1) ) return 0; else return 1;
    }

    // logisitic selectivity function
    else if( nature==6) {
      return 1/(1+mfexp(-(obs-par_func(1))/par_func(2)));
    }

    // gamma selectivity function in terms of mode and CV (assuming sel. of oldest age is constant)
    else if( nature==7) {
      return pow((mfexp(1-obs/par_func(1))*obs/par_func(1)),1.0/square(par_func(2))-1.0);
    }

    // Chapman-Richards growth function (reduces to vonB with par_func(3)=1
    else if( nature==8) {
      if(par_func(5)<=0 || par_func(1) <=0 || (1-par_func(4)*mfexp(-par_func(2)*(obs-par_func(3))))<=0) cout << "Error in growth parameters" << endl;
      return mfexp(log(par_func(5))+par_func(6)*(log(par_func(1))+log(1-par_func(4)*mfexp(-par_func(2)*(obs-par_func(3))))/par_func(4))) ;
    }

    // Gompertz growth function
    else if( nature==9) {
      return par_func(1)*mfexp(-mfexp(-par_func(2)*(obs-par_func(3))));
    }

    // Beverton and Holt asymptotic function (par_func(1)=alpha-1)
    else if( nature==10) {
      return (par_func(1)+1)*obs/(1+obs*par_func(1));
    }

    // Ricker function (par_func(1)=alpha-1)
    else if( nature==11) {
      return obs*pow(par_func(1)+1,1-obs);
    }

    // power function y=a*x**b
    else if( nature==12) {
      if(obs == zero)return zero;
      else return par_func(1)*pow(obs,par_func(2));
    }

    // invalid function type
    else {
      cout << "No such function type accomodated" << endl; exit(0);
      return answer;
    }
  }

 //-----------------------------------------------------------------------------------
  double get_variance(dvariable pred,dvariable var,dvariable modifier, int pdf,int scale, int modify)
 //-------------------------------------------------------------
  {
    switch(pdf) {
      case 1: // autocorrelated lognormal
        if(pred<0) pred=1.0E-10;
        if(var<0)                     var=log(1.0+var*var) ;      // convert cv to variance on log scale
          else if(scale==2) var=log(1.0+var/pred/pred); // convert observation variance to log scale
          else if(scale==0) var=1.0;                    // automatic equal weighting
          if(modify>0) var+=modifier; else if(modify<0) var*=modifier;
        break;
      case 2: // autocorrelated normal
        if(var<0)                    var=var*var*pred*pred;      // convert cv to variance on observation scale
          else if(scale==1) var=pred*pred*(mfexp(var)-1); // convert log-scale variance to observation scale
          else if(scale==0) var=1.0;                    // automatic equal weighting
          if(modify>0) var+=modifier; else if(modify<0) var*=modifier;
        break;
      default: // no such pdf accomodated
        exit(0);
    }
    return value(var);
  }


 //-----------------------------------------------------------------------------------
  dvariable spr(dvar_vector pp, dvar_vector ww, dvar_vector mm, dvar_vector ss, dvariable ff, dvariable tau ,int na)
 //  Computes equilibrium spawn per recruit
 //-----------------------------------------------------------------------------------
  {  
    dvariable answer;
    dvariable survive;
    dvariable zz;
    survive=1;
    answer=0;
    for (a=1; a<na; a++) {
      zz=mm(a)+ff*ss(a);
      answer+=pp(a)*ww(a)*mfexp(-zz*tau)*survive;
      survive=survive*mfexp(-zz);
    }
    zz=mm(na)+ff*ss(na);
    return answer+pp(na)*ww(na)*mfexp(-zz*tau)*survive/(1-mfexp(-zz));
  }

 //-----------------------------------------------------------------------------------
  dvariable ypr(dvar_vector ww, dvar_vector mm, dvar_vector ss, dvariable ff,int na)
 //  Computes equilibrium yield per recruit
 //-----------------------------------------------------------------------------------
  {  
    dvariable answer;
    dvariable survive;
    dvariable zz;
    survive=1;
    answer=0;
    for (a=1; a<na; a++) {
      zz=mm(a)+ff*ss(a);
      answer+=ww(a)*ss(a)*(1-mfexp(-zz))*survive/zz;
      survive=survive*mfexp(-zz);
    }
    zz=mm(na)+ff*ss(na);
    return ff*(answer+ww(na)*ss(na)*survive/zz);
  }

 //-----------------------------------------------------------------------------------
  dvariable equilibrium_ssb(int nature, dvar_vector par_func, dvariable spratio)
 //  Computes equilibrium spawning biomass
 //-----------------------------------------------------------------------------------
  {  
    // Beverton and Holt asymptotic function
    if( nature==10)      return ( (par_func(1)+1)*spratio-1.0 )/par_func(1);      // Beverton and Holt asymptotic function in terms of (alpha-1)
    else if( nature==11) return 1.0 + log(spratio)/log(par_func(1)+1);            // Ricker dome function in terms of (alpha-1)
  }

 //-----------------------------------------------------------------------------------
  dvariable goldensection(int typ, dvariable bf, dvar_vector ww, dvar_vector mm, dvar_vector ss, int na, dvar_vector mat, dvar_vector fec, dvariable tau, dvariable spr00, int sr_nature, dvar_vector par_func)
 //  Computes F's at maximum equilibrium yield per recruit and MSY
 //-----------------------------------------------------------------------------------
  {  
    dvariable y1, y2, f0, f1, f2, f3, af, cf, sprtemp, slope0;
    double g1, g2;
    int iter;
    af=0.0001; cf=3.0; g1=0.618034; g2=0.381966; 
    if(typ==i_two) {
      for (iter=1; iter<29; iter++) {
        cf=cf-0.1; 
        sprtemp=spr(mat, fec, mm, ss, cf, tau, na)/spr00; y1=equilibrium_ssb(sr_nature,par_func,sprtemp)/sprtemp; 
        if(y1>0) break;
      }
    }
    if(bf>(cf-0.1)) bf=bf-(bf-cf+0.1);
    f0=af; f3=cf;

    if(fabs(cf-bf)>fabs(bf-af)) { f1=bf; f2=bf+g2*(cf-bf); }
    else { f2=bf; f1=bf-g2*(bf-af); }
    y1= -ypr(ww, mm, ss, f1, na); y2= -ypr(ww, mm, ss, f2, na); // yield per recruit
    if(typ==3) { slope0=0.1*ypr(ww, mm, ss, 0.001, na); y1=fabs(slope0+y1+ypr(ww, mm, ss, f1-0.001, na)); y2=fabs(slope0+y2+ypr(ww, mm, ss, f2-0.001, na)); }
    if(typ==i_two) { 
       sprtemp=spr(mat, fec, mm, ss, f1, tau, na)/spr00; y1=y1*equilibrium_ssb(sr_nature,par_func,sprtemp)/sprtemp; 
       sprtemp=spr(mat, fec, mm, ss, f2, tau, na)/spr00; y2=y2*equilibrium_ssb(sr_nature,par_func,sprtemp)/sprtemp; 
    }
    for (iter=1; iter<21; iter++) {
      if(y2<y1) { 
        f0=f1; f1=f2; f2=g1*f1+g2*f3; y1=y2; y2= -ypr(ww, mm, ss, f2, na); 
        if(typ==3) y2=fabs(slope0+y2+ypr(ww, mm, ss, f2-0.001, na));
        if(typ==i_two) {sprtemp=spr(mat, fec, mm, ss, f2, tau, na)/spr00; y2=y2*equilibrium_ssb(sr_nature,par_func,sprtemp)/sprtemp; }
      }
      else      { 
        f3=f2; f2=f1; f1=g1*f2+g2*f0; y2=y1; y1= -ypr(ww, mm, ss, f1, na); 
        if(typ==3) y1=fabs(slope0+y1+ypr(ww, mm, ss, f1-0.001, na));
        if(typ==i_two) {sprtemp=spr(mat, fec, mm, ss, f1, tau, na)/spr00; y1=y1*equilibrium_ssb(sr_nature,par_func,sprtemp)/sprtemp; }
      }
    }
    if(y1<y2) return f1;
    else return f2;
  }
