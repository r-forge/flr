// Maximum lengths of the staticly allocated matrices

// Maximum lengths of the staticly allocated matrices
// Same as in DIMS but there is aproblem reading them in C
extern "C" int ARRAYLEN_mp_NAGES; 	   // maximum no of ages including + gp
extern "C" int ARRAYLEN_mp_NYRS; 	   // maximum no of years in analysis
extern "C" int ARRAYLEN_mp_MAXNSURVEYS; 	// maximum no of age-structured surveys
extern "C" int ARRAYLEN_mp_MAXNBSURVEYS; 	// maximum number of SSB surveys
extern "C" int ARRAYLEN_mp_NDATA; 		// maximum no of observations 2500
extern "C" int ARRAYLEN_mp_NPARM; 		// maximum no of parameters
extern "C" int ARRAYLEN_mp_NITERS; 		// maximum iterations in uncertainty eval.

extern "C" int DIMS_mp_maxage;	    //maximum no of ages including + gp
extern "C" int DIMS_mp_maxyear;	    //maximum no of years in analysis
extern "C" int DIMS_mp_maxsurvey;	 //maximum no of age-structured surveys
extern "C" int DIMS_mp_maxbsurv;	    //maximum number of SSB surveys
extern "C" int DIMS_mp_maxdata;	    //maximum no of observations 2500
extern "C" int DIMS_mp_maxparm;	    //maximum no of parameters
extern "C" int DIMS_mp_maxiters;   	 //maximum iterations in uncertainty eval.
extern "C" int DIMS_mp_fbyear;
extern "C" int DIMS_mp_lbyear;
extern "C" int DIMS_mp_nageix;
extern "C" int DIMS_mp_nssbix;

extern "C" double DIMS_mp_missing;

//STCK these are the input population data and estimates
extern "C" int    STCK_mp_FIRSTAGE;
extern "C" int    STCK_mp_LASTAGE;
extern "C" int    STCK_mp_FIRSTYEAR;
extern "C" int    STCK_mp_LASTYEAR;
extern "C" double STCK_mp_CN[DIMS_NAges][DIMS_NYrs];				// Catch Number 
extern "C" double STCK_mp_NM[DIMS_NAges][DIMS_NYrs];				// Natural Mortality
extern "C" double STCK_mp_SW[DIMS_NAges][DIMS_NYrs];				// Stock Weight
extern "C" double STCK_mp_MO[DIMS_NAges][DIMS_NYrs];				// Maturity Ogive
extern "C" double STCK_mp_CW[DIMS_NAges][DIMS_NYrs];				// Stock Weight
extern "C" double STCK_mp_LA[DIMS_NYrs];					         // Reported Landings 
extern "C" double STCK_mp_PF;                                  // Proportions of F before spawning
extern "C" double STCK_mp_PM;                                  // Proportions of M before spawning
extern "C" double STCK_mp_N[DIMS_NAges][DIMS_NYrs];				// Numbers-at-age
extern "C" double STCK_mp_F[DIMS_NAges][DIMS_NYrs];				// F-at-age

// CONTROL Parameters and control flags for fitting the separable model
extern "C" int    CONTROL_mp_MAXSEP;     
extern "C" bool   CONTROL_mp_DLLFLAG;    // Flag to indicate whether DLL][ if true turns off cosole IO
extern "C" bool   CONTROL_mp_USESEP;     // not used
extern "C" bool   CONTROL_mp_WRITEOUT;   // whether objective function writes out residuals to disk
extern "C" bool   CONTROL_mp_FULL;       // whether objective funtion recalculates whole VPA][ or only as far back as is currently required 
extern "C" int    CONTROL_mp_NYSEP;      // NySep - Number of years for separable model 
extern "C" int    CONTROL_mp_REFAGE;     // RefAge - Reference age for fitting the separable model  
extern "C" int    CONTROL_mp_RWOPT;

// Parameter list +/- standard deviation 
extern "C" double CONTROL_mp_RELWT[DIMS_NAges];	 // Relative weights by age 
extern "C" double CONTROL_mp_YEARWT[DIMS_NYrs];	 // Relative weights by year  
extern "C" double CONTROL_mp_TERMS;        // selection on last true reference age 

// Weighting matrices for catch-at-age; for aged surveys; for SSB surveys
extern "C" double CONTROL_mp_W[DIMS_NAges][DIMS_NYrs];  
		
extern "C" bool   CONTROL_mp_TWOSEL;    // whether to have 2 selection patterns 
extern "C" bool   CONTROL_mp_STEPSEL;   // whether change in selection is step or gradual
extern "C" int    CONTROL_mp_CHANGESEL; // last year for constant selection][ if 2 sel patterns chosen
extern "C" double CONTROL_mp_TERMS2;
extern "C" double CONTROL_mp_CNLAMBDA[DIMS_NAges][DIMS_NYrs];
extern "C" double CONTROL_mp_SRRLAMBDA; // weight for the SRR term in the objective function

// Stock and recruitment parameters [from SRR.INC]
extern "C" bool   CONTROL_mp_FITSRR;  // whether to fit a stock-recruit relation
extern "C" int    CONTROL_mp_LAG;   // time lag between spawning and firstage

extern "C" int    CONTROL_mp_FYCHOSEN; // first and last years of choice for SRR relation
extern "C" int    CONTROL_mp_LYCHOSEN; // first and last years of choice for SRR relation

extern "C" double CONTROL_mp_IXCOR[DIMS_NSurveys];

// STATS statistics
extern "C" int  STATS_mp_NXDATA;      // NxData - Number of observations 
extern "C" int  STATS_mp_NXPARM;      // NxParm - Number of parameters     
extern "C" int  STATS_mp_NOCDATA;
extern "C" int  STATS_mp_dfsep;
extern "C" int  STATS_mp_NoSRRdata;
extern "C" int  STATS_mp_NoBdata[DIMS_NBSurveys]; 
extern "C" int  STATS_mp_NoAdata[DIMS_NAges][DIMS_NSurveys];
		
extern "C" double	 STATS_mp_SRRVar;
extern "C" double  STATS_mp_CVar;
extern "C" double  STATS_mp_Cchi2;
extern "C" double  STATS_mp_CPchi;
extern "C" double  STATS_mp_CCV;
extern "C" double  STATS_mp_Cskew;
extern "C" double  STATS_mp_CKurt;
     
extern "C" double  STATS_mp_BVar[DIMS_NBSurveys];
extern "C" double  STATS_mp_Bchi2[DIMS_NBSurveys];
extern "C" double  STATS_mp_BPchi[DIMS_NBSurveys];
extern "C" double  STATS_mp_BCV[DIMS_NBSurveys];
extern "C" double  STATS_mp_Bskew[DIMS_NBSurveys];
extern "C" double  STATS_mp_BKurt[DIMS_NBSurveys];

extern "C" double  STATS_mp_AVar[DIMS_NAges][DIMS_NSurveys];
extern "C" double  STATS_mp_Achi2[DIMS_NAges][DIMS_NSurveys];;
extern "C" double  STATS_mp_APchi[DIMS_NAges][DIMS_NSurveys];;
extern "C" double  STATS_mp_ACV[DIMS_NAges][DIMS_NSurveys];;
extern "C" double  STATS_mp_Askew[DIMS_NAges][DIMS_NSurveys];;
extern "C" double  STATS_mp_AKurt[DIMS_NAges][DIMS_NSurveys];;

extern "C" double  STATS_mp_XBEST[DIMS_NParm];
extern "C" double  STATS_mp_XLOW[DIMS_NParm];
extern "C" double  STATS_mp_XHIGH[DIMS_NParm];

// CPUE Parameters and data for CPUE
extern "C" int CPUE_mp_NAGEIX;
extern "C" int CPUE_mp_NSSBIX;
extern "C" int CPUE_mp_FBYEAR;
extern "C" int CPUE_mp_LBYEAR;
extern "C" int CPUE_mp_FAGE[DIMS_NSurveys];
extern "C" int CPUE_mp_LAGE[DIMS_NSurveys];
extern "C" int CPUE_mp_FYEAR[DIMS_NSurveys];
extern "C" int CPUE_mp_LYEAR[DIMS_NSurveys];
extern "C" int CPUE_mp_QAPARM[DIMS_NSurveys];       // number of fitted parameters for catchability relationship
extern "C" int CPUE_mp_QBPARM[DIMS_NSurveys]; 
extern "C" bool   CPUE_mp_RecalculatePopulations;
extern "C" int    CPUE_mp_XPLUSGP[DIMS_NSurveys]; // plusgp[n] is whether the last age of the nth age-structured survey is a plus-group
extern "C" bool   CPUE_mp_PLUSGP[DIMS_NSurveys]; // plusgp[n] is whether the last age of the nth age-structured survey is a plus-group
extern "C" double CPUE_mp_ALAMBDA[DIMS_NAges][DIMS_NSurveys];  
extern "C" double CPUE_mp_BLAMBDA[DIMS_NBSurveys]; 
  
extern "C" double CPUE_mp_MaxWeight;
extern "C" double CPUE_mp_NextRec;

extern "C" double CPUE_mp_TIMING[DIMS_NSurveys];	    // Timing of each of the age-structured surveys in fractions of a year
extern "C" double CPUE_mp_X[DIMS_NParm];
extern "C" double CPUE_mp_S[DIMS_NAges];
extern "C" double CPUE_mp_SOP[DIMS_NYrs]; 
extern "C" double CPUE_mp_Npool[DIMS_NYrs]; // dynamic pool plus-gp Ns
extern "C" double CPUE_mp_BSURVEY[DIMS_NYrs][DIMS_NBSurveys];	// SSB survey data indexed as [survey][ year]
extern "C" double CPUE_mp_COV[DIMS_NParm][DIMS_NParm];  
extern "C" double CPUE_mp_FPerceived[DIMS_NAges][DIMS_NYrs]; 
extern "C" double CPUE_mp_NPerceived[DIMS_NAges][DIMS_NYrs];
extern "C" double CPUE_mp_ASURVEY[DIMS_NAges][DIMS_NYrs][DIMS_NSurveys];    // Age structured survey data indexed as [survey][ year][ age]

// RESULTS parameter estimates][ fitted values from the model fit
extern "C" int    RESULTS_mp_HiFage;      // Lowest age for calcualting arithmetic mean F][ for output table  
extern "C" int    RESULTS_mp_LoFage;      // Highest age for calcualting arithmetic mean F][ for output table  
extern "C" double RESULTS_mp_PREDCN[DIMS_NAges][DIMS_NYrs];
extern "C" double RESULTS_mp_PREDAINDEX[DIMS_NAges][DIMS_NYrs][DIMS_NSurveys]; // Age structured survey data indexed
extern "C" double RESULTS_mp_PREDBINDEX[DIMS_NYrs][DIMS_NBSurveys];   // SSB survey data indexed as [survey][ year] 
extern "C" double RESULTS_mp_A;   // Parameters of the Beverton and Holt SRR
extern "C" double RESULTS_mp_B;   // Parameters of the Beverton and Holt SRR
extern "C" double RESULTS_mp_PredRecruit[DIMS_NYrs];


