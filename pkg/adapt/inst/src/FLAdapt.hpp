#ifndef _INC_fladapt
#define _INC_fladapt

#ifdef WIN32
   #include <windows.h>
   #define SEXPDLLExport __declspec(dllexport) SEXP __cdecl    
#else
   #define SEXPDLLExport SEXP    
#endif

#include <math.h>
#include <float.h>
#include "Rdefines.h"
#include "Rinternals.h"
   
extern "C" void VPA(void);
extern "C" void VPA_INIT(void);
extern "C" void VPA_DATA(void);
extern "C" void VPA_RUN(void);
extern "C" void VPA_RUN_SCRNIO(void);
extern "C" void VPA_OUTPUT(void);
extern "C" void VPA_PARAMETERS(void);
extern "C" int __stdcall VPA_WRITE_PARAMETERS(double [200][11]);
extern "C" void __stdcall VPA_SCALECPUE(void);

const int                As =  20; // maximum age
const int                Ys = 100; // maximum no of years 
const int                Bs =   2;
const int                Gs =  20;
const int DIMS_NAges   =       20; // maximum age
const int DIMS_NYrs    =      100; // maximum no of years 
const int DIMS_NStocks =        2;
const int DIMS_NFleets =       20;

const int              NPar =2000;

extern "C" double 
PARAMETERS_mp_PARM_SPECS[NPar][5],
PARAMETERS_mp_PARM_EST[NPar],
PARAMETERS_mp_PARM_VAR[NPar],
PARAMETERS_mp_PDEV,
PARAMETERS_mp_SIGMA_FT,
PARAMETERS_mp_SIGMA_REC,
PARAMETERS_mp_SIGMA_STOCK,
PARAMETERS_mp_RATIO_STOCK,
PARAMETERS_mp_CV_OVERIDE,
PARAMETERS_mp_SIGMA_TAG_MOD,
PARAMETERS_mp_FISHING_PRESSURE[12];

extern "C" int 
PARAMETERS_mp_PARM_KEY[NPar+2],
PARAMETERS_mp_OPTION[10],
PARAMETERS_mp_SEED,
PARAMETERS_mp_MAXITER,
PARAMETERS_mp_CHECKFLAG,
PARAMETERS_mp_MODEL_TYPE,
PARAMETERS_mp_SCALES,
PARAMETERS_mp_ICALL,
PARAMETERS_mp_NBOX,
PARAMETERS_mp_LINK_FT,
PARAMETERS_mp_FIRSTCALL,
PARAMETERS_mp_LINK_REC,
PARAMETERS_mp_LINK_STOCK,
PARAMETERS_mp_PDF_TAG,
PARAMETERS_mp_ADD_VAR,
PARAMETERS_mp_N_RETRO,
PARAMETERS_mp_N_RETRO_LOOP,
PARAMETERS_mp_LINK_YOUNGEST,
PARAMETERS_mp_LINK_OLDEST,
PARAMETERS_mp_STINE_CORR,
PARAMETERS_mp_DATE_VALUES[8];

extern "C" char 
PARAMETERS_mp_CONFILE[70];

extern "C" double DATUM_mp_CATCH_DATA[Ys+1][As+1][Bs];
extern "C" double DATUM_mp_WEIGHT_CATCH[Ys][As+1][Bs][Gs];
extern "C" double DATUM_mp_FECUNDITY[As+1][Bs];
extern "C" double DATUM_mp_WEIGHT_SSB[Ys+1][As+1][Bs];

extern "C" double DATUM_mp_EFFORT_DATA[Ys+1][Bs][Gs];
extern "C" double DATUM_mp_PSEL[Ys][As+1][Bs][Gs];
extern "C" double DATUM_mp_SIGMA_CATCH[Bs];
extern "C" double DATUM_mp_SIGMA_EFFORT[Ys+1][Bs][Gs];
extern "C" double DATUM_mp_SIGMA_M[Ys+1][As+1][Bs];
extern "C" double DATUM_mp_SIGMA_T[Ys+1][As+1][Bs];
extern "C" double DATUM_mp_SIGMA_F[Ys+1][Bs];
extern "C" double DATUM_mp_SIGMA_FRATIO[Ys+1][Bs];
extern "C" double DATUM_mp_SIGMA_TERMINAL[As+1][Bs];
extern "C" double DATUM_mp_SIGMA_SCALE_VARIANCE[Bs][Gs];
extern "C" double DATUM_mp_SIGMA_TAG_REPORT[Ys][As+1][Bs];
extern "C" double DATUM_mp_SIGMA_TAG_SURV[Ys][As+1][Bs];
extern "C" double DATUM_mp_SIGMA_TAG_LOSS[Ys][As+1][Bs];
extern "C" double DATUM_mp_SIGMA_Q[Ys+1][Bs][Gs];
extern "C" double DATUM_mp_INDEX_MEAN[Bs][Gs];
extern "C" double DATUM_mp_RDATA[Ys][Ys][As+1][Bs][Bs];
extern "C" double DATUM_mp_NEWTAGS[Ys][As+1][Bs];
extern "C" double DATUM_mp_SIGMA_TAG[Ys][As+1][Bs];
extern "C" double DATUM_mp_EFFORT_DATA_STORE[Ys+1][Bs][Gs];
extern "C" double DATUM_mp_SUMRDATA[Ys][As+1][Bs];
extern "C" double DATUM_mp_TAG_TIME[Ys][As+1][Bs];
extern "C" double DATUM_mp_SIGMA_TAG_NOMIX[Ys][As+1][Bs];
extern "C" double DATUM_mp_SIGMA_TAG_NOMIX2[Ys][As+1][Bs];
extern "C" double DATUM_mp_FISH_TIME[Ys][As+1][Bs];


extern "C" int DATUM_mp_PDF_CATCH[Bs];
extern "C" int DATUM_mp_PDF_EFFORT[Bs][Gs];
extern "C" int DATUM_mp_BIO_EFFORT[Bs][Gs];
extern "C" int DATUM_mp_BIO_CATCH[Bs];
extern "C" int DATUM_mp_PDF_STOCKRECRUIT;
extern "C" int DATUM_mp_SEL_TYPE[Bs][Gs];
extern "C" int DATUM_mp_PDF_FRATIO[Bs];
extern "C" int DATUM_mp_PDF_TERMINAL[Bs];
extern "C" int DATUM_mp_PDF_M[Bs];
extern "C" int DATUM_mp_PDF_T[Bs];
extern "C" int DATUM_mp_BOTH_STOCKS[Bs][Gs];
extern "C" int DATUM_mp_IGNORE_RECRUIT[2];
extern "C" int DATUM_mp_N_DATA;
extern "C" int DATUM_mp_N_QS;
extern "C" int DATUM_mp_NPI[Bs][Gs];

extern "C" char DATUM_mp_TITLE_FISHERY[Bs][50];
extern "C" char DATUM_mp_TITLE_EFFORT[Gs][Bs][50];

extern "C" double STATISTICS_mp_N_STOCK[Ys+1][As+1][Bs];
extern "C" double STATISTICS_mp_N_AREA[Ys+1][As+1][Bs];
extern "C" double STATISTICS_mp_F[Ys+1][As+1][Bs];
extern "C" double STATISTICS_mp_M[Ys+1][As+1][Bs];
extern "C" double STATISTICS_mp_T[Ys+1][As+1][Bs];
extern "C" double STATISTICS_mp_EFFORT[Ys+1][Bs][Gs];
extern "C" double STATISTICS_mp_SEL_EFFORT[Ys][As+1][Bs][Gs];
extern "C" double STATISTICS_mp_Q_EFFORT[Ys][Bs][Gs];
extern "C" double STATISTICS_mp_PLUSAGE[Ys+1][Bs];
extern "C" double STATISTICS_mp_STOCK_RECRUIT[5][Bs];
extern "C" double STATISTICS_mp_SSB[Ys][Bs];
extern "C" double STATISTICS_mp_RECRUITS[Ys][Bs];
extern "C" double STATISTICS_mp_SCALE_VARIANCE[Bs][Gs];
extern "C" double STATISTICS_mp_SEL_TERMINAL[2][As+1][Bs];
extern "C" double STATISTICS_mp_F_RATIO[Ys][Bs];
extern "C" double STATISTICS_mp_TAG_LOSS[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_TAG_REPORT[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_TAG_SURV[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_N_TAGS[Ys][Ys][As+1][Bs][Bs];
extern "C" double STATISTICS_mp_R[Ys][Ys][As+1][Bs][Bs];
extern "C" double STATISTICS_mp_ETA_Q[Ys][Bs][Gs];
extern "C" double STATISTICS_mp_ETA_F[Ys][Bs];
extern "C" double STATISTICS_mp_ETA_SR[Ys][Bs];
extern "C" double STATISTICS_mp_ETA_T[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_ETA_M[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_ETA_TERMINAL[As+1][Bs];
extern "C" double STATISTICS_mp_ETA_TAG_SURV[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_ETA_TAG_LOSS[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_ETA_TAG_REPORT[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_TAG_NOMIX[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_ETA_TAG_NOMIX[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_ETA_SCALE_VARIANCE[Bs][Gs];
extern "C" double STATISTICS_mp_TAG_NOMIX2[Ys][As+1][Bs];
extern "C" double STATISTICS_mp_ETA_TAG_NOMIX2[Ys][As+1][Bs];

extern "C" int STATISTICS_mp_FIRSTYEAR;
extern "C" int STATISTICS_mp_LASTYEAR;
extern "C" int STATISTICS_mp_DISPLAYYEAR;
extern "C" int STATISTICS_mp_NYEARS;
extern "C" int STATISTICS_mp_FIRSTAGE;
extern "C" int STATISTICS_mp_LASTAGE;
extern "C" int STATISTICS_mp_YEAR;
extern "C" int STATISTICS_mp_AGE;
extern "C" int STATISTICS_mp_GEAR;
extern "C" int STATISTICS_mp_BOX;
extern "C" int STATISTICS_mp_PLUSGROUP;
extern "C" int STATISTICS_mp_NGEARS[Bs];
extern "C" int STATISTICS_mp_SEASON_EFFORT[Bs][Gs];
extern "C" int STATISTICS_mp_SEASON_SSB[Bs];
extern "C" int STATISTICS_mp_AGE_EFFORT[Bs][Gs][2];
extern "C" int STATISTICS_mp_STARTYEAR[Bs];
extern "C" int STATISTICS_mp_ENDYEAR[Bs];

extern "C" double LOGLIKELIHOODS_mp_LIKE_RECRUITMENT[Bs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_EFFORT[Bs][Gs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_TAG[Bs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_M[Bs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_T[Bs];
extern "C" double LOGLIKELIHOODS_mp_PENALTY;
extern "C" double LOGLIKELIHOODS_mp_LIKELIHOODS;
extern "C" double LOGLIKELIHOODS_mp_POSTERIORS;
extern "C" double LOGLIKELIHOODS_mp_CONSTRAINTS;
extern "C" double LOGLIKELIHOODS_mp_LIKE_Q[Bs][Gs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_F[Bs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_TERMINAL[Bs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_TAG_REPORT[Bs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_TAG_SURV[Bs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_TAG_LOSS[Bs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_TAG_NOMIX[Bs];
extern "C" double LOGLIKELIHOODS_mp_LIKE_V[Bs];
extern "C" double LOGLIKELIHOODS_mp_SUM_EFFORT_DISCREPANCY[Bs][Gs];
extern "C" double LOGLIKELIHOODS_mp_EFFORT_DISCREPANCY[Ys][Bs][Gs];
extern "C" double LOGLIKELIHOODS_mp_DISCREPANCY;

extern "C" char LOGLIKELIHOODS_mp_PDFNAME[13][30];
extern "C" char LOGLIKELIHOODS_mp_CONSTANTS[1];

#endif /* _INC_fladapt */
