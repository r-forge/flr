//#define __MSVC32__

//#define __cplusplus

//#include <windows.h>
//#include "rdefines.h"
//#include "rinternals.h"
#include "FLAdapt.hpp"

//HWND      hWndMain;   
//HINSTANCE hInst;    

//32 bit Entry point

//#define CLASS_NAME_BUFFER  50

bool  InputStock(SEXP);
short InputCPUE(SEXP);
bool  InputControl(SEXP);

bool InputFLQuantAB(  SEXP, double [As+1][Bs],           short, short,  short Stock=0);
bool InputFLQuantYAB( SEXP, double [Ys+1][As+1][Bs],     int , int , int, int, short Stock=0);
bool InputFLQuantYBG( SEXP, double [Ys+1][Bs][Gs],       int, int, short, short Stock=0);
bool InputFLQuantYABG(SEXP, double [Ys+1][As+1][Bs][Gs], int , int , int, int, short, short Stock=0);
void InputRange(SEXP, short *, short *, short *, short *, short *, double *, double *);

SEXP CreateFLQuantYAB(double [Ys+1][As+1][Bs], int, int, int, int, short Stock=0);
SEXP CreateFLAdapt(void);
SEXP CreateControl(void);
SEXP CreateArray(double [200][11], int);

short GetVPA2BoxPDF(SEXP);
short GetVPA2BoxUnits(SEXP);
short GetVPA2BoxSelectivity(SEXP);

int NElemList(SEXP);

extern "C" SEXPDLLExport FLAdapt(SEXP xStock, SEXP xCPUE, SEXP xControl) 
   {
   SEXP ReturnObject = R_NilValue;

   VPA_INIT();

//   VPA_DATA(); 

   InputControl(xControl);
   InputStock(xStock);
   InputCPUE(xCPUE);

   VPA_RUN();
   
   VPA_OUTPUT();

   ReturnObject = CreateFLAdapt();

   return ReturnObject; 
   }

extern "C" SEXPDLLExport readFLAdaptControl(SEXP xFile) 
   {
   SEXP ReturnObject = R_NilValue;

   char *_CONFILE = CHAR(STRING_ELT(xFile, 0));


   strcpy(PARAMETERS_mp_CONFILE, _CONFILE);

   VPA_DATA(); 
   
   ReturnObject = CreateControl();

   return ReturnObject; 
   }
/*
BOOL WINAPI DllMain(HINSTANCE hDLL, DWORD dwReason, LPVOID lpReserved)
   {					
   extern HWND      hWndMain;
   extern HINSTANCE hInst;    

   hInst = hDLL; 
 
   return TRUE;
   }

extern "C" int FAR PASCAL WEP (int nArgument)
   {
   return 1;
   } 
*/
extern "C" SEXPDLLExport FLVersion(void)
   {
   char szBuf[255];

   wsprintf((LPSTR)szBuf, " FLRVPA.DLL version 0.5-1 built on %s at %s", __DATE__, __TIME__);

   SEXP ReturnObject = R_NilValue;

   PROTECT(ReturnObject = NEW_CHARACTER(1));
   SET_STRING_ELT(ReturnObject, 0, COPY_TO_USER_STRING(szBuf));

   UNPROTECT(1);

   return ReturnObject;
   }

bool InputStock(SEXP xStock)
   {
   short MinAge, MaxAge, MinYear, MaxYear, PlusGroup;
   double Start, End;

   SEXP range  = PROTECT(duplicate(GET_SLOT(xStock, install("range"))));    
  
   InputRange(range, &MinAge, &MaxAge, &PlusGroup, &MinYear, &MaxYear, &Start, &End);
  
   STATISTICS_mp_FIRSTAGE  = MinAge;
   STATISTICS_mp_LASTAGE   = MaxAge;
   STATISTICS_mp_PLUSGROUP = PlusGroup;
   if (STATISTICS_mp_PLUSGROUP <= STATISTICS_mp_FIRSTAGE) STATISTICS_mp_PLUSGROUP = STATISTICS_mp_LASTAGE;
   STATISTICS_mp_FIRSTYEAR = MinYear;
   STATISTICS_mp_LASTYEAR  = MaxYear;

   SEXP xM     = PROTECT(duplicate(GET_SLOT(xStock, install("m"))));
   SEXP xCatch = PROTECT(duplicate(GET_SLOT(xStock, install("catch.n"))));
   SEXP xSWt   = PROTECT(duplicate(GET_SLOT(xStock, install("stock.wt"))));
   SEXP xMat   = PROTECT(duplicate(GET_SLOT(xStock, install("mat"))));
   SEXP xCWt   = PROTECT(duplicate(GET_SLOT(xStock, install("catch.wt"))));
  
   InputFLQuantYAB(xCatch, DATUM_mp_CATCH_DATA, STATISTICS_mp_FIRSTAGE, STATISTICS_mp_LASTAGE, STATISTICS_mp_FIRSTYEAR, STATISTICS_mp_LASTYEAR);
   InputFLQuantYAB(xSWt,   DATUM_mp_WEIGHT_SSB, STATISTICS_mp_FIRSTAGE, STATISTICS_mp_LASTAGE, STATISTICS_mp_FIRSTYEAR, STATISTICS_mp_LASTYEAR);
   InputFLQuantAB( xMat,   DATUM_mp_FECUNDITY,                                                 STATISTICS_mp_FIRSTYEAR, STATISTICS_mp_LASTYEAR);

   STATISTICS_mp_LASTYEAR    -= STATISTICS_mp_FIRSTYEAR - 1;
   STATISTICS_mp_DISPLAYYEAR  = STATISTICS_mp_FIRSTYEAR;
   STATISTICS_mp_FIRSTYEAR    = 1;

   for (int iYear=STATISTICS_mp_FIRSTYEAR; iYear<=STATISTICS_mp_LASTYEAR; iYear++)
      for (int iAge=STATISTICS_mp_FIRSTAGE; iAge<=STATISTICS_mp_LASTAGE; iAge++)
         DATUM_mp_CATCH_DATA[iYear][iAge][0]   *= 1.0;
   
   UNPROTECT(7);
   
   return true; 
   }

short InputCPUE2(SEXP xCPUE)
   {
   short NCPUE=0, _NCPUE=0;

   //get N Indices
   _NCPUE = NElemList(xCPUE);

   if (_NCPUE <= 0) return false;

   for (short i=0; i<_NCPUE; i++)
      {
      if (i == 6) 
         i = i;

      SEXP  range   = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("range")))),  
            index   = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("index")))),
            effort  = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("effort")))),
            weights = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("weighting")))),
            prop    = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("prop")))),
            type    = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("type")))),
            pdf     = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("pdf"))));

      short MinAge, MaxAge, MinYear, MaxYear, PlusGroup;
      double Start, End;

      InputRange(range, &MinAge, &MaxAge, &PlusGroup, &MinYear, &MaxYear, &Start, &End);
      
      //Timing
      STATISTICS_mp_SEASON_EFFORT[0][i] = (short)(12*min((Start+End)/2.0,1.0));
      if (STATISTICS_mp_SEASON_EFFORT[0][i] < 0.0)  STATISTICS_mp_SEASON_EFFORT[0][i] = -1;
      //PDF
      DATUM_mp_PDF_EFFORT[        0][i] = GetVPA2BoxPDF(pdf);
      //Units
      DATUM_mp_BIO_EFFORT[        0][i] = GetVPA2BoxUnits(type);
      //Sel
      DATUM_mp_SEL_TYPE[          0][i] = GetVPA2BoxSelectivity(type);
   
      if (DATUM_mp_PDF_EFFORT[0][i] == 1 || DATUM_mp_PDF_EFFORT[0][i] == 2)
         {
         STATISTICS_mp_AGE_EFFORT[0][NCPUE][0] = MinAge;
         STATISTICS_mp_AGE_EFFORT[0][NCPUE][1] = MaxAge;
  
         InputFLQuantYBG( index,  DATUM_mp_EFFORT_DATA_STORE, MinYear, MaxYear, i);
         InputFLQuantYBG( index,  DATUM_mp_EFFORT_DATA,       MinYear, MaxYear, i);
         InputFLQuantYBG( weights,DATUM_mp_SIGMA_EFFORT,      MinYear, MaxYear, i);
         InputFLQuantYABG(prop,   DATUM_mp_PSEL,              MinAge,  MaxAge, MinYear, MaxYear, i);

         for (short j=MinYear-STATISTICS_mp_DISPLAYYEAR+1; j<=MaxYear-STATISTICS_mp_DISPLAYYEAR+1; j++)
            {
            if ((PARAMETERS_mp_CV_OVERIDE>0.00001 && (short)(PARAMETERS_mp_CV_OVERIDE)!=999) || PARAMETERS_mp_CV_OVERIDE<-0.00001) 
                  DATUM_mp_SIGMA_EFFORT[j][0][i]=PARAMETERS_mp_CV_OVERIDE;
            
            for (short iAge = MinAge; iAge <= MaxAge; iAge++)
               {
               DATUM_mp_PSEL[j][iAge][0][i] = (MinAge == MaxAge ? 1.0 : DATUM_mp_PSEL[j][iAge][0][i]);

               if      (DATUM_mp_SEL_TYPE[0][i] == 1) // fixed selectivities
                  STATISTICS_mp_SEL_EFFORT[j][iAge][0][i] = DATUM_mp_PSEL[j][iAge][0][i];
               else if (DATUM_mp_SEL_TYPE[0][i] >= 3) // partial catches
                  DATUM_mp_PSEL[j][iAge][0][i] /= DATUM_mp_CATCH_DATA[j][iAge][0];
               }
            }

         NCPUE++;
         }

      if (DATUM_mp_PDF_EFFORT[0][i] != 0 && DATUM_mp_PDF_EFFORT[0][i] != 12 && PARAMETERS_mp_OPTION[3] <= 0) 
         DATUM_mp_N_QS += 1;
		}

   STATISTICS_mp_NGEARS[0] = NCPUE;

   VPA_SCALECPUE();

   return NCPUE;
   }

short InputCPUE(SEXP xCPUE)
   {
   short NCPUE=0, _NCPUE=0;

   //get N Indices
   _NCPUE = NElemList(xCPUE);

   if (_NCPUE <= 0) return false;

   for (short i=0; i<_NCPUE; i++)
      {
      if (i == 6) 
         i = i;

      SEXP  range   = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("range")))),  
            index   = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("index")))),
            effort  = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("effort")))),
            weights = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("weighting")))),
            prop    = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("prop")))),
            type    = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("type")))),
            pdf     = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("pdf"))));

      short MinAge, MaxAge, MinYear, MaxYear, PlusGroup;
      double Start, End;

      InputRange(range, &MinAge, &MaxAge, &PlusGroup, &MinYear, &MaxYear, &Start, &End);
      
      //Timing
      STATISTICS_mp_SEASON_EFFORT[0][i] = (short)(12*min((Start+End)/2.0,1.0));
      if (STATISTICS_mp_SEASON_EFFORT[0][i] < 0.0)  STATISTICS_mp_SEASON_EFFORT[0][i] = -1;
      //PDF
      DATUM_mp_PDF_EFFORT[        0][i] = GetVPA2BoxPDF(pdf);
      //Units
      DATUM_mp_BIO_EFFORT[        0][i] = GetVPA2BoxUnits(type);
      //Sel
      DATUM_mp_SEL_TYPE[          0][i] = GetVPA2BoxSelectivity(type);
   
      if (DATUM_mp_PDF_EFFORT[0][i] == 1 || DATUM_mp_PDF_EFFORT[0][i] == 2)
         {
         STATISTICS_mp_AGE_EFFORT[0][NCPUE][0] = MinAge;
         STATISTICS_mp_AGE_EFFORT[0][NCPUE][1] = MaxAge;
  
         InputFLQuantYBG( index,  DATUM_mp_EFFORT_DATA_STORE, MinYear, MaxYear, i);
         InputFLQuantYBG( index,  DATUM_mp_EFFORT_DATA,       MinYear, MaxYear, i);
         InputFLQuantYBG( weights,DATUM_mp_SIGMA_EFFORT,      MinYear, MaxYear, i);
         InputFLQuantYABG(prop,   DATUM_mp_PSEL,              MinAge,  MaxAge, MinYear, MaxYear, i);

         for (short j=MinYear-STATISTICS_mp_DISPLAYYEAR+1; j<=MaxYear-STATISTICS_mp_DISPLAYYEAR+1; j++)
            {
            if ((PARAMETERS_mp_CV_OVERIDE>0.00001 && (short)(PARAMETERS_mp_CV_OVERIDE)!=999) || PARAMETERS_mp_CV_OVERIDE<-0.00001) 
                  DATUM_mp_SIGMA_EFFORT[j][0][i]=PARAMETERS_mp_CV_OVERIDE;
            
            for (short iAge = MinAge; iAge <= MaxAge; iAge++)
               {
               DATUM_mp_PSEL[j][iAge][0][i] = (MinAge == MaxAge ? 1.0 : DATUM_mp_PSEL[j][iAge][0][i]);

               if      (DATUM_mp_SEL_TYPE[0][i] == 1) // fixed selectivities
                  STATISTICS_mp_SEL_EFFORT[j][iAge][0][i] = DATUM_mp_PSEL[j][iAge][0][i];
               else if (DATUM_mp_SEL_TYPE[0][i] >= 3) // partial catches
                  DATUM_mp_PSEL[j][iAge][0][i] /= DATUM_mp_CATCH_DATA[j][iAge][0];
               }
            }

         NCPUE++;
         }

      if (DATUM_mp_PDF_EFFORT[0][i] != 0 && DATUM_mp_PDF_EFFORT[0][i] != 12 && PARAMETERS_mp_OPTION[3] <= 0) 
         DATUM_mp_N_QS += 1;
		}

   STATISTICS_mp_NGEARS[0] = NCPUE;

   VPA_SCALECPUE();

   return NCPUE;
   }

SEXP CreateFLAdapt(void)
   { 
   SEXP ReturnObject = R_NilValue;

   PROTECT(ReturnObject = NEW_OBJECT(MAKE_CLASS("FLAdapt")));

   //Output results to R 
   SET_SLOT(ReturnObject, install("stock.n"), CreateFLQuantYAB(STATISTICS_mp_N_AREA, STATISTICS_mp_FIRSTAGE, STATISTICS_mp_LASTAGE, STATISTICS_mp_FIRSTYEAR+STATISTICS_mp_DISPLAYYEAR-1, STATISTICS_mp_LASTYEAR+STATISTICS_mp_DISPLAYYEAR-1));
   SET_SLOT(ReturnObject, install("harvest"), CreateFLQuantYAB(STATISTICS_mp_F,      STATISTICS_mp_FIRSTAGE, STATISTICS_mp_LASTAGE, STATISTICS_mp_FIRSTYEAR+STATISTICS_mp_DISPLAYYEAR-1, STATISTICS_mp_LASTYEAR+STATISTICS_mp_DISPLAYYEAR-1));

   UNPROTECT(1);

   return ReturnObject;
   }

void ControlDefaults(void)
   {
   //Set defaults

   PARAMETERS_mp_NBOX             = 1;
   STATISTICS_mp_SEASON_SSB[0]    = 0;
   DATUM_mp_PDF_CATCH[0]          = 0;
   DATUM_mp_SIGMA_CATCH[0]        = 0.1;
   
   PARAMETERS_mp_MODEL_TYPE       = 1;    // diffusion =1, overlap = 2
   PARAMETERS_mp_OPTION[1-1]      = 0;    // diffusion = 1, overlap = 2
   PARAMETERS_mp_OPTION[3-1]      = 0;    // bootstraps
   PARAMETERS_mp_STINE_CORR       = 0;
   PARAMETERS_mp_PDF_TAG          = 0;    // Don't use tagging data
   PARAMETERS_mp_N_RETRO          = 0;    // retrospective analyses

   PARAMETERS_mp_SEED             = -911;
   PARAMETERS_mp_MAXITER          = 20;
   PARAMETERS_mp_CHECKFLAG        = 3;
   PARAMETERS_mp_PDEV             = 0.4;
   PARAMETERS_mp_OPTION[2-1]      = 1;    // OPTION TO USE (1) F'S OR (2) N'S AS TERMINAL YEAR PARAMETERS

   PARAMETERS_mp_OPTION[4-1]      = -1;   // estimate q via mle or as part of search (+) or by concentrated MLE's  (<0)

   PARAMETERS_mp_SCALES           = 0;    // scale indices by mean if > 0
   PARAMETERS_mp_CV_OVERIDE       = 0;    // over-ride index cv's to this value
   PARAMETERS_mp_ADD_VAR          = 0;    // (1) additive variance (0) multiplicative variance
   
   // VARIOUS PENALTIES TO CONSTRAIN THE SOLUTION
   // link selectivities in last n years
   PARAMETERS_mp_LINK_FT          = 0;    // apply this penalty to the last N years (SET N = 0 TO IGNORE)
   PARAMETERS_mp_SIGMA_FT         = 0;    // standard deviation controlling the severity of the penalty
   PARAMETERS_mp_LINK_YOUNGEST    = STATISTICS_mp_FIRSTAGE;    // first age affected
   PARAMETERS_mp_LINK_OLDEST      = STATISTICS_mp_LASTAGE;     // last age affected

   DATUM_mp_PDF_STOCKRECRUIT      = 0;    // Impose a Beverton and Holt stock recruitment penalty over certain years
   PARAMETERS_mp_SIGMA_STOCK      = 0.0;  // standard deviation controlling the severity of the penalty
   PARAMETERS_mp_RATIO_STOCK      = 1.0;  // ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}
   DATUM_mp_IGNORE_RECRUIT[0]     =    
   DATUM_mp_IGNORE_RECRUIT[1]     = 0; 


   // link recruitments in last n years
   PARAMETERS_mp_SIGMA_REC        =  0;   // apply this penalty to the last N years (SET N = 0 TO IGNORE)
   PARAMETERS_mp_SIGMA_STOCK      = .1;   // standard deviation controlling the severity of the penalty
   PARAMETERS_mp_RATIO_STOCK      =  1;   // ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}
   PARAMETERS_mp_LINK_REC         = 0;    
   PARAMETERS_mp_LINK_STOCK       = 0;    // link recruitments of two stocks
   }

bool InputControl(SEXP xControl)
   {
   ControlDefaults();
   
   // VARIOUS PENALTIES TO CONSTRAIN THE SOLUTION
   PARAMETERS_mp_SEED         =     INTEGER(GET_SLOT(xControl,install("seed"      )))[0];
   PARAMETERS_mp_MAXITER      =     INTEGER(GET_SLOT(xControl,install("maxit"     )))[0];                  
   PARAMETERS_mp_CHECKFLAG    =     LOGICAL(GET_SLOT(xControl,install("check"     )))[0];                 
   PARAMETERS_mp_PDEV         =        REAL(GET_SLOT(xControl,install("pdev"      )))[0];                 
   STATISTICS_mp_SEASON_SSB[0]= (int)(12.0*min(max(REAL(GET_SLOT(xControl,install("season" )))[0],0.0),1.0));              
   PARAMETERS_mp_OPTION[2-1]  =     INTEGER(GET_SLOT(xControl,install("option.f"     )))[0];                             
   
   if (LOGICAL(GET_SLOT(xControl,install("q.est")))[0])       PARAMETERS_mp_OPTION[4-1] = 1; else  PARAMETERS_mp_OPTION[4-1] = 0;
   if (LOGICAL(GET_SLOT(xControl,install("q.scale")))[0])     PARAMETERS_mp_SCALES      = 1; else  PARAMETERS_mp_SCALES      = 0;                
   if (LOGICAL(GET_SLOT(xControl,install("q.add")))[0])       PARAMETERS_mp_ADD_VAR     = 1; else  PARAMETERS_mp_ADD_VAR     = 0;                
   if (LOGICAL(GET_SLOT(xControl,install("sel.penalty")))[0]) PARAMETERS_mp_LINK_FT     = 1; else  PARAMETERS_mp_LINK_FT     = 0;                
   
   PARAMETERS_mp_CV_OVERIDE  =     REAL(GET_SLOT(xControl,install("q.cv"     )))[0];                             
   

   PARAMETERS_mp_SIGMA_FT     = max(   REAL(GET_SLOT(xControl,install("sel.sigma"    )))[0],0.0);              
   PARAMETERS_mp_SIGMA_FT     = max(   REAL(GET_SLOT(xControl,install("sel.minage"   )))[0],0.0);             
   PARAMETERS_mp_SIGMA_FT     = max(   REAL(GET_SLOT(xControl,install("sel.maxage"   )))[0],0.0);             
   //PARAMETERS_mp_LINK_STOCK   =     
   if (LOGICAL(GET_SLOT(xControl,install("sr.penalty"   )))[0])
      DATUM_mp_PDF_STOCKRECRUIT  = GetVPA2BoxPDF(GET_SLOT(xControl,install("sr.pdf"       )));             
   else
      DATUM_mp_PDF_STOCKRECRUIT  = 0;             

   PARAMETERS_mp_SIGMA_STOCK  = max(   REAL(GET_SLOT(xControl,install("sr.sigma"     )))[0],0.0);                

   DATUM_mp_IGNORE_RECRUIT[0] =     INTEGER(GET_SLOT(xControl,install("rec.minyr"    )))[0];                              
   DATUM_mp_IGNORE_RECRUIT[1] =     INTEGER(GET_SLOT(xControl,install("rec.maxyr"    )))[0];                             
   PARAMETERS_mp_LINK_REC     =     LOGICAL(GET_SLOT(xControl,install("rec.penalty"  )))[0];                          
   PARAMETERS_mp_SIGMA_REC    = max(   REAL(GET_SLOT(xControl,install("rec.sigma"    )))[0],0.0);

   return false;
   }

SEXP CreateControl(void)
   { 
   SEXP ReturnObject = R_NilValue;

   PROTECT(ReturnObject = NEW_OBJECT(MAKE_CLASS("FLAdapt.control")));

   SEXP
   	////Simplex options
	   seed         = R_NilValue,   //Random number seed
	   maxit        = R_NilValue,   //Maximum number of amoeba simplex search restarts
	   check        = R_NilValue,   //check flag, convergence is declared when CHECKFLAG number of consecutive restarts result in parameter estimates that vary by less than 1% 
	   pdev         = R_NilValue,   //standard deviation controlling the random specification of vertices for the initial simplex of each restart

	   ////General options
	   season		 = R_NilValue,   //Spawning season as fraction of a year
	   option_f     = R_NilValue,   //Option to use F's as terminal year parameters default is true, if false then use N's
	
	   ////Index options
	   q_est        = R_NilValue,   //Estimate q in search if true, default is false use concentrated MLE's
	   q_scale      = R_NilValue,   //Scale indices (i.e. divide index values by their mean, default is true
	   q_cv         = R_NilValue,   //Index weighting option default is "sd", alternative options are "input" or "mlm", so-called maximum likelihood method
	   q_add        = R_NilValue,   //Variance scaling factor default is false (i.e. multipicative)
	
	   ////Selectivity options
	   sel_penalty  = R_NilValue,   //Links selectivities in the last n years, default is false
	   sel_sigma    = R_NilValue,   //sigma (i.e. penalty)
	   sel_minage   = R_NilValue,   //first year
	   sel_maxage   = R_NilValue,   //last year
	
	   ////Stock recruit options
	   sr_penalty   = R_NilValue,   //Imposes stock recruitment relationship (penalises departures from Beverton and Holt model)   
	   sr_nyr       = R_NilValue,   //n years
	   sr_pdf       = R_NilValue,   //Error model
	
	   ////Recruitment options
	   rec_penalty  = R_NilValue,   //Links rectuitments in th elast n years
	   rec_sigma    = R_NilValue,   //sigma (i.e. penalty)
	   rec_minyr    = R_NilValue,   //sigma (i.e. penalty)
	   rec_maxyr    = R_NilValue,   //sigma (i.e. penalty)
	
	   ////Catch options
	   catch_penalty= R_NilValue,
      catch_sigma  = R_NilValue,  //sigma (i.e. penalty)
	   catch_pdf    = R_NilValue,  //PDF of catch
	
	   ////Parameter options as in the VPA2Box *.p file
	   param_termage= R_NilValue, //terminal ages
	   param_fratio = R_NilValue, //f ratio for oldest age or plusgroup
      param_srr    = R_NilValue, //stock recruit parameters
	   param_var    = R_NilValue, //variance scaling parameters
	   param_q      = R_NilValue; //catchability parameters
      
	seed         = PROTECT(NEW_INTEGER(1)),
	maxit        = PROTECT(NEW_INTEGER(1)), 
	check        = PROTECT(NEW_LOGICAL(1)),   
	pdev         = PROTECT(NEW_NUMERIC(1)),   
   season       = PROTECT(NEW_NUMERIC(1)),         
	option_f     = PROTECT(NEW_LOGICAL(1)),  
	q_est        = PROTECT(NEW_LOGICAL(1)),
	q_scale      = PROTECT(NEW_LOGICAL(1)),              
	q_cv         = PROTECT(NEW_NUMERIC(1)),         
	q_add        = PROTECT(NEW_LOGICAL(1)),     
	sel_penalty  = PROTECT(NEW_LOGICAL(1)),
	sel_sigma    = PROTECT(NEW_NUMERIC(1)),         
	sel_minage   = PROTECT(NEW_INTEGER(1)),
	sel_maxage   = PROTECT(NEW_INTEGER(1)),
	sr_penalty   = PROTECT(NEW_LOGICAL(1)),
   sr_nyr       = PROTECT(NEW_INTEGER(1)),
	sr_pdf       = PROTECT(NEW_CHARACTER(1)),
	rec_penalty  = PROTECT(NEW_LOGICAL(1)),
	rec_sigma    = PROTECT(NEW_NUMERIC(1)),         
	rec_minyr    = PROTECT(NEW_INTEGER(1)),
	rec_maxyr    = PROTECT(NEW_INTEGER(1)),
	catch_penalty= PROTECT(NEW_LOGICAL(1)),         
	catch_sigma  = PROTECT(NEW_NUMERIC(1)),         
	catch_pdf    = PROTECT(NEW_CHARACTER(1));
 
  
   INTEGER(seed       )[0] = PARAMETERS_mp_SEED;                                      
   INTEGER(maxit      )[0] = PARAMETERS_mp_MAXITER;                                   
   LOGICAL(check      )[0] = PARAMETERS_mp_CHECKFLAG;                                 
   REAL(   pdev       )[0] = PARAMETERS_mp_PDEV;                                
   REAL(   season     )[0] = STATISTICS_mp_SEASON_SSB[0]/12.0;
   LOGICAL(option_f   )[0] = PARAMETERS_mp_OPTION[2-1];                                           
   LOGICAL(q_est      )[0] = PARAMETERS_mp_OPTION[4-1]>0 ? true : false;
   LOGICAL(q_scale    )[0] = PARAMETERS_mp_SCALES;                                    
   REAL(   q_cv       )[0] = PARAMETERS_mp_CV_OVERIDE;                                
   LOGICAL(q_add      )[0] = PARAMETERS_mp_ADD_VAR;                                   
   LOGICAL(sel_penalty)[0] = PARAMETERS_mp_LINK_FT;                                   
   REAL(   sel_sigma  )[0] = PARAMETERS_mp_SIGMA_FT;                                  
   LOGICAL(sel_minage )[0] = PARAMETERS_mp_LINK_YOUNGEST;                             
   REAL(   sel_maxage )[0] = PARAMETERS_mp_LINK_OLDEST;                               
   LOGICAL(sr_penalty )[0] = PARAMETERS_mp_LINK_FT;                                   
   LOGICAL(rec_penalty)[0] = PARAMETERS_mp_LINK_REC;                                               
   INTEGER(rec_minyr  )[0] = DATUM_mp_IGNORE_RECRUIT[0];                                              
   INTEGER(rec_maxyr  )[0] = DATUM_mp_IGNORE_RECRUIT[1];                                              
   REAL(   rec_sigma  )[0] = PARAMETERS_mp_SIGMA_REC;                                              
   REAL(   catch_sigma)[0] = DATUM_mp_SIGMA_CATCH[0]; 

   //0=none, 1=lognormal, 2=normal 
   //negative PDF tells program to estimate sigma by concentrated likelihood
   if (DATUM_mp_PDF_STOCKRECRUIT < 0)
      DATUM_mp_PDF_STOCKRECRUIT *= -1;

   SET_STRING_ELT(sr_pdf, 0, COPY_TO_USER_STRING("lognormal"));
   if (DATUM_mp_PDF_STOCKRECRUIT == 0) {
      SET_STRING_ELT(sr_pdf, 0, COPY_TO_USER_STRING("lognormal"));
      LOGICAL(sr_penalty)[0] = 0;                                  
      }
   else if (DATUM_mp_PDF_STOCKRECRUIT == 1)
      SET_STRING_ELT(sr_pdf, 0, COPY_TO_USER_STRING("lognormal"));
   else if (DATUM_mp_PDF_STOCKRECRUIT == 2)
      SET_STRING_ELT(sr_pdf, 0, COPY_TO_USER_STRING("normal"));
   
   //0=none, 1=lognormal, 2=normal 
   SET_STRING_ELT(catch_pdf, 0, COPY_TO_USER_STRING("lognormal"));
   if (DATUM_mp_PDF_CATCH[0] == 0){
      LOGICAL(catch_penalty )[0] = 0; 
      SET_STRING_ELT(catch_pdf, 0, COPY_TO_USER_STRING("normal"));
      }                                  
   else if (DATUM_mp_PDF_CATCH[0] == 1)
      SET_STRING_ELT(catch_pdf, 0, COPY_TO_USER_STRING("lognormal"));
   else if (DATUM_mp_PDF_CATCH[0] == 2)
      SET_STRING_ELT(catch_pdf, 0, COPY_TO_USER_STRING("normal"));
   
   
   SET_SLOT(ReturnObject,install("seed"),         seed);                            
   SET_SLOT(ReturnObject,install("maxit"),        maxit);                            
   SET_SLOT(ReturnObject,install("check"),        check);                            
   SET_SLOT(ReturnObject,install("pdev"),         pdev);                            
   SET_SLOT(ReturnObject,install("season"),       season);                                     
   SET_SLOT(ReturnObject,install("option.f"),     option_f);                                     
   SET_SLOT(ReturnObject,install("q.est"),        q_est);                            
   SET_SLOT(ReturnObject,install("q.scale"),      q_scale);                         
   SET_SLOT(ReturnObject,install("q.cv"),         q_cv);                             
   SET_SLOT(ReturnObject,install("q.add"),        q_add);                            
   SET_SLOT(ReturnObject,install("sel.penalty"),  sel_penalty);                         
   SET_SLOT(ReturnObject,install("sel.sigma"),    sel_sigma);                        
   SET_SLOT(ReturnObject,install("sel.minage"),   sel_minage);                        
   SET_SLOT(ReturnObject,install("sel.maxage"),   sel_maxage);                        
   SET_SLOT(ReturnObject,install("sr.penalty"),   sr_penalty);                         
   SET_SLOT(ReturnObject,install("sr.pdf"),       sr_pdf);                           
   SET_SLOT(ReturnObject,install("sr.nyr"),       sr_nyr);                                         
   SET_SLOT(ReturnObject,install("rec.penalty"),  rec_penalty);                                     
   SET_SLOT(ReturnObject,install("rec.sigma"),    rec_sigma);                       
   SET_SLOT(ReturnObject,install("rec.minyr"),    rec_minyr);                                        
   SET_SLOT(ReturnObject,install("rec.maxyr"),    rec_maxyr);  
   SET_SLOT(ReturnObject,install("catch.penalty"),catch_penalty);                         
   SET_SLOT(ReturnObject,install("catch.pdf"),    catch_pdf);                           
   SET_SLOT(ReturnObject,install("catch.sigma"),  catch_sigma);                         
   
   double P[200][11];

   int n = VPA_WRITE_PARAMETERS(P)-1;
   
   SET_SLOT(ReturnObject,install("param.termage"),CreateArray(P, 1));
   SET_SLOT(ReturnObject,install("param.fratio"), CreateArray(P, 2));
   SET_SLOT(ReturnObject,install("param.srr"),    CreateArray(P, 5));
   SET_SLOT(ReturnObject,install("param.var"),    CreateArray(P, 6));
   SET_SLOT(ReturnObject,install("param.q"),      CreateArray(P, 7));
   
   UNPROTECT(24);
 
   return ReturnObject;
   }

bool InputFLQuantAB(SEXP FLQuant, double D[As+1][Bs], short Min, short Max, short Stock)
    {
    if ((Max-Min+1)>DIMS_NYrs || Stock != 0)
        return false;

    int _Min, _Max;

    SEXP Quant    = PROTECT(duplicate(GET_SLOT(FLQuant, install(".Data")))),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    double *Q     = NUMERIC_POINTER(Quant);

    int dim[5], n = length(dims);

    if (n != 5)
       return false;
 
    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];

    if (((int)dim[0]) <  1 || ((int)dim[1]) != 1 || 
        ((int)dim[2]) != 1 || ((int)dim[3]) != 1 || ((int)dim[4]) != 1)
      {
      UNPROTECT(1);

      return FALSE;
      }

   _Min  = 1;
   _Max  = (short)dim[1];
 	      
   if (dimnames != R_NilValue) 
      if (TYPEOF(dimnames) == VECSXP) 
         {
         int t = 0;

         if (INTEGER(dims)[0] > 1) 
            t = atoi(CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 0), 0))) - 1; 
      
         _Min += t;
         _Max += t;
         }
   
   if (_Min > Min || _Max < Max)
      {
      UNPROTECT(1);

      return FALSE;
      }

   int i,    j;

   for (i = _Min, j = 0; i <= _Max; i++, j++)
      D[j][Stock] = (Q)[j];       

   UNPROTECT(1);

   return TRUE;
   }

bool InputFLQuantYAB(SEXP FLQuant, double D[Ys+1][As+1][Bs], int MinAge, int MaxAge, int MinYear, int MaxYear, short Stock)
    {
    if ((MaxAge-MinAge+1)>As || (MaxYear-MinYear+1)>Ys || Stock != 0)
        return false;

    SEXP Quant    = PROTECT(duplicate(GET_SLOT(FLQuant, install(".Data")))),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    double *Q     = NUMERIC_POINTER(Quant);

    int dim[5], n = length(dims);
 
    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];

    if (((int)dim[0]) <  1 || ((int)dim[1]) <  1 || 
        ((int)dim[2]) != 1 || ((int)dim[3]) != 1 || ((int)dim[4]) != 1)
      {
      UNPROTECT(1);

      return FALSE;
      }

    short _MinAge  = 1,
          _MinYear = 1,
          _MaxAge  = (short)dim[0],
          _MaxYear = (short)dim[1];
 	      
    if (dimnames != R_NilValue) 
      if (TYPEOF(dimnames) == VECSXP) 
         {
         int t;
      
         if (n >= 1 && INTEGER(dims)[0] >= 1) 
               {
               t = atoi(CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 0), 0))) - 1; 
               _MinAge += t;
               _MaxAge += t;
               }
	      if (n >= 2 && INTEGER(dims)[1] >= 1) 
               {
               t = atoi(CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 1), 0))) - 1; 
               _MinYear += t;
               _MaxYear += t;
 	            }
	      }

    if (MinAge  < _MinAge  || MaxAge  > _MaxAge || 
        MinYear < _MinYear || MaxYear > _MaxYear )
      {
      UNPROTECT(1);

      return FALSE;
      }

    int i,    j,    k=0;

    for (j=MinYear-STATISTICS_mp_DISPLAYYEAR+1; j<=MaxYear-STATISTICS_mp_DISPLAYYEAR+1; j++)
        for (i = MinAge; i <= MaxAge; i++)
            D[j][i][Stock] = (Q)[k++];       
   
    UNPROTECT(1);

    return TRUE;
    }

bool InputFLQuantYABG(SEXP FLQuant, double D[Ys+1][As+1][Bs][Gs], int MinAge, int MaxAge, int MinYear, int MaxYear, short Fleet, short Stock)
    {
    if ((MaxAge-MinAge+1)>As || (MaxYear-MinYear+1)>Ys || Stock != 0)
        return false;

    SEXP Quant    = PROTECT(duplicate(GET_SLOT(FLQuant, install(".Data")))),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    double *Q     = NUMERIC_POINTER(Quant);

    int dim[5], n = length(dims);
 
    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];

    if (((int)dim[0]) <  1 || ((int)dim[1]) <  1 || 
        ((int)dim[2]) != 1 || ((int)dim[3]) != 1 || ((int)dim[4]) != 1)
      {
      UNPROTECT(1);

      return FALSE;
      }

    short _MinAge  = 1,
          _MinYear = 1,
          _MaxAge  = (short)dim[0],
          _MaxYear = (short)dim[1];
 	      
    if (dimnames != R_NilValue) 
      if (TYPEOF(dimnames) == VECSXP) 
         {
         int t;
      
         if (n >= 1 && INTEGER(dims)[0] >= 1) 
               {
               t = atoi(CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 0), 0))) - 1; 
               _MinAge += t;
               _MaxAge += t;
               }
	      if (n >= 2 && INTEGER(dims)[1] >= 1) 
               {
               t = atoi(CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 1), 0))) - 1; 
               _MinYear += t;
               _MaxYear += t;
 	            }
	      }

    if (MinAge  < _MinAge  || MaxAge  > _MaxAge || 
        MinYear < _MinYear || MaxYear > _MaxYear )
      {
      UNPROTECT(1);

      return FALSE;
      }

     int i=0, j=0, k=0;

     for (j=MinYear-STATISTICS_mp_DISPLAYYEAR+1; j<=MaxYear-STATISTICS_mp_DISPLAYYEAR+1; j++)
        for (i = MinAge; i <= MaxAge; i++)
            D[j][i][Stock][Fleet] = (Q)[k++];       
   
    UNPROTECT(1);

    return TRUE;
    }

bool InputFLQuantYBG(SEXP FLQuant, double D[Ys+1][Bs][Gs], int MinYear, int MaxYear, short Fleet, short Stock)
    {
    if ((MaxYear-MinYear+1)>Ys || Stock != 0)
        return false;

    SEXP Quant    = PROTECT(duplicate(GET_SLOT(FLQuant, install(".Data")))),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    double *Q     = NUMERIC_POINTER(Quant);

    int dim[5], n = length(dims);
 
    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];

    if (((int)dim[0]) <  1 || ((int)dim[1]) <  1 || 
        ((int)dim[2]) != 1 || ((int)dim[3]) != 1 || ((int)dim[4]) != 1)
      {
      UNPROTECT(1);

      return FALSE;
      }

    short _MinYear = 1,
          _MaxYear = (short)dim[1];
 	      
    if (dimnames != R_NilValue) 
      if (TYPEOF(dimnames) == VECSXP) 
         {
         int t;
      
         if (n >= 2 && INTEGER(dims)[1] >= 1) 
               {
               t = atoi(CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 1), 0))) - 1; 
               _MinYear += t;
               _MaxYear += t;
 	            }
	      }

    if (MinYear < _MinYear || MaxYear > _MaxYear )
      {
      UNPROTECT(1);

      return FALSE;
      }

    int i, j;

    for (j=MinYear-STATISTICS_mp_DISPLAYYEAR+1, i=0; j<=MaxYear-STATISTICS_mp_DISPLAYYEAR+1; i++, j++)
        D[j][Stock][Fleet] = (Q)[i];       
   
    UNPROTECT(1);

    return TRUE;
    }

SEXP CreateFLQuantAYB(double D[Ys+1][As+1][Bs], int MinAge, int MaxAge, int MinYear, int MaxYear, short Stock)
    {
    int i, j, iAge, iYear;
 
    SEXP FLQuant, v, 
         d1, d2, d3, d4, d5,  
         dim,   dimnames, names;    

    if ((MaxAge-MinAge+1)>DIMS_NAges || (MaxYear-MinYear+1)>DIMS_NYrs)
       return false;

    //Create new S4 object    
    //PROTECT(v = NEW_OBJECT(MAKE_CLASS("array")));
    PROTECT(FLQuant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 5));       
    INTEGER(dim)[0] = MaxAge -MinAge +1;
    INTEGER(dim)[1] = MaxYear-MinYear+1;
    INTEGER(dim)[2] = 1; 
    INTEGER(dim)[3] = 1; 
    INTEGER(dim)[4] = 1; 
     
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
 
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 5));
 
    PROTECT(d1 = allocVector(INTSXP, MaxAge-MinAge +1));
    for (iAge=MinAge, i=0; iAge<=MaxAge; iAge++, i++)
        INTEGER(d1)[i] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
 
    PROTECT(d2 = allocVector(INTSXP, MaxYear-MinYear+1));
    for (iYear=MinYear, i=0; iYear<=MaxYear; iYear++, i++)
        INTEGER(d2)[i] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
  
    PROTECT(d3 = allocVector(STRSXP, 1));
    SET_STRING_ELT(d3, 0, mkChar("unique"));
    SET_VECTOR_ELT(dimnames, 2, d3);
 
    PROTECT(d4 = allocVector(STRSXP, 1));
    SET_STRING_ELT(d4, 0, mkChar("all"));
    SET_VECTOR_ELT(dimnames, 3, d4);
 
    PROTECT(d5 = allocVector(STRSXP, 1));
    SET_STRING_ELT(d5, 0, mkChar("unique"));
    SET_VECTOR_ELT(dimnames, 4, d5);
 
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, 5));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
 
    //Set data
    for (iAge=MinAge, i=0; iAge<=MaxAge; iAge++, i++)
       for (iYear=MinYear, j=0; iYear<=MaxYear; iYear++, j++)
          REAL(v)[i + j*(MaxAge-MinAge+1)] = D[j][i][Stock];

    //Set slot
    FLQuant = R_do_slot_assign(FLQuant, install(".Data"), v);

    UNPROTECT(10);
 
    return FLQuant;
    }

SEXP CreateFLQuantYAB(double D[Ys+1][As+1][Bs], int MinAge, int MaxAge, int MinYear, int MaxYear, short Stock)
    {
    int i, j, iAge, iYear;
 
    SEXP FLQuant, v, 
         d1, d2, d3, d4, d5,  
         dim,   dimnames, names;    

    if ((MaxAge-MinAge+1)>DIMS_NAges || (MaxYear-MinYear+1)>DIMS_NYrs)
       return false;

    //Create new S4 object    
    //PROTECT(v = NEW_OBJECT(MAKE_CLASS("array")));
    PROTECT(FLQuant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 5));       
    INTEGER(dim)[0] = MaxAge -MinAge +1;
    INTEGER(dim)[1] = MaxYear-MinYear+1;
    INTEGER(dim)[2] = 1; 
    INTEGER(dim)[3] = 1; 
    INTEGER(dim)[4] = 1; 
     
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
 
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 5));
 
    PROTECT(d1 = allocVector(INTSXP, MaxAge-MinAge +1));
    for (iAge=MinAge, i=0; iAge<=MaxAge; iAge++, i++)
        INTEGER(d1)[i] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
 
    PROTECT(d2 = allocVector(INTSXP, MaxYear-MinYear+1));
    for (iYear=MinYear, i=0; iYear<=MaxYear; iYear++, i++)
        INTEGER(d2)[i] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
  
    PROTECT(d3 = allocVector(STRSXP, 1));
    SET_STRING_ELT(d3, 0, mkChar("unique"));
    SET_VECTOR_ELT(dimnames, 2, d3);
 
    PROTECT(d4 = allocVector(STRSXP, 1));
    SET_STRING_ELT(d4, 0, mkChar("all"));
    SET_VECTOR_ELT(dimnames, 3, d4);
 
    PROTECT(d5 = allocVector(STRSXP, 1));
    SET_STRING_ELT(d5, 0, mkChar("unique"));
    SET_VECTOR_ELT(dimnames, 4, d5);
 
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, 5));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
 
    //Set data
    for (iAge=MinAge, i=0; iAge<=MaxAge; iAge++, i++)
       for (iYear=MinYear, j=0; iYear<=MaxYear; iYear++, j++)
          REAL(v)[i + j*(MaxAge-MinAge+1)] = D[j+1][iAge][Stock];

    //Set slot
    FLQuant = R_do_slot_assign(FLQuant, install(".Data"), v);

    UNPROTECT(10);
 
    return FLQuant;
    }

SEXP CreateArray(double D[200][11], int index)
    {
    int i, j, k, n=0;
 
    SEXP v = R_NilValue, 
         dim;    

    for (i=0; i<200; i++)
      if (D[i][10] == index) n++;

    if (n<1) return v;

    //Create new S4 object    
    PROTECT(v = NEW_OBJECT(MAKE_CLASS("array")));
    
    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 2));       
    INTEGER(dim)[0] = n;
    INTEGER(dim)[1] = 6;
     
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
 
    //Set data
    k=0;
    for (j=0; j<200; j++)
      if (D[j][10] == index) {
         for (i=0; i<6; i++)
            REAL(v)[k + i*n] = D[j][i];
            k++;
            }
      
    UNPROTECT(1);
 
    return v;
    }


/*
short GetCPUEData(LPXLOPER *ppx, LPXLOPER pCPUESmry)
   {
   LPXLOPER FAR *ppxArg;   // Pointer to the argument being processed

   short NCPUE=0, _NCPUE=0;

   double CPUEOptions[Gs][4];
   
   _NCPUE=GetCPUESmry(pCPUESmry, &CPUEOptions);

   for (int iArg = 0; iArg < _NCPUE; iArg++)
      {
      STATISTICS_mp_SEASON_EFFORT[0][iArg] = (int)CPUEOptions[iArg][0];
      DATUM_mp_PDF_EFFORT[        0][iArg] = (int)CPUEOptions[iArg][1]; 
      DATUM_mp_BIO_EFFORT[        0][iArg] = (int)CPUEOptions[iArg][2]; 
      DATUM_mp_SEL_TYPE[          0][iArg] = (int)CPUEOptions[iArg][3];
       

      if (DATUM_mp_PDF_EFFORT[0][iArg] == 1 || DATUM_mp_PDF_EFFORT[0][iArg] == 2)
         {
         ppxArg = ppx PROCESSARGS iArg;
         GetCPUE(ppxArg, &NCPUE);
         }

      if (DATUM_mp_PDF_EFFORT[0][iArg] != 0 && DATUM_mp_PDF_EFFORT[0][iArg] != 12 && PARAMETERS_mp_OPTION[3] <= 0) 
         DATUM_mp_N_QS += 1;
		}
   
   STATISTICS_mp_NGEARS[0] = NCPUE;

   VPA_SCALECPUE();

   return NCPUE;
   }

short GetCPUESmry(LPXLOPER pCPUESmry, double (*pCPUEOptions)[Gs][4])
   {
   short N;

   //process argument
   switch (pCPUESmry->xltype)
      {
      case xltypeRef:
      case xltypeSRef: 
      case xltypeMulti:
      case xltypeMulti | xlbitDLLFree:

         static XLOPER xMulti;    // Argument coerced to xltypeMulti 
    
         if (xlretUncalced == Excel(xlCoerce, &xMulti, 2,(LPXLOPER) pCPUESmry, TempInt(xltypeMulti))) 
            N = 0;
         else
            N = xMulti.val.array.columns;
         break; 
      
      default:
         N = 0;
      break;    
      }
        
   for (short i=0; i<N; i++)
      {
      //Timing
      (*pCPUEOptions)[i][0] = GetVPA2BoxTiming(     pCPUESmry, i+1, 1);
      //PDF
      (*pCPUEOptions)[i][1] = GetVPA2BoxPDF(        pCPUESmry, i+1, 2);
      //Units
      (*pCPUEOptions)[i][2] = GetVPA2BoxUnits(      pCPUESmry, i+1, 3);
      //Sel
      (*pCPUEOptions)[i][3] = GetVPA2BoxSelectivity(pCPUESmry, i+1, 4);
      }

     
   return N;
   }


bool GetCPUE(LPXLOPER *ppx, short *pNCPUE)
   {
   short MinAge, MaxAge, MinYear, MaxYear, flag=0;
   LP2DOUBLE Matrix;
   LPDOUBLE  Index, IndexSE;

   //Aged rows are catches or selection pattern
   //Final rows are index and then wts
   if (InputXlAY_Y(*ppx,&Matrix,&Index,&MinAge,&MaxAge,&MinYear,&MaxYear))
      flag = 1;
   else if (InputXlAY_Y_Y(*ppx,&Matrix,&Index,&IndexSE,&MinAge,&MaxAge,&MinYear,&MaxYear))
      flag = 2;
   else
      return false;

   STATISTICS_mp_AGE_EFFORT[0][*pNCPUE][0] = MinAge;
   STATISTICS_mp_AGE_EFFORT[0][*pNCPUE][1] = MaxAge;
  
   for (short iYear = MinYear, j=MinYear-STATISTICS_mp_DISPLAYYEAR+1; iYear <= MaxYear && j<=STATISTICS_mp_NYEARS; iYear++, j++)
      {
      DATUM_mp_EFFORT_DATA_STORE[ j][0][*pNCPUE] = 
      DATUM_mp_EFFORT_DATA[       j][0][*pNCPUE] = Index[iYear];
      DATUM_mp_SIGMA_EFFORT[      j][0][*pNCPUE] = (flag == 1 ? 1.0 : IndexSE[iYear]);

      for (short iAge = MinAge; iAge <= MaxAge; iAge++)
          {
          DATUM_mp_PSEL[j][iAge][0][*pNCPUE] = (MinAge == MaxAge ? 1.0 : Matrix[iAge][iYear]);

          if      (DATUM_mp_SEL_TYPE[0][*pNCPUE] == 1) // fixed selectivities
             STATISTICS_mp_SEL_EFFORT[j][iAge][0][*pNCPUE] = DATUM_mp_PSEL[j][iAge][0][*pNCPUE];
          else if (DATUM_mp_SEL_TYPE[0][*pNCPUE] >= 3) // partial catches
             DATUM_mp_PSEL[j][iAge][0][*pNCPUE] /= DATUM_mp_CATCH_DATA[j][iAge][0];
          }
      }
   (*pNCPUE)++;
 
   free_flallocArray(Matrix,MinAge,MaxAge,MinYear,MaxYear);
   free_flallocArray(Index,               MinYear,MaxYear);
   if (flag==2)
      free_flallocArray(IndexSE,          MinYear,MaxYear);

   return true;
   }
*/


int NElemList(SEXP x)
   {
   //Check that it is a list
   if (!IS_LIST(x) || TYPEOF(x) != VECSXP) 
      return 0;
   else
      return length(x);
  }

void InputRange(SEXP obj, short *MinAge, short *MaxAge, short *PlusGroup, short *MinYear, short *MaxYear, double *Start, double *End)
   {
   SEXP names = GET_NAMES(obj);
	  
   double *a     = NUMERIC_POINTER(obj);

   int n = length(obj);

   for (int i=0; i<n; i++)
      {
      char *s = CHAR(VECTOR_ELT(names, i));

      if (      strcmp(s, "min")==0 || strcmp(s, "minage")==0)
         *MinAge     = (short)((a)[i]);
      else  if (strcmp(s, "max")==0 || strcmp(s, "maxage")==0)
         *MaxAge     = (short)((a)[i]);
      else  if (strcmp(s, "plusgroup")==0)
         *PlusGroup  = (short)((a)[i]);
      else  if (strcmp(s, "minyear")==0)
         *MinYear    = (short)((a)[i]);
      else  if (strcmp(s, "maxyear")==0)
         *MaxYear    = (short)((a)[i]);
      else  if (strcmp(s, "startf")==0)
         *Start      =        ((a)[i]);
      else  if (strcmp(s, "endf")==0)
         *End        =        ((a)[i]);
      }

  UNPROTECT(1);
  }

short GetVPA2BoxPDF(SEXP x)
   {
   //0=none, 1=lognormal, 2=normal 
   char *s = _strlwr(CHAR(VECTOR_ELT(x, 0)));

   if (strcmp(s, "none")==0)
      return 0;
   else  if (strcmp(s, "l")==1)
      return 1;
   else  if (strcmp(s, "n")==1)
      return 2;
   else
      return 0;   
   }

short GetVPA2BoxUnits(SEXP x)
   {
   //0=none, 1=lognormal, 2=normal 
   int n = length(x);
   
   SEXP names = GET_NAMES(x);
	
   for (int i=1; i<=n; i++)
      {
      char *s = CHAR(VECTOR_ELT(names, i-1));
      char *t = CHAR(VECTOR_ELT(x,     i-1));

      if (strcmp(s, "units")==0)
         {
         char *s2 = _strlwr(CHAR(VECTOR_ELT(x, i)));

         if (strcmp(t, "n")==1)
            return 1;
         else  if (strcmp(t, "b")==1)
            return 2;
         else 
            return 1;   
         }
      }
   
   return 1;   
   }

short GetVPA2BoxSelectivity(SEXP x)
   {
   //selection, fractional catches, partial catches, ?
   int n = length(x);
   
   SEXP names = GET_NAMES(x);
	
   for (int i=1; i<=n; i++)
      {
      char *s = CHAR(VECTOR_ELT(names, i-1));

      if (strcmp(s, "method")==0)
         {
         char *t = _strlwr(CHAR(STRING_ELT(x,     i-1)));
   
         if       (strcmp(t, "s")==1)
            return 1;
         else  if (strcmp(t, "f")==1)
            return 2;
         else  if (strcmp(t, "p")==1)
            return 3;
         else  if (strcmp(t, "b")==1)
            return 4;
         else
            return 1;   
         }
      }
   
   return 1;   
   }
