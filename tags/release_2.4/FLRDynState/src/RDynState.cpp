#include <Rdefines.h>
#include <Rinternals.h>
#include <R.h>

#include <vector> 
#include <math.h>
#include <stdlib.h>
#include <time.h>   
#include "matalloc.h"
#include <omp.h>

using namespace std; 

int             outofbounds_int;
bool            outofbounds_bool;
double          outofbounds_double;

#define PLAICECAPACITY        360
#define SOLECAPACITY          360
#define EFFORTCAPACITY        110
#define NOSPEC                  2
#define MAXNOINC               30
#define MAXHORIZON             12

typedef short SMALLINT;

typedef float    (*FTYPE)[EFFORTCAPACITY][SOLECAPACITY];
typedef SMALLINT (*ITYPE)[SOLECAPACITY][EFFORTCAPACITY];
typedef float    (*PTYPE)[MAXHORIZON][NOSPEC][MAXNOINC];

float ranf ()
{
  return ((float)rand()/RAND_MAX);
}

float utility (int aPlaice, int aPlaiceUplimit, float aPricePlaice, int aSole, int aSoleUplimit, float aPriceSole, int anEffort, int anEffortUplimit, float aPriceEffort)
{
  if ((aPlaice >= aPlaiceUplimit) || (aSole >= aSoleUplimit) || (anEffort >= anEffortUplimit))   return (-2E28);
  else if ((aPlaice < aPlaiceUplimit) &&  (aSole < aSoleUplimit) && (anEffort < anEffortUplimit)) return ((aPlaice * aPricePlaice) + (aSole * aPriceSole)  - (anEffort * aPriceEffort));
  return -1.0;
}

float FF (int aPlaice, int aSole, int anEffort, double aPriceEffort, int aTime, int aNoInc, PTYPE aParms, int aPlaiceUplimit, int aSoleUplimit, int anEffortUplimit, int aNPatch, FTYPE anF1, ITYPE aIStar, int anEffortArray[])
{
  double vMax = -2E120;
  aIStar[aPlaice][aSole][anEffort] = -1;
  int thePlaiceVal, theSoleVal;

  if ((aPlaice > aPlaiceUplimit) || (aSole > aSoleUplimit) || (anEffort > anEffortUplimit) ) return (-anEffort * aPriceEffort);
    
  for (int i = 0; i < aNPatch; i++){
    double tsom = 0;
    int theNewEffort = anEffort + anEffortArray[i];
    int inc0, inc1;
    float x0;
    for (thePlaiceVal = aPlaice, inc0 = 0; thePlaiceVal < aPlaice + aNoInc; thePlaiceVal++, inc0++) {
      x0 = aParms[i][aTime][0][inc0];
      for (theSoleVal = aSole, inc1 = 0; theSoleVal < aSole + aNoInc; theSoleVal++, inc1++)
 	 tsom += x0 * aParms[i][aTime][1][inc1] * anF1[thePlaiceVal][theNewEffort][theSoleVal];
    }
    if (tsom > vMax){
      vMax = tsom;
      aIStar[aPlaice][aSole][anEffort] = i;
    }
  }
  return vMax;
}

SEXP Simulate ( int aSimNumber, int aHorizon, vector< int > maxplaice, vector < int > maxsole, vector< int > maxeff, vector<vector<SMALLINT > > val,vector<vector< unsigned short > > rep, ITYPE aIstar, int aPlaiceUplimit, int aSoleUplimit, int aNoInc, double aSizeSoleInc, double aSizePlaiceInc, PTYPE aParms, int anEffortArray[]){
  SEXP ReturnObject, SimDims, Choice, Spp1Rand, Spp2Rand, Spp1Landings, Spp2Landings, Spp1Hold, Spp2Hold, anEffort, Exceed;
  
  PROTECT(ReturnObject = NEW_OBJECT(MAKE_CLASS("Sim")));
  
  PROTECT(SimDims      = allocVector(INTSXP,2));
  INTEGER(SimDims)[0]  = aSimNumber; 
  INTEGER(SimDims)[1]  = aHorizon; 
  PROTECT(Choice       = allocArray(INTSXP,SimDims));
  PROTECT(Spp1Rand     = allocArray(REALSXP,SimDims));
  PROTECT(Spp2Rand     = allocArray(REALSXP,SimDims));
  PROTECT(Spp1Landings = allocArray(REALSXP,SimDims));
  PROTECT(Spp2Landings = allocArray(REALSXP,SimDims));
  PROTECT(Spp1Hold     = allocArray(REALSXP,SimDims));
  PROTECT(Spp2Hold     = allocArray(REALSXP,SimDims));
  PROTECT(anEffort     = allocArray(INTSXP,SimDims));
  PROTECT(Exceed       = allocArray(INTSXP,SimDims));
    
  for (int s= 0; s < aSimNumber; s++)	 { 
       
      float Q;
      int aPlaiceHold = 0;
      int aSoleHold = 0;
      int Effort = 0;

      for (int t = 0; t < aHorizon; t++) { 
	     	if ((aPlaiceHold < aPlaiceUplimit) && (aSoleHold < aSoleUplimit)) { /* if not exceeding quota  */
	       	/* get best patch from the decision array */
	      	INTEGER(Exceed)[s+ t*aSimNumber] = 0;/* determine what patch to go to */
          /********************************************************************************/ 
          /* RLE DECODING TO GET CHOICE                                                   */
	        /********************************************************************************/ 
	        int choicepos= aSoleHold + Effort * (maxsole[t]) + aPlaiceHold * (maxsole[t]) * (maxeff[t]);
          int ii=0;
          int nn=0;
          do { ii = ii + rep[t][nn];
	          nn++;
          } while ( ii <= choicepos );
          int choice =  val[t][nn-1];

          INTEGER(Choice)[s+ t*aSimNumber]  = choice +1; /* + 1 because choice in c++ starts at 0 */
          /********************************************************************************/ 
          /* GET CONSEQUENCES OF CHOICE                                                   */
 	        /********************************************************************************/ 
 	      	if (choice == -1) {                  /* if backcalc are unable to determine the diff between -1 and rest then choose patch 1 and make homebrew 1 */
		  		    INTEGER(Choice)[s + t*aSimNumber] = 1;
		  		    INTEGER(Exceed)[s + t*aSimNumber] = 1;
			    }
            
          INTEGER(anEffort)[s + t*aSimNumber]= anEffortArray[choice];
	        Effort = Effort + INTEGER(anEffort)[s + t*aSimNumber]; /* calulate fueluse */

	      	/* calculate plaice catch in the patch */
	      	Q = ranf ();	/* random number 0-1 */
	      	REAL(Spp1Rand)[s + t*aSimNumber] = Q;
          float probl = 0;
	      	float probup = 0;

		    	probup = aParms[choice][t][0][0];
		    	if (Q <= probup){
              REAL(Spp1Landings)[s + t*aSimNumber] = 0;
          }
		      probl = probup;
        	for (int stickPlaice = 1; stickPlaice < aNoInc; stickPlaice++) {
		      		probup = aParms[choice][t][0][stickPlaice] + probl;
		      		if ((Q > probl) && (Q <= probup)){
                  aPlaiceHold = aPlaiceHold + stickPlaice;
     
                  REAL(Spp1Landings)[s+ t*aSimNumber] = stickPlaice * aSizePlaiceInc;
              }
		      		probl = probup;
		    	}

	      	/* calculate sole catch in the patch */
	      	Q = ranf ();	/* random number 0-1 */
	      	REAL(Spp2Rand)[s + t*aSimNumber] = Q;
          probl  = 0;
          probup = aParms[choice][t][1][0];
		     	if (Q <= probup){
              REAL(Spp2Landings)[s+ t*aSimNumber] = 0;
          }
          probl = probup;

        	for (int stickSole = 1; stickSole < aNoInc; stickSole++) {
		      		probup = aParms[choice][t][1][stickSole] + probl;
		      		if ((Q > probl) && (Q <= probup)){
                  aSoleHold = aSoleHold + stickSole;               
                  REAL(Spp2Landings)[s+ t*aSimNumber] = stickSole * aSizeSoleInc;
              }
					    probl = probup;
		    	}
	    }
	  	else if ((aPlaiceHold >= aPlaiceUplimit)|| (aSoleHold >= aSoleUplimit))  {
        INTEGER(Choice)[s+ t*aSimNumber] = -1;
        REAL(Spp1Landings)[s+ t*aSimNumber] = 0;
        REAL(Spp2Landings)[s+ t*aSimNumber]   = 0;
        INTEGER(anEffort)[s+ t*aSimNumber]     = 0;
        INTEGER(Exceed)[s+ t*aSimNumber]      = 1;
	    }
    REAL(Spp1Hold)[s+ t*aSimNumber] = aPlaiceHold * aSizePlaiceInc;
    REAL(Spp2Hold)[s+ t*aSimNumber] = aSoleHold * aSizeSoleInc;
	  }
  }
  SET_SLOT(ReturnObject, install("Choice"),  Choice);
  SET_SLOT(ReturnObject, install("Spp1Rand"),  Spp1Rand);
  SET_SLOT(ReturnObject, install("Spp2Rand"),  Spp2Rand);
  SET_SLOT(ReturnObject, install("Spp1Landings"),  Spp1Landings);
  SET_SLOT(ReturnObject, install("Spp2Landings"),  Spp2Landings);
  SET_SLOT(ReturnObject, install("Spp1Hold"),  Spp1Hold);
  SET_SLOT(ReturnObject, install("Spp2Hold"),  Spp2Hold);
  SET_SLOT(ReturnObject, install("Effort"),  anEffort);
  SET_SLOT(ReturnObject, install("Exceed"),  Exceed);    
      
  return ReturnObject;
}


extern "C" SEXP DynState(SEXP cParms,SEXP eParms, SEXP xControl, SEXP bin1size, SEXP bin2size) {

  /*******************************************************************************************************************/
  /* INITIALISE VARIABLES, READ VARIABLES,                                                                           */
  /*******************************************************************************************************************/
  int kEffortCapacity = EFFORTCAPACITY;
  int noSpec = NOSPEC;
   
  SEXP a = GET_DIM(cParms);
  int kNPatch  = INTEGER(a)[0]; 
  int kHorizon = INTEGER(a)[1];
  // number of species is INTEGER(a)[2], but has to be two in current setting
  int noInc    = INTEGER(a)[3];
  
  int kPlaiceCapacity = (noInc*kHorizon) + noInc;
  int kSoleCapacity   = (noInc*kHorizon) + noInc;
 
  double vPlaiceUplimit =            REAL(GET_SLOT(xControl,install("Spp1Uplimit"   )))[0];
  double vSoleUplimit   =            REAL(GET_SLOT(xControl,install("Spp2Uplimit"   )))[0];
  int    kEffortUplimit = (short) INTEGER(GET_SLOT(xControl,install("EffortUplimit" )))[0];
  int    kSimNumber     = (short) INTEGER(GET_SLOT(xControl,install("SimNumber"     )))[0];
  double vPricePlaice   =            REAL(GET_SLOT(xControl,install("Spp1Price"     )))[0];
  double vPriceSole     =            REAL(GET_SLOT(xControl,install("Spp2Price"     )))[0];
  double kPriceEffort   =            REAL(GET_SLOT(xControl,install("EffortPrice"   )))[0];
  double sizePlaiceInc  =            REAL(bin1size)[0];
  double sizeSoleInc    =            REAL(bin2size)[0];
 
  int   kPlaiceUplimit = vPlaiceUplimit/sizePlaiceInc;
  int   kSoleUplimit   = vSoleUplimit/sizeSoleInc;
  float kPricePlaice   = vPricePlaice*sizePlaiceInc;
  float kPriceSole     = vPriceSole*sizeSoleInc;
    
  SEXP ReturnObject, Simulations ;

  /****************************************************************************************************************************/
  /*CALC MAX EFFORT TO REDUCE COMPUTATION LATER BY CALC F ONLY BY MAXEFF * T AND TO CHECK IF NOT NOT EXCEEDING EFFORTCAPACITY */
  /****************************************************************************************************************************/
  int EffNoInc =0;
    for (int i = 0; i < kNPatch; i++) if (REAL(eParms)[i] > EffNoInc) EffNoInc = REAL(eParms)[i];

  /********************************************************************************************************************/
  /* Check if khorizon is not bigger than HORIZON, if so simulations will not extend arrays                           */
  /********************************************************************************************************************/
  if (kHorizon > MAXHORIZON) Rprintf("Error, horizon dimensions exceed array");
  if (kPlaiceCapacity > PLAICECAPACITY) Rprintf("Error, horizon*noinc for spp1 dimensions exceed array");
  if (kSoleCapacity   > SOLECAPACITY) Rprintf("Error, horizon*noinc for spp2 dimensions exceed array");    
  if (noInc > MAXNOINC) Rprintf("Error, noinc larger than MAXNOINC");
  if ((EffNoInc * kHorizon) > EFFORTCAPACITY) Rprintf("Error, maximum effort multiplied by time larger than EFFORTCAPACITY ");

  /********************************************************************************************************************/
  /* INITIALISE SRAND FOR RANDOM NUMBER GENERATOR                                                                     */
  /********************************************************************************************************************/
  // srand (1);
  srand(time(0));                         
  /**********************************************************************************************************************/
  /* DEFINE THE ARRAYS                                                                                                  */
  /**********************************************************************************************************************/
  FTYPE theF0    = (FTYPE) malloc((size_t)sizeof(*theF0) * kPlaiceCapacity);
  FTYPE theF1    = (FTYPE) malloc((size_t)sizeof(*theF1) * kPlaiceCapacity);
  ITYPE theIStar = (ITYPE) malloc((size_t)sizeof(*theIStar)*kPlaiceCapacity);
  PTYPE theParms = (PTYPE) malloc((size_t)sizeof(*theParms)*kNPatch);
  int *theEffortArray = (int *) malloc((size_t)kNPatch * sizeof (int));

  /*********************************************************************************************************************/
  /* CHECK WHETHER ARRAY ALLOCTION SUCCEEDED                                                                           */
  /*********************************************************************************************************************/
  if ((theF0 == 0l) || (theF1 == 0l) || (theIStar == 0l) || (theParms == 0)) Rprintf("error in memory allocation");
  
  /********************************************************************************************************************/
  /* WARN IF UPLIMITS +INCREMENTS EXCEED MODEL CAPACITIES                                                             */
  /********************************************************************************************************************/
  if ((kPlaiceUplimit + noInc > kPlaiceCapacity) || (kSoleUplimit + noInc > kSoleCapacity))
    Rprintf("Warning: Uplimit +NoInc will exceed Capacity for at least one species");

  /*********************************************************************************************************************/
  /* PUT Parmdata from xParms in theParms array                                                                        */
  /*********************************************************************************************************************/  
  for (int i = 0; i < kNPatch; i++)
      for (int t = 0; t < kHorizon; t++)
          for (int s = 0; s < noSpec; s++)
	          for (int inc = 0; inc < noInc; inc++){
	      	      theParms[i][t][s][inc] = REAL(cParms)[i + t*kNPatch + s*kNPatch*kHorizon + inc*kNPatch*noSpec*kHorizon]; 	
               }
  /*********************************************************************************************************************/
  /*INITIALISE THE EFFORTCOST FOR THE DIFFERENT PATCHES                                                                  */
  /*********************************************************************************************************************/  
  for (int i = 0; i < kNPatch; i++) theEffortArray[i] = REAL(eParms)[i]; 	
  
  /*********************************************************************************************************************/
  /*DEBUGCHECK                                                                                 */
  /*********************************************************************************************************************/
   Rprintf("noInc: ");  Rprintf("%d", noInc);
   Rprintf("; Spp1 Increment size "); Rprintf("%f",sizePlaiceInc);
   Rprintf("; Spp2 Increment size "); Rprintf("%f\n",sizeSoleInc);
   Rprintf("kSpp1Uplimit: ");  Rprintf("%d", kPlaiceUplimit);
   Rprintf("; kSpp2Uplimit: ");  Rprintf("%d\n", kSoleUplimit);
     
  /* variables */
  int Plaice, Sole, Effort;
  /*********************************************************************************************************************/
  /* INITIALISE F0 AND F1 ARRAY TO 0 IF IN OPENMP ENVIRONMENT                                                           */
  /*********************************************************************************************************************/
  int numthreads = 10;
  omp_set_num_threads(numthreads);
  
  int maxthreads = omp_get_max_threads();
  Rprintf("OPEN_MP environment: "); Rprintf("%d",numthreads); Rprintf(" threads\n");
  
  #pragma omp parallel private(Plaice,Sole,Effort)
  {
  #pragma omp for schedule(static)
  for (Plaice = 0; Plaice < kPlaiceCapacity ; Plaice++) {
    for (Effort = 0; Effort < kEffortCapacity; Effort++) {
      for (Sole = 0; Sole < kSoleCapacity; Sole++) {
        theF0[Plaice][Effort][Sole] = 0.0;
        theF1[Plaice][Effort][Sole] = 0.0;
      }
    }
  }
  } 
  /*********************************************************************************************************************/
  /* INITIALISE THE F1 ARRAY AT FINAL TIMESTEP (FINAL FITNESS FUNCTION)                                                */
  /*********************************************************************************************************************/
  for (Plaice = 0; Plaice < kPlaiceCapacity ; Plaice++) {
    for (Effort = 0; Effort < kEffortCapacity; Effort++) {
      for (Sole = 0; Sole < kSoleCapacity; Sole++) {
              theF1[Plaice][Effort][Sole] = utility(Plaice, kPlaiceUplimit, kPricePlaice, Sole, kSoleUplimit, kPriceSole, Effort, kEffortUplimit, kPriceEffort); 
  	  }
    }  
  }

  /****************************************************************************************************************/
  /* MAIN PART OF MODEL :  do backward calculations                                                               */
  /****************************************************************************************************************/
  vector<int> maxplaice(MAXHORIZON,0);
  vector<int> maxeff(MAXHORIZON,0);
  vector<int> maxsole(MAXHORIZON,0);
  vector< vector<SMALLINT> > val(MAXHORIZON,  vector<SMALLINT>(1,0)) ;
  vector< vector<unsigned short> > rep(MAXHORIZON,  vector<unsigned short>(1,1)) ; 
  
  for (int t = kHorizon - 1; t >= 0; t--)  {
    
     if ( (noInc * (t+1)) < (kPlaiceUplimit + noInc)) {maxplaice[t] = (noInc * (t+1));} else {maxplaice[t] = (kPlaiceUplimit + noInc);}
     if ( (noInc * (t+1)) < (kSoleUplimit + noInc)) {maxsole[t] = (noInc * (t+1));} else {maxsole[t] = (kSoleUplimit + noInc);}
   
     maxeff[t] = (EffNoInc * (t+1)); 
     Rprintf("Timestep "); Rprintf("%d", t+1); Rprintf(" maxple "); Rprintf("%d", maxplaice[t]);Rprintf(" maxsol "); Rprintf("%d", maxsole[t]);Rprintf(" maxeff "); Rprintf("%d", maxeff[t]);
     #pragma omp parallel private(Plaice,Sole,Effort)
  {
  #pragma omp for schedule(static)
     for (Plaice = 0; Plaice < maxplaice[t] ; Plaice++)	{
	     for (Effort = 0; Effort < maxeff[t]; Effort++){
         for (Sole = 0; Sole < maxsole[t]; Sole++){ 
		    	 theF0[Plaice][Effort][Sole] = FF (Plaice, Sole, Effort, kPriceEffort, t, noInc, theParms, kPlaiceUplimit, kSoleUplimit, kEffortUplimit, kNPatch, theF1, theIStar, theEffortArray);
         }
       }
     }
    }
    /***************************************************************************************************************/
    /* RLE ENCODING                                                                                                      */
    /***************************************************************************************************************/
    Rprintf(" RLE encoding \n");
    int ii = 0;
    val[t][0]= theIStar[0][0][0];
    rep[t][0] =1;
 
    for (Plaice = 0; Plaice < maxplaice[t]; Plaice++) {
      for (Effort = 0; Effort < maxeff[t]; Effort++){
        for (Sole = 0; Sole < maxsole[t]; Sole++) {
	        if (ii==0){  // stepping into procedure skip evaluationstep but move on  
		        ii++;
		      }else{ // do evaluation
		        if ((theIStar[Plaice][Sole][Effort] != val[t][ii-1]) ||( rep[t][ii-1] > 65531) ){  
		          ii++;
		          val[t].push_back( theIStar[Plaice][Sole][Effort]);
		          rep[t].push_back( 1);
		        }else{   
		          rep[t][ii-1]= rep[t][ii-1] + 1;
		        }
		      }
	      }
	    }
    } 
    /***************************************************************************************************************/
    /* PUT FO ARRAY IN F1 ARRAY AFTER TIMESTEP IS CALCULATED                                                       */
    /***************************************************************************************************************/
    for (Plaice = 0; Plaice < kPlaiceCapacity; Plaice++){
	    for (Effort = 0; Effort < kEffortCapacity; Effort++){
        for (Sole = 0; Sole < kSoleCapacity; Sole++){
          theF1[Plaice][Effort][Sole] =  theF0[Plaice][Effort][Sole];
        }
      }
    }
  }
  
  Rprintf("Backward calculations done \n");

  /***************************************************************************************************************/
  /* DO MONTE CARLO SIMULATIONS                                                                                  */
  /***************************************************************************************************************/
  Simulations = Simulate(kSimNumber, kHorizon, maxplaice, maxsole, maxeff, val, rep, theIStar, kPlaiceUplimit, kSoleUplimit, noInc, sizeSoleInc, sizePlaiceInc, theParms, theEffortArray);
  Rprintf("Simulations done \n");
  
  PROTECT(ReturnObject = NEW_OBJECT(MAKE_CLASS("DynState")));
  SET_SLOT(ReturnObject, install("Sim"),  Simulations);
  UNPROTECT(12);
  
  matfree(theF0);
  matfree(theF1);
  matfree(theIStar);
  matfree(theParms);
    
  return ReturnObject;
}

