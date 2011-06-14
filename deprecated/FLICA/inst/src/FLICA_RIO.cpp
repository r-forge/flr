//D:\FLR\Packages\FLICA\inst\libs

#include "flica_rio.hpp"

bool InputFLQuantY(SEXP FLQuant, double D[DIMS_NYrs], int Min, int Max)
    {
    if ((Max-Min+1)>DIMS_NYrs)
        return false;

    int _Min, _Max;

    SEXP Quant    = PROTECT(duplicate(GET_SLOT(FLQuant, install(".Data")))),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    double *Q     = NUMERIC_POINTER(Quant);

    int dim[6], n = length(dims);

    if (n > 6)
       return false;
 
    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];
    dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 

    if (((int)dim[0]) != 1 || ((int)dim[1]) <  1 || 
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

         if (INTEGER(dims)[1] > 1) 
            t = atoi(CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 1), 0))) - 1; 
      
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
      D[j] = (Q)[j];       

   UNPROTECT(1);

   return TRUE;
   }

bool InputFLQuantAY(SEXP FLQuant, double D[DIMS_NAges][DIMS_NYrs], int MinAge, int MaxAge, int MinYear, int MaxYear)
    {
    if ((MaxAge-MinAge+1)>DIMS_NAges || (MaxYear-MinYear+1)>DIMS_NYrs)
        return false;

    SEXP Quant    = PROTECT(duplicate(GET_SLOT(FLQuant, install(".Data")))),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    double *Q     = NUMERIC_POINTER(Quant);

    int dim[6], n = length(dims);
 
    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];
    dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 


    if (((int)dim[0]) <  1 || ((int)dim[1]) <  1 || 
        ((int)dim[2]) != 1 || ((int)dim[3]) != 1 || 
        ((int)dim[4]) != 1 || ((int)dim[5]) != 1)
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

    int iAge, iYear,
        i,    j;

    for (iAge = MinAge, i = 0; iAge <= MaxAge; iAge++, i++)
       for (iYear = MinYear, j = 0; iYear <= MaxYear; iYear++, j++)
          D[i][j] = (Q)[i + j*(_MaxAge-_MinAge+1)];       
   
    UNPROTECT(1);

    return TRUE;
    }

bool InputFLQuantAY(SEXP FLQuant, double D[DIMS_NAges][DIMS_NYrs], int *pMinAge, int *pMaxAge, int *pMinYear, int *pMaxYear)
    {
    SEXP Quant    = PROTECT(duplicate(GET_SLOT(FLQuant, install(".Data")))),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    double *Q     = NUMERIC_POINTER(Quant);

    int dim[6], n = length(dims);
 
    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];
    dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 


    if (((int)dim[0]) <  1 || ((int)dim[1]) <  1 || 
        ((int)dim[2]) != 1 || ((int)dim[3]) != 1 || 
        ((int)dim[4]) != 1 || ((int)dim[5]) != 1)
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

    if ((_MaxAge-_MinAge+1)>DIMS_NAges || (_MaxYear-_MinYear+1)>DIMS_NYrs)
      {
      UNPROTECT(1);

      return FALSE;
      }

    *pMinAge  = _MinAge;
    *pMaxAge  = _MaxAge;
    *pMinYear = _MinYear;
    *pMaxYear = _MaxYear;

    int iAge, iYear,
        i,    j;

    for (iAge = *pMinAge, i = 0; iAge <= *pMaxAge; iAge++, i++)
       for (iYear = *pMinYear, j = 0; iYear <= *pMaxYear; iYear++, j++)
          D[i][j] = (Q)[i + j*(_MaxAge-_MinAge+1)];       
   
    UNPROTECT(1);

    return TRUE;
    }

double InputFLQuantMeanVal(SEXP FLQuant)
    {
    SEXP Quant    = PROTECT(duplicate(GET_SLOT(FLQuant, install(".Data")))),
         dims     = GET_DIM(Quant);

    double *Q     = NUMERIC_POINTER(Quant);

    double Val = 0.0;

    int dim[6]; 

    if (length(dims) < 5) return 0.0;
 
    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];
    dim[5] = length(dims)>=6 ? INTEGER(dims)[5] : 1; 


    int n = 0;
    for (int i = 0; i < dim[0]; i++)
      for (int j = 0; j < dim[1]; j++)
         for (int k = 0; k < dim[2]; k++)
            for (int l = 0; l < dim[3]; l++)
               for (int m = 0; m < dim[4]; m++)
                  for (int p = 0; p < dim[4]; p++)
                     Val += (Q)[n++];       

   
    UNPROTECT(1);

    return Val/n;
    }

bool InputFLQuantAYMeanByYear(SEXP FLQuant, double D[DIMS_NAges], int MinAge, int MaxAge, int MinYear, int MaxYear)
    {
    if ((MaxAge-MinAge+1)>DIMS_NAges || (MaxYear-MinYear+1)>DIMS_NYrs)
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
    dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 

    if (((int)dim[0]) <  1 || ((int)dim[1]) <  1 || 
        ((int)dim[2]) != 1 || ((int)dim[3]) != 1 ||
        ((int)dim[4]) != 1 || ((int)dim[5]) != 1)
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

    int iAge, iYear,
        i,    j;

    for (iAge = MinAge, i = 0; iAge <= MaxAge; iAge++, i++)
       for (iYear = MinYear, j = 0; iYear <= MaxYear; iYear++, j++)
          D[i] = (Q)[i + j*(_MaxAge-_MinAge+1)];       
   
    UNPROTECT(1);

    return TRUE;
    }

SEXP CreateFLQuantY(double D[DIMS_NYrs], int MinYear, int MaxYear)
    {
    int i, iYear;
 
    SEXP FLQuant, v, 
         d1, d2, d3, d4, d5,  
         dim,   dimnames, names;    

   if ((MaxYear-MinYear+1)>DIMS_NYrs)
         return false;

    //Create new S4 object    
    //PROTECT(v = NEW_OBJECT(MAKE_CLASS("array")));
    PROTECT(FLQuant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 5));       
    INTEGER(dim)[0] = 1;
    INTEGER(dim)[1] = MaxYear-MinYear+1;
    INTEGER(dim)[2] = 1; 
    INTEGER(dim)[3] = 1; 
    INTEGER(dim)[4] = 1; 
     
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
 
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 5));
 
    PROTECT(d1 = allocVector(STRSXP, 1));
    SET_STRING_ELT(d1, 0, mkChar("all"));
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
    for (iYear=MinYear, i=0; iYear<=MaxYear; iYear++, i++)
        REAL(v)[i] = D[i];
 
    //Set slot
    //FLQuant = SET_SLOT(FLQuant, install("v"), v);
    FLQuant = R_do_slot_assign(FLQuant, install(".Data"), v);

    UNPROTECT(10);
 
    return v; //FLQuant;
    }

SEXP CreateFLQuantAY(double D[DIMS_NAges][DIMS_NYrs], int MinAge, int MaxAge, int MinYear, int MaxYear)
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
          REAL(v)[i + j*(MaxAge-MinAge+1)] = D[i][j];

    //Set slot
    FLQuant = R_do_slot_assign(FLQuant, install(".Data"), v);

    UNPROTECT(10);
 
    return v; //FLQuant;
    }

int NElemList(SEXP x)
   {
   //Check that it is a list
   if (!IS_LIST(x) || TYPEOF(x) != VECSXP) 
      return 0;
   else
      return length(x);
  }

short GetCPUEType(char *TypeStr)
   {
   //a (0), l (1) or p (2)
   
   int result = strspn(TypeStr, " " );

   if (TypeStr[result] == 'a' || TypeStr[result] == 'A')
      return 0;
   if (TypeStr[result] == 'l' || TypeStr[result] == 'L')
      return 1;
   if (TypeStr[result] == 'p' || TypeStr[result] == 'P')
      return 2;

   return 1;
   }

FLConstUnits GetType(SEXP xType)
   {
   char *TypeStr;

   //Number (1), Biomass (2)
   
   if (!isString(xType))
      return FLcNumber;

   TypeStr = CHAR(STRING_ELT(xType, 0));
   
   int result = strspn(TypeStr, " " );

   if (TypeStr[result] == 'N' || TypeStr[result] == 'n')
      return FLcNumber;
   if (TypeStr[result] == 'B' || TypeStr[result] == 'b')
      return FLcBiomass;
  
   return FLcNumber;
   }

bool IsBiomassIndex(SEXP xCPUE, int i)
   {
   SEXP xIndex = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("index"))));
   SEXP xType  = PROTECT(duplicate(GET_SLOT(VECTOR_ELT(xCPUE, i), install("type"))));

   //Units in Biomass?
   if (FLcBiomass != GetType(xType)) 
      {
      UNPROTECT(2);

      return false; 
      }

   //Is length of age dim == 1 ?
   SEXP Quant    = PROTECT(duplicate(GET_SLOT(xIndex, install(".Data")))),
        dims     = GET_DIM(Quant),
        dimnames = GET_DIMNAMES(Quant);

    if ((INTEGER(dims)[0]) != 1)
      {
      UNPROTECT(3);
 
      return false;
      }

   //Is name of dim - "all"?
   char *Str = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 0), 0));
   //int t = strcmp(Str, "a");
   if (Str[0] == 'a')
      {
      UNPROTECT(3);

      return true;
      }

   UNPROTECT(3);

   return false;
   }

bool InputStock(SEXP xStock)
   {
   SEXP range  = PROTECT(duplicate(GET_SLOT(xStock, install("range"))));    
   if (length(range) <5)
      return false;

   STCK_mp_FIRSTAGE  = int(REAL(range)[0]);
   STCK_mp_LASTAGE   = int(REAL(range)[1]);
   STCK_mp_FIRSTYEAR = int(REAL(range)[3]);
   STCK_mp_LASTYEAR  = int(REAL(range)[4]);


   SEXP xCN    = PROTECT(duplicate(GET_SLOT(xStock, install("catch.n"))));       // Catch Number 
   SEXP xNM    = PROTECT(duplicate(GET_SLOT(xStock, install("m"))));             // Natural Mortality
   SEXP xSW    = PROTECT(duplicate(GET_SLOT(xStock, install("stock.wt"))));      // Stock Weight
   SEXP xMO    = PROTECT(duplicate(GET_SLOT(xStock, install("mat"))));           // Maturity Ogive
   SEXP xCW    = PROTECT(duplicate(GET_SLOT(xStock, install("catch.wt"))));      // Stock Weight
   SEXP xLA    = PROTECT(duplicate(GET_SLOT(xStock, install("landings"))));      // Reported Landings 
   SEXP xPF    = PROTECT(duplicate(GET_SLOT(xStock, install("harvest.spwn"))));  // Proportions of F before spawning
   SEXP xPM    = PROTECT(duplicate(GET_SLOT(xStock, install("m.spwn"))));        // Proportions of M before spawning

   InputFLQuantAY(xCN, STCK_mp_CN, STCK_mp_FIRSTAGE, STCK_mp_LASTAGE, STCK_mp_FIRSTYEAR, STCK_mp_LASTYEAR);
   InputFLQuantAY(xNM, STCK_mp_NM, STCK_mp_FIRSTAGE, STCK_mp_LASTAGE, STCK_mp_FIRSTYEAR, STCK_mp_LASTYEAR);
   InputFLQuantAY(xSW, STCK_mp_SW, STCK_mp_FIRSTAGE, STCK_mp_LASTAGE, STCK_mp_FIRSTYEAR, STCK_mp_LASTYEAR);
   InputFLQuantAY(xMO, STCK_mp_MO, STCK_mp_FIRSTAGE, STCK_mp_LASTAGE, STCK_mp_FIRSTYEAR, STCK_mp_LASTYEAR);
   InputFLQuantAY(xCW, STCK_mp_CW, STCK_mp_FIRSTAGE, STCK_mp_LASTAGE, STCK_mp_FIRSTYEAR, STCK_mp_LASTYEAR);
   InputFLQuantY( xLA, STCK_mp_LA,                                    STCK_mp_FIRSTYEAR, STCK_mp_LASTYEAR);
   
   //Sort out plusgroup
   int PlusGroup = int(REAL(range)[2]);

   if (!ISNA(REAL(range)[2]) && PlusGroup < STCK_mp_LASTAGE)   
      for (int iYear = 0; iYear < STCK_mp_FIRSTYEAR - STCK_mp_LASTYEAR; iYear++)
         {
         double SumCatch = 0.0;

         for (int iAge = PlusGroup+1-STCK_mp_FIRSTAGE; iAge <= STCK_mp_LASTAGE-STCK_mp_FIRSTAGE; iAge++)
            SumCatch += STCK_mp_CN[iAge][iYear];

         STCK_mp_NM[PlusGroup][iYear] = STCK_mp_NM[PlusGroup][iYear]*STCK_mp_CN[PlusGroup][iYear];
         STCK_mp_SW[PlusGroup][iYear] = STCK_mp_SW[PlusGroup][iYear]*STCK_mp_CN[PlusGroup][iYear];
         STCK_mp_MO[PlusGroup][iYear] = STCK_mp_MO[PlusGroup][iYear]*STCK_mp_CN[PlusGroup][iYear];
         STCK_mp_CW[PlusGroup][iYear] = STCK_mp_CW[PlusGroup][iYear]*STCK_mp_CN[PlusGroup][iYear];
          
         for (iAge = PlusGroup+1; iAge <= STCK_mp_LASTAGE; iAge++)
            {
            STCK_mp_CN[PlusGroup][iYear] += STCK_mp_CN[iAge][iYear];
            STCK_mp_NM[PlusGroup][iYear] += STCK_mp_NM[iAge][iYear]*STCK_mp_CN[iAge][iYear];
            STCK_mp_SW[PlusGroup][iYear] += STCK_mp_SW[iAge][iYear]*STCK_mp_CN[iAge][iYear];
            STCK_mp_MO[PlusGroup][iYear] += STCK_mp_MO[iAge][iYear]*STCK_mp_CN[iAge][iYear];
            STCK_mp_CW[PlusGroup][iYear] += STCK_mp_CW[iAge][iYear]*STCK_mp_CN[iAge][iYear];
         
            STCK_mp_CN[iAge][iYear] = 
            STCK_mp_NM[iAge][iYear] =
            STCK_mp_SW[iAge][iYear] =
            STCK_mp_MO[iAge][iYear] =
            STCK_mp_CW[iAge][iYear] = 0.0;
            }

         STCK_mp_NM[iAge][iYear] /= SumCatch;
         STCK_mp_SW[iAge][iYear] /= SumCatch;
         STCK_mp_MO[iAge][iYear] /= SumCatch;
         STCK_mp_CW[iAge][iYear] /= SumCatch;
         }

   STCK_mp_PF = InputFLQuantMeanVal(xPF);
   STCK_mp_PM = InputFLQuantMeanVal(xPM);
   
   UNPROTECT(9);

   for (int i =0; i < STCK_mp_LASTAGE-STCK_mp_FIRSTAGE; i++)
      {
      int lastyr = STCK_mp_LASTYEAR-STCK_mp_FIRSTYEAR;

		STCK_mp_SW[i][lastyr+1] = STCK_mp_SW[i][lastyr];
		STCK_mp_NM[i][lastyr+1] = STCK_mp_NM[i][lastyr];
		STCK_mp_MO[i][lastyr+1] = STCK_mp_MO[i][lastyr];
      }

   return true; 
   }

bool InputCPUE(SEXP xCPUE, SEXP xControl, FLTypeCPUEFlags &CPUEFlags)
   {
   CPUEFlags.n = NElemList(xCPUE);

   for (int i=0; i < CPUEFlags.n; i++)
      {
      CPUEFlags.Use[i]     = TRUE;
      CPUEFlags.Biomass[i] = IsBiomassIndex(xCPUE,i);
      if (CPUEFlags.Biomass[i])
           CPUEFlags.PlusGroup[i] = true;
//         CPUE_mp_XPLUSGP[i] = CPUEFlags.PlusGroup[i] = true;
      }


   CPUE_mp_NAGEIX =
   CPUE_mp_NSSBIX = 0;

   int NProtect = 0;
   for (i=0; i < CPUEFlags.n; i++)
      {
      //Check plusgroup
      SEXP x      = VECTOR_ELT(xCPUE, i);
      SEXP xRange = PROTECT(duplicate(GET_SLOT(x, install("range")))); NProtect++;

      if (length(xRange) <3)
         return false;

      if (!CPUEFlags.Biomass[i])
         {
         CPUEFlags.PlusGroup[i] = (!ISNA(REAL(xRange)[2])); // || ((int(REAL(xRange)[1])) <=  (int(REAL(xRange)[2]))));
         //CPUE_mp_XPLUSGP[i] = CPUEFlags.PlusGroup[i] = (!ISNA(REAL(xRange)[2])); // || ((int(REAL(xRange)[1])) <=  (int(REAL(xRange)[2]))));
  
          //Get range
         int fage  = int(REAL(xRange)[0]),
             lage  = int(REAL(xRange)[1]),
             fyear = int(REAL(xRange)[3]),
             lyear = int(REAL(xRange)[4]);
 
        //Get Index
         double Index[DIMS_NAges][DIMS_NYrs];
         SEXP xIndex  = PROTECT(duplicate(GET_SLOT(x, install("index"))));    NProtect++;
         if (InputFLQuantAY(xIndex, Index, fage, lage,  fyear, lyear))
            {
            //Set ranges
            CPUE_mp_FAGE[CPUE_mp_NAGEIX]  = fage;
            CPUE_mp_LAGE[CPUE_mp_NAGEIX]  = lage;
            CPUE_mp_FYEAR[CPUE_mp_NAGEIX] = fyear;
            CPUE_mp_LYEAR[CPUE_mp_NAGEIX] = lyear;

            //Get Timing
            CPUE_mp_TIMING[CPUE_mp_NAGEIX] = (__max(__min(REAL(xRange)[5],1.0),0.0) + __max(__min(REAL(xRange)[6],1.0),0.0))/2.0;       
            
            for (int iAge = 0; iAge <= CPUE_mp_LAGE[CPUE_mp_NAGEIX] - CPUE_mp_FAGE[CPUE_mp_NAGEIX]; iAge++)
               for (int iYear = 0; iYear <= CPUE_mp_LYEAR[CPUE_mp_NAGEIX]-CPUE_mp_FYEAR[CPUE_mp_NAGEIX]; iYear++)
                  CPUE_mp_ASURVEY[iAge][iYear][CPUE_mp_NAGEIX] = (Index[iAge][iYear]> 0.0 ? log(Index[iAge][iYear]) : -99.0);

            
            //Set Wts
            double Wts[DIMS_NAges][DIMS_NYrs];
            SEXP xWt = PROTECT(duplicate(GET_SLOT(x, install("index.var"))));    NProtect++;
      
            if (InputFLQuantAY(xWt, Wts, CPUE_mp_FAGE[CPUE_mp_NAGEIX], CPUE_mp_LAGE[CPUE_mp_NAGEIX],  CPUE_mp_FYEAR[CPUE_mp_NAGEIX], CPUE_mp_LYEAR[CPUE_mp_NAGEIX]))
               for (int iAge = 0; iAge <= CPUE_mp_LAGE[CPUE_mp_NAGEIX] - CPUE_mp_FAGE[CPUE_mp_NAGEIX]; iAge++)
                  {
                  double t = 0.0;
                  for (int iYear = 0; iYear <= CPUE_mp_LYEAR[CPUE_mp_NAGEIX]-CPUE_mp_FYEAR[CPUE_mp_NAGEIX]; iYear++)
                     t += 1.0/Wts[iAge][iYear];
         
                  t /= (CPUE_mp_LYEAR[CPUE_mp_NAGEIX]-CPUE_mp_FYEAR[CPUE_mp_NAGEIX]+1);
                  CPUE_mp_ALAMBDA[iAge][CPUE_mp_NAGEIX] = t;
                  }
            else
               for (int iAge = 0; iAge <= CPUE_mp_LAGE[CPUE_mp_NAGEIX] - CPUE_mp_FAGE[CPUE_mp_NAGEIX]; iAge++)
                  CPUE_mp_ALAMBDA[iAge][CPUE_mp_NAGEIX] = 1.0;


            CPUEFlags.Use[i] = true;
            CPUE_mp_NAGEIX++;
            }
         }
      else
         {
         //Get range
         int fyear  = int(REAL(xRange)[3]),
             lyear  = int(REAL(xRange)[4]);
         
         //Get Index
         double Index[DIMS_NYrs];
      
         SEXP xIndex  = PROTECT(duplicate(GET_SLOT(x, install("index"))));    NProtect++;
     
         if (InputFLQuantY(xIndex, Index, fyear, lyear))
            {           
            CPUE_mp_FBYEAR = fyear;
            CPUE_mp_LBYEAR = lyear;

            for (int iYear = 0; iYear <= CPUE_mp_LBYEAR-CPUE_mp_FBYEAR; iYear++)
               CPUE_mp_BSURVEY[iYear][CPUE_mp_NSSBIX] = (Index[iYear] > 0.0 ? log(Index[iYear]) : -99.0);

            //Get Weights
            double Wts[DIMS_NYrs];
   
            SEXP xWt    = PROTECT(duplicate(GET_SLOT(x, install("index.var"))));         NProtect++;    
   
            if (InputFLQuantY(xWt, Wts, CPUE_mp_FBYEAR, CPUE_mp_LBYEAR))
               {
               double t = 0.0;
               for (int iYear = 0; iYear <= CPUE_mp_LBYEAR-CPUE_mp_FBYEAR; iYear++)
                  CPUE_mp_BLAMBDA[CPUE_mp_NSSBIX] += 1.0/Wts[iYear];
      
               CPUE_mp_BLAMBDA[CPUE_mp_NSSBIX] /= (CPUE_mp_LBYEAR-CPUE_mp_FBYEAR+1);
               }
            else
               CPUE_mp_BLAMBDA[CPUE_mp_NSSBIX] = 1.0;

            CPUEFlags.Use[i] = true;
            CPUE_mp_NSSBIX++;
            }
         }
      }      
      
   int j;
   for (i=0, j=0; i < CPUEFlags.n; i++)
      if (!CPUEFlags.Biomass[i]) {
         CPUE_mp_PLUSGP[j]    = CPUEFlags.PlusGroup[i];
         CPUE_mp_XPLUSGP[j++] = CPUEFlags.PlusGroup[i] ? 1 : 0;
         }
  
   UNPROTECT(NProtect);

   return true;
   }

bool InputControl(SEXP xControl, FLTypeCPUEFlags &CPUEFlags)
   {
   CONTROL_mp_RWOPT     = 2;

   CONTROL_mp_DLLFLAG   = true;    // Flag to indicate whether DLL, if true turns off cosole IO
   CONTROL_mp_WRITEOUT  = false;   // whether objective function writes out residuals to disk
   CONTROL_mp_FULL      = false;   // whether objective funtion recalculates whole VPA, or only as far back as is currently required    
  
   //NySep - Number of years for separable model 
   CONTROL_mp_NYSEP = INTEGER(PROTECT(duplicate(GET_SLOT(xControl, install("sep.nyr")))))[0];
   UNPROTECT(1);
  
   //RefAge - Reference age for fitting the separable model  
   CONTROL_mp_REFAGE = INTEGER(PROTECT(duplicate(GET_SLOT(xControl, install("sep.age")))))[0];
   UNPROTECT(1);
  
   CONTROL_mp_TWOSEL    = false;   // whether to have 2 selection patterns 
   CONTROL_mp_STEPSEL   = false;   // whether change in selection is step or gradual

	//Selection on last true reference age 
   // last year for constant selection, if 2 sel patterns chosen
   SEXP xChangeSel = PROTECT(duplicate(GET_SLOT(xControl,install("sep.2")))); 
   if (!ISNA(INTEGER(xChangeSel)[0]) && INTEGER(xChangeSel)[0]>0)
      {
      CONTROL_mp_CHANGESEL = INTEGER(xChangeSel)[0];  
      CONTROL_mp_TWOSEL    = true;
      }
   UNPROTECT(1);
    
   // whether change in selection is step or gradual
   CONTROL_mp_STEPSEL = LOGICAL(PROTECT(duplicate(GET_SLOT(xControl,install("sep.gradual")))))[0];  
   

   SEXP xTermSel = PROTECT(duplicate(GET_SLOT(xControl, install("sep.sel")))); 
   CONTROL_mp_TERMS =  REAL(xTermSel)[0];
   if (length(xTermSel)>1 && CONTROL_mp_TWOSEL)
      CONTROL_mp_TERMS2 = REAL(xTermSel)[1];
   else
      CONTROL_mp_TERMS2 = CONTROL_mp_TERMS;
   UNPROTECT(2);

   // Stock and recruitment parameters [
   // whether to fit a stock-recruit relation
   CONTROL_mp_FITSRR = LOGICAL(PROTECT(duplicate(GET_SLOT(xControl,install("sr")))))[0];  
   UNPROTECT(1);

   CONTROL_mp_LAG = __max(0,INTEGER(PROTECT(duplicate(GET_SLOT(xControl, install("sr.age")))))[0]);
   UNPROTECT(2);  

   // weight for the SRR term in the objective function
   CONTROL_mp_SRRLAMBDA = REAL(PROTECT(duplicate(GET_SLOT(xControl, install("lambda.sr")))))[0];
   UNPROTECT(1);


	// Weighting matrices for catch-at-age; for aged surveys; for SSB surveys
   //Relative weights by age   
   SEXP xWtAge = PROTECT(duplicate(GET_SLOT(xControl,install("lambda.age"))));   
   for (int i=0; i<STCK_mp_LASTAGE-STCK_mp_FIRSTAGE; i++)
      CONTROL_mp_RELWT[i] = REAL(xWtAge)[__min(i,length(xWtAge)-1)];  
   UNPROTECT(1);

   //Relative weights by year   
   SEXP xWtYr = PROTECT(duplicate(GET_SLOT(xControl,install("lambda.yr"))));   
   for (i=0; i<CONTROL_mp_NYSEP; i++)
      CONTROL_mp_YEARWT[i] = REAL(xWtYr)[__min(i,length(xWtYr)-1)];  
   UNPROTECT(1);

   int j, k;
	for (i=0, j=STCK_mp_LASTYEAR-STCK_mp_FIRSTYEAR-CONTROL_mp_NYSEP+1; i<CONTROL_mp_NYSEP; i++, j++)
      for (k=0; k<STCK_mp_LASTAGE-STCK_mp_FIRSTAGE; k++)
   		CONTROL_mp_CNLAMBDA[k][j]=pow(CONTROL_mp_RELWT[k]*CONTROL_mp_YEARWT[i],0.5); 


   SEXP xUType = PROTECT(duplicate(GET_SLOT(xControl,install("index.model"))));   
	SEXP xUCor  = PROTECT(duplicate(GET_SLOT(xControl,install("index.cor" ))));
   int iIndex  = 0;
   int iBIndex = 0; 	
   for (i = 0; i < CPUEFlags.n; i++)
      if (CPUEFlags.Biomass[i])
         {
         CPUE_mp_QBPARM[iBIndex++] = CPUEFlags.Type[i] = GetCPUEType(CHAR(VECTOR_ELT(xUType, __min(i,length(xUType)-1))));  
         //CONTROL_mp_IXCOR[i]     = 0.0;
         //CPUEFlags.Cor[i]        = 0.0;
         }
      else
         {   
         CPUE_mp_QAPARM[iIndex]   = CPUEFlags.Type[i] = GetCPUEType(CHAR(VECTOR_ELT(xUType, __min(i,length(xUType)-1))));  
         CONTROL_mp_IXCOR[iIndex] = 1.0 - __min(1.0, __max(0.0, REAL(xUCor)[ __min(i,length(xUCor)-1)]));
         CPUEFlags.Cor[i]         =       __min(1.0, __max(0.0, REAL(xUCor)[ __min(i,length(xUCor)-1)]));
         //CPUEFlags.Pos[i]         = (iIndex++) +iBIndex;
         CPUEFlags.Pos[i]         = (iIndex++);
         }

   UNPROTECT(2);
 
   return PARMINIT();
   }

SEXP CreateFLICA(SEXP xControl, FLTypeCPUEFlags &CPUEFlags)
   { 
   SEXP ReturnObject    = R_NilValue;

   PROTECT(ReturnObject = NEW_OBJECT(MAKE_CLASS("FLICA")));

   //Output results to R 
   SET_SLOT(ReturnObject, install("stock.n"),     CreateFLQuantAY(STCK_mp_N, STCK_mp_FIRSTAGE, STCK_mp_LASTAGE, STCK_mp_FIRSTYEAR, STCK_mp_LASTYEAR));
   SET_SLOT(ReturnObject, install("harvest"),     CreateFLQuantAY(STCK_mp_F, STCK_mp_FIRSTAGE, STCK_mp_LASTAGE, STCK_mp_FIRSTYEAR, STCK_mp_LASTYEAR));

   for (int iAge = 0; iAge <= STCK_mp_LASTAGE - STCK_mp_FIRSTAGE; iAge++)
      {
      for (int iYear = 0; iYear < STCK_mp_LASTYEAR-STCK_mp_FIRSTYEAR-CONTROL_mp_NYSEP+1; iYear++)
         RESULTS_mp_PREDCN[iAge][iYear] = STCK_mp_CN[iAge][iYear];

      for (iYear = STCK_mp_LASTYEAR-STCK_mp_FIRSTYEAR-CONTROL_mp_NYSEP+1; iYear <= STCK_mp_LASTYEAR-STCK_mp_FIRSTYEAR; iYear++)
         {
         double Z = STCK_mp_F[iAge][iYear] + STCK_mp_NM[iAge][iYear];
         
         RESULTS_mp_PREDCN[iAge][iYear] = STCK_mp_N[iAge][iYear]*(STCK_mp_F[iAge][iYear]/Z)*(1.0-exp(-Z));                     
         }
      }

   SET_SLOT(ReturnObject, install("catch.n"), CreateFLQuantAY(RESULTS_mp_PREDCN, STCK_mp_FIRSTAGE, STCK_mp_LASTAGE, STCK_mp_FIRSTYEAR, STCK_mp_LASTYEAR));

   //Control
   SET_SLOT(ReturnObject, install("control"),  ReturnControl(CPUEFlags));

   //q residuals
   SEXP index, indexres, indexhat, q, qvar;
   PROTECT(index    = allocVector(VECSXP,CPUE_mp_NAGEIX+CPUE_mp_NSSBIX));
   PROTECT(indexres = allocVector(VECSXP,CPUE_mp_NAGEIX+CPUE_mp_NSSBIX));
   PROTECT(indexhat = allocVector(VECSXP,CPUE_mp_NAGEIX+CPUE_mp_NSSBIX));
   PROTECT(q        = allocVector(VECSXP,CPUE_mp_NAGEIX+CPUE_mp_NSSBIX));
   PROTECT(qvar     = allocVector(VECSXP,CPUE_mp_NAGEIX+CPUE_mp_NSSBIX));
   for (int i=1; i<=CPUEFlags.n; i++)
      {
      SET_VECTOR_ELT(index,    i-1,  ReturnIndex(   i, CPUEFlags));
      SET_VECTOR_ELT(indexres, i-1,  ReturnIndexRes(i, CPUEFlags));
      SET_VECTOR_ELT(indexhat, i-1,  ReturnIndexHat(i, CPUEFlags));
//      SET_VECTOR_ELT(q,        i-1,  ReturnQ(       i, CPUEFlags));
//      SET_VECTOR_ELT(qvar,     i-1,  ReturnQvar(    i, CPUEFlags));
      }

   SET_SLOT(ReturnObject, install("index"),     index);
   SET_SLOT(ReturnObject, install("index.res"), indexres);
   SET_SLOT(ReturnObject, install("index.hat"), indexhat);
   SET_SLOT(ReturnObject, install("q"),         q);
   SET_SLOT(ReturnObject, install("qvar"),      qvar);

   //ReturnDiagnostics(&ReturnObject); 
   
   UNPROTECT(4);

   return ReturnObject;
   }

SEXP ReturnControl(FLTypeCPUEFlags &CPUEFlags)
   {
   SEXP xControl = R_NilValue;

   PROTECT(xControl = NEW_OBJECT(MAKE_CLASS("FLICA.control")));

   SEXP SepNyr		 = R_NilValue,  
		  SepAge		 = R_NilValue,  
		  SepTerm    = R_NilValue,  
		  SR			 = R_NilValue,  
		  SRAge      = R_NilValue,  
		  WtAge		 = R_NilValue,  
		  WtYr		 = R_NilValue,  
		  WtSr		 = R_NilValue,  
		  UType      = R_NilValue,  
		  UCor       = R_NilValue;

   SepNyr   = PROTECT(NEW_INTEGER(1));
	SepAge   = PROTECT(NEW_INTEGER(1));
	SepTerm  = PROTECT(NEW_NUMERIC(1));
	SR		   = PROTECT(NEW_LOGICAL(1));
	SRAge    = PROTECT(NEW_INTEGER(1));
	WtAge	   = PROTECT(NEW_NUMERIC(STCK_mp_LASTAGE -STCK_mp_FIRSTAGE +1));
	WtYr	   = PROTECT(NEW_NUMERIC(CONTROL_mp_NYSEP));
	WtSr	   = PROTECT(NEW_NUMERIC(1));
	UCor     = PROTECT(NEW_NUMERIC(CPUEFlags.n));
	PROTECT(UType = allocVector(STRSXP, CPUEFlags.n));
   
   INTEGER(SepNyr)[0] = CONTROL_mp_NYSEP;             // NySep - Number of years for separable model 
   INTEGER(SepAge)[0] = CONTROL_mp_REFAGE;            // RefAge - Reference age for fitting the separable model  
	REAL(SepTerm)[0]   = CONTROL_mp_TERMS;             // selection on last true reference age 
	LOGICAL(SR)[0]		 = CONTROL_mp_FITSRR;            // whether to fit a stock-recruit relation
	INTEGER(SRAge)[0]  = CONTROL_mp_LAG;
	REAL(WtAge)[0]	    = CONTROL_mp_RELWT[DIMS_NAges];	// Relative weights by age 
	REAL(WtYr)[0]	    = CONTROL_mp_YEARWT[DIMS_NYrs];	// Relative weights by year  
	REAL(WtSr)[0]	    = CONTROL_mp_SRRLAMBDA;

   int i;
   for (i=0; i<=STCK_mp_LASTAGE-STCK_mp_FIRSTAGE; i++)
	   REAL(WtAge)[i]	 = CONTROL_mp_RELWT[i];
   for (i=0; i<CONTROL_mp_NYSEP; i++)
   	REAL(WtYr)[i]    = CONTROL_mp_YEARWT[i];
	   
   for (i=0; i<CPUEFlags.n; i++)
      {   
	   REAL(UCor)[i]    = CPUEFlags.Cor[i];

      if (CPUEFlags.Type[i] == 0)
         SET_STRING_ELT(UType, i, mkChar("absolute"));
      else if (CPUEFlags.Type[i] == 1)
         SET_STRING_ELT(UType, i, mkChar("linear"));
      else if (CPUEFlags.Type[i] == 2)
         SET_STRING_ELT(UType, i, mkChar("power"));
	   }
      
   SET_SLOT(xControl,install("sep.nyr"),      SepNyr);
	SET_SLOT(xControl,install("sep.age"),      SepAge);
	SET_SLOT(xControl,install("sep.sel"),      SepTerm);
	SET_SLOT(xControl,install("sr"),           SR);
	SET_SLOT(xControl,install("sr.age"),       SRAge);
	SET_SLOT(xControl,install("lambda.age"),   WtAge);
	SET_SLOT(xControl,install("lambda.yr"),    WtYr);
	SET_SLOT(xControl,install("lambda.sr"),    WtSr);
	SET_SLOT(xControl,install("index.model"),  UType);
	SET_SLOT(xControl,install("index.cor"),    UCor);
   

   UNPROTECT(11);

   return xControl;
   }

void ReturnDiagnostics(SEXP *ReturnObject)
    {
    int i,    j, 
        iAge, iYear;
    
    SEXP v,   d1,       d2,
         dim, dimnames, names;    

    SEXP sep_res = R_NilValue, 
         f_ref   = R_NilValue, f_ref_var = R_NilValue, 
                               ssb_var   = R_NilValue, 
         q       = R_NilValue, q_var     = R_NilValue,
         sel_var = R_NilValue;

   SET_SLOT(*ReturnObject, install("f.ref"),     f_ref);     //the estimate of F for the reference age in the separable model
   SET_SLOT(*ReturnObject, install("f.ref.var"), f_ref_var); //the standard errors about the reference age in F for the reference age
   SET_SLOT(*ReturnObject, install("ssb.var"),   ssb_var);   //the standard errors around the estimate of SSB in the separable periods
   SET_SLOT(*ReturnObject, install("sel.var"),   sel_var);   //the standard error of that selection pattern by age
   SET_SLOT(*ReturnObject, install("q"),         q);         //catchability
   SET_SLOT(*ReturnObject, install("q.var"),     q_var);     //catchability variance
   }

SEXP ReturnIndexRes(short iCPUE, FLTypeCPUEFlags &CPUEFlags)
   {
    int i,    j, 
        iAge, iYear;
    
    SEXP v,   d1,       d2,
         dim, dimnames, names;    

    //Create array    
    //Set dimensions of array
    PROTECT(dim = allocVector(INTSXP, 2));       
    if (CPUEFlags.Biomass[iCPUE-1])
       {
       INTEGER(dim)[0] = 1;
       INTEGER(dim)[1] = CPUE_mp_LBYEAR - CPUE_mp_FBYEAR + 1;
       }
    else
       {
       INTEGER(dim)[0] = CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]  - CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]] + 1;
       INTEGER(dim)[1] = CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]] - CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]] + 1;
       }
    
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 2));
    
    if (CPUEFlags.Biomass[iCPUE-1])
       {
       PROTECT(d1 = allocVector(STRSXP, 1));
       SET_STRING_ELT(d1, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d1 = allocVector(INTSXP, CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]-CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]] +1));
       for (iAge=CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]], i=0; iAge<=CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]; iAge++, i++)
          INTEGER(d1)[i] = iAge; 
       }

    if (CPUEFlags.Biomass[iCPUE-1])
      {    
       PROTECT(d2 = allocVector(INTSXP, CPUE_mp_LBYEAR-CPUE_mp_FBYEAR+1));
       for (iYear=CPUE_mp_FBYEAR, i=0; iYear<=CPUE_mp_LBYEAR; iYear++, i++)
          INTEGER(d2)[i] = iYear; 
       }
    else
       {
       PROTECT(d2 = allocVector(INTSXP, CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]]-CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]]+1));
       for (iYear=CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]], i=0; iYear<=CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]]; iYear++, i++)
          INTEGER(d2)[i] = iYear; 
       }


    //Create names for dimensions
    SET_VECTOR_ELT(dimnames, 0, d1);
    SET_VECTOR_ELT(dimnames, 1, d2);
    PROTECT(names = allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);

    //Set data
    if (CPUEFlags.Biomass[iCPUE-1])
       for (iYear=CPUE_mp_FBYEAR, j=0; iYear<=CPUE_mp_LBYEAR; iYear++, j++)
          REAL(v)[j] = CPUE_mp_BSURVEY[j][iCPUE-1] - RESULTS_mp_PREDBINDEX[j][iCPUE-1]; 
    else
       for (iYear=CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]], j=0; iYear<=CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]]; iYear++, j++)
          for (iAge=CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]], i=0; iAge<=CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]; iAge++, i++)
             REAL(v)[i + j*(CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]-CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]]+1)] = CPUE_mp_ASURVEY[i][j][CPUEFlags.Pos[iCPUE-1]] - RESULTS_mp_PREDAINDEX[i][j][CPUEFlags.Pos[iCPUE-1]]; //(double)iYear+((double)iAge)/10.0;
            
    //Set slot
    //SEXP Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(6);
    
    return v;
    }

SEXP ReturnIndexHat(short iCPUE, FLTypeCPUEFlags &CPUEFlags)
   {
    int i,    j, 
        iAge, iYear;
    
    SEXP v,   d1,       d2,
         dim, dimnames, names;    

    //Create array    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 2));       
    if (CPUEFlags.Biomass[iCPUE-1])
       {
       INTEGER(dim)[0] = 1;
       INTEGER(dim)[1] = CPUE_mp_LBYEAR - CPUE_mp_FBYEAR + 1;
       }
    else
       {
       INTEGER(dim)[0] = CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]  - CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]] + 1;
       INTEGER(dim)[1] = CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]] - CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]] + 1;
       }
            
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 2));
    
    if (CPUEFlags.Biomass[iCPUE-1])
       {
       PROTECT(d1 = allocVector(STRSXP, 1));
       SET_STRING_ELT(d1, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d1 = allocVector(INTSXP, CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]-CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]] +1));
       for (iAge=CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]], i=0; iAge<=CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]; iAge++, i++)
          INTEGER(d1)[i] = iAge; 
       }

   if (CPUEFlags.Biomass[iCPUE-1])
       {   
       PROTECT(d2 = allocVector(INTSXP, CPUE_mp_LBYEAR-CPUE_mp_FBYEAR+1));
       for (iYear=CPUE_mp_FBYEAR, i=0; iYear<=CPUE_mp_LBYEAR; iYear++, i++)
          INTEGER(d2)[i] = iYear; 
       }
    else
       {
       PROTECT(d2 = allocVector(INTSXP, CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]]-CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]]+1));
       for (iYear=CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]], i=0; iYear<=CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]]; iYear++, i++)
          INTEGER(d2)[i] = iYear; 
       }

     
    //Create names for dimensions
    SET_VECTOR_ELT(dimnames, 0, d1);
    SET_VECTOR_ELT(dimnames, 1, d2);
    PROTECT(names = allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);

    //Set data
    if (CPUEFlags.Biomass[iCPUE-1])
       for (iYear=CPUE_mp_FBYEAR, j=0; iYear<=CPUE_mp_LBYEAR; iYear++, j++)
          REAL(v)[j] = RESULTS_mp_PREDBINDEX[j][iCPUE-1]; 
    else
       for (iYear=CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]], j=0; iYear<=CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]]; iYear++, j++)
          for (iAge=CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]], i=0; iAge<=CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]; iAge++, i++)
             REAL(v)[i + j*(CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]-CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]]+1)] = CPUE_mp_ASURVEY[i][j][CPUEFlags.Pos[iCPUE-1]] - RESULTS_mp_PREDAINDEX[i][j][CPUEFlags.Pos[iCPUE-1]]; //(double)iYear+((double)iAge)/10.0;
            
    //Set slot
    //SEXP Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(6);
    
    return v;
    }

SEXP ReturnIndex(short iCPUE, FLTypeCPUEFlags &CPUEFlags)
   {
    int i,    j, 
        iAge, iYear;
    
    SEXP v,   d1,       d2,
         dim, dimnames, names;    

    //Create array    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 2));       
    if (CPUEFlags.Biomass[iCPUE-1])
       {
       INTEGER(dim)[0] = 1;
       INTEGER(dim)[1] = CPUE_mp_LBYEAR - CPUE_mp_FBYEAR + 1;
       }
    else
       {
       INTEGER(dim)[0] = CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]  - CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]] + 1;
       INTEGER(dim)[1] = CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]] - CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]] + 1;
       }
            
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 2));
    
    if (CPUEFlags.Biomass[iCPUE-1])
       {
       PROTECT(d1 = allocVector(STRSXP, 1));
       SET_STRING_ELT(d1, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d1 = allocVector(INTSXP, CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]-CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]] +1));
       for (iAge=CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]], i=0; iAge<=CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]; iAge++, i++)
          INTEGER(d1)[i] = iAge; 
       }

   if (CPUEFlags.Biomass[iCPUE-1])
       {   
       PROTECT(d2 = allocVector(INTSXP, CPUE_mp_LBYEAR-CPUE_mp_FBYEAR+1));
       for (iYear=CPUE_mp_FBYEAR, i=0; iYear<=CPUE_mp_LBYEAR; iYear++, i++)
          INTEGER(d2)[i] = iYear; 
       }
    else
       {
       PROTECT(d2 = allocVector(INTSXP, CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]]-CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]]+1));
       for (iYear=CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]], i=0; iYear<=CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]]; iYear++, i++)
          INTEGER(d2)[i] = iYear; 
       }

     
    //Create names for dimensions
    SET_VECTOR_ELT(dimnames, 0, d1);
    SET_VECTOR_ELT(dimnames, 1, d2);
    PROTECT(names = allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);

    //Set data
    if (CPUEFlags.Biomass[iCPUE-1])
       for (iYear=CPUE_mp_FBYEAR, j=0; iYear<=CPUE_mp_LBYEAR; iYear++, j++)
          REAL(v)[j] = CPUE_mp_BSURVEY[j][iCPUE-1]; //[CPUEFlags.Pos[iCPUE-1]]; 
    else
       for (iYear=CPUE_mp_FYEAR[CPUEFlags.Pos[iCPUE-1]], j=0; iYear<=CPUE_mp_LYEAR[CPUEFlags.Pos[iCPUE-1]]; iYear++, j++)
          for (iAge=CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]], i=0; iAge<=CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]; iAge++, i++)
             REAL(v)[i + j*(CPUE_mp_LAGE[CPUEFlags.Pos[iCPUE-1]]-CPUE_mp_FAGE[CPUEFlags.Pos[iCPUE-1]]+1)] = CPUE_mp_ASURVEY[i][j][CPUEFlags.Pos[iCPUE-1]]; //(double)iYear+((double)iAge)/10.0;
            
    //Set slot
    //SEXP Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(6);
    
    return v;
    }

bool isFLQuant(SEXP x)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(x), 0));

   return strcmp(s, "FLQuant")==0;
   }

bool isFLICA(SEXP x)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(x), 0));

   return strcmp(s, "FLICA")==0;
   }

bool isFLICAControl(SEXP x)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(x), 0));

   return strcmp(s, "FLICA.control")==0;
   }

bool isFLStock(SEXP x)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(x), 0));

   return strcmp(s, "FLStock")==0;
   }

bool isFLStocks(SEXP x)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(x), 0));

   return strcmp(s, "FLStocks")==0;
   }

bool isFLCPUE(SEXP x)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(x), 0));

   return strcmp(s, "FLCPUE")==0;
   }

bool isFLCPUEs(SEXP x)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(x), 0));

   return strcmp(s, "FLCPUEs")==0;
   }

/*
int NParam(double *Param, int *iAge, int *iYr, int *iType, int *iFleet);
   {
   int Nparm = 2*NYsep + (lastage-firstage-2)+(lastage-firstage-1) 
																    
	 If(TwoSel) Nparm =NParm+(lastage-firstage+1)-3              
																    
	 UseRecr= .False.         
	 do i = 1, NAgeix         
	   if ((fage(i) .eq. firstage) .and.(lyear(i).eq.                      &                        
	     lastyear+1) .and.       &                        
	     (ASurvey(i,lyear(i)-fyear(i)+1,1).ne.Missing))              &                        
		 UseRecr=.True.    
	 enddo                    
																	   
	 if (UseRecr) Nparm=Nparm+1       
																    
	 do index = 1, nssbix     
	   do i = 1,QBParm(index) 
	     Nparm=Nparm+1        
	   enddo ! catchability parameters
	 enddo   ! SSB indices    
																    
	 do index = 1, nageix     
	 do age = fage(index), lage(index)
	   do i =1,QAparm(index)  
	    Nparm = Nparm+1       
	   enddo ! parameters     
	 enddo ! ages             
	 enddo ! indices          
;
   }
*/