/** $Id$ **/
#include "FLCoreClasses_pointer.hpp"

FLQuant_pointer::FLQuant_pointer(void)      
    {
    InitFlag() = false;
    }

int FLQuant_pointer::i(int _age,int _yr,int _unit, int _season, int _area, int _iter) 
   { 
   int _miniter = 1;

   return (((((_iter-_miniter)*nareas()+_area-1)*nseasons()+_season-1)*nunits()+_unit-1)*(maxyr()-minyr()+1)+_yr-minyr())*(maxquant()-minquant()+1)+_age-minquant();
   } 

double& FLQuant_pointer::operator()(int _age,int _yr,int _unit, int _season, int _area, int _iter) 
   { 
   if (!InitFlag()      || 
       _age <minquant() || _age >maxquant()   || 
       _yr  <minyr()    || _yr  >maxyr()      || 
       _unit<1          || _unit>nunits()     || 
       _season<1        || _season>nseasons() || 
       _area<1          || _area>nareas()) 
      return outofbounds_double;
   else
      return (data)[i(_age,_yr,_unit,_season,_area,_iter)];
   } 

FLQuant_pointer::FLQuant_pointer(SEXP x)  
    {
    if (isFLQuant(x))
       Init(x);
    }

void FLQuant_pointer::Init(SEXP x)      
    {
    SEXP Quant    = GET_SLOT(x, install(".Data")),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    data          = NUMERIC_POINTER(AS_NUMERIC(Quant));

    int dim[6], n = length(dims);

    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];
    dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 
      
    if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
        ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || ((int)dim[4]) < 1 || ((int)dim[5]) < 1)
      {
      UNPROTECT(1);

      return;
      }

    minquant() = 0;
    minyr()    = 0;
    maxquant() = (int)dim[0] -1;
    maxyr()    = (int)dim[1] -1;
    nunits()   = (int)dim[2];
    nseasons() = (int)dim[3];
    nareas()   = (int)dim[4]; 
    niters()   = (int)dim[5];
	   
      
    if (dimnames != R_NilValue) 
      if (TYPEOF(dimnames) == VECSXP) 
         {
         int  t = 0;
         const char *c;
         
         if (n >= 1 && INTEGER(dims)[0] >= 1) 
            {
            c = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 0), 0));

            //check that name is not a text string
            for (int i=0; i<=(signed)strlen(c); i++)
               if (isalpha(c[i])) t=1;

            if (t !=1)
	            t = atoi(c); 

            minquant() += t;
            maxquant() += t;
  	         }
		   
         if (n >= 2 && INTEGER(dims)[1] >= 1) 
            {
            t = 0;
            c = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 1), 0));

            //check that name is not a text string
            for (int i=0; i<=(signed)strlen(c); i++)
               if (isalpha(c[i])) t=1;

            if (t !=1)
	            t = atoi(c); 
            
            minyr()   += t;
            maxyr()   += t;
 	      	}
		   }

   InitFlag() = true;

   UNPROTECT(1);
   }

FLQuant_pointer::~FLQuant_pointer(void)      
   {
   ;
   }                               

SEXP FLQuant_pointer::Return(void)      
    {
    SEXP Quant, v, 
         d1, d2, d3, d4, d5, d6, 
         dim, dimnames, names;    

    int j, iAge, iYear, iUnit, iArea, iSeason, iIter;

    //Create new S4 object    

    PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 6));       
    INTEGER(dim)[0] = maxquant()-minquant() +1;
    INTEGER(dim)[1] = maxyr()   -minyr()    +1;
    INTEGER(dim)[2] = nunits(); 
    INTEGER(dim)[3] = nseasons(); 
    INTEGER(dim)[4] = nareas();
    INTEGER(dim)[5] = niters();
        
    //allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 6));
    
    PROTECT(d1 = allocVector(INTSXP, maxquant()-minquant() +1));
    for (iAge=minquant(),j=0; iAge<=maxquant(); iAge++, j++)
        INTEGER(d1)[j] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, maxyr()-minyr()+1));
    for (iYear=minyr(), j=0; iYear<=maxyr(); iYear++, j++)
        INTEGER(d2)[j] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
     
    if (nunits()==1)
       {
       PROTECT(d3 = allocVector(STRSXP, nunits()));
       SET_STRING_ELT(d3, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d3 = allocVector(INTSXP, nunits()));
       for (iUnit=1, j=0; iUnit<=nunits(); iUnit++, j++)
          INTEGER(d3)[j] = iUnit; 
       }
    SET_VECTOR_ELT(dimnames, 2, d3);
       
    if (nseasons()==1)
       {
       PROTECT(d4 = allocVector(STRSXP, nseasons()));
       SET_STRING_ELT(d4, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d4 = allocVector(INTSXP, nseasons()));
       for (iSeason=1, j=0; iSeason<=nseasons(); iSeason++, j++)
          INTEGER(d4)[j] = iSeason; 
       }
    SET_VECTOR_ELT(dimnames, 3, d4);
    

    if (nareas()==1)
       {
       PROTECT(d5 = allocVector(STRSXP, nareas()));
       SET_STRING_ELT(d5, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d5 = allocVector(INTSXP, nareas()));
       for (iArea=1, j=0; iArea<=nareas(); iArea++, j++)
          INTEGER(d5)[j] = iArea; 
       }
    SET_VECTOR_ELT(dimnames, 4, d5);

    PROTECT(d6 = allocVector(INTSXP, niters()));
    for (iIter=1, j=0; iIter<=niters(); iIter++, j++)
        INTEGER(d6)[j] = iIter; 
    SET_VECTOR_ELT(dimnames, 5, d6);
    
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
    SET_STRING_ELT(names, 5, mkChar("iter")); 

    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
   
    //Set data
    j=0;
    for(iIter = 1; iIter <= niters(); iIter++)
	    for (iArea = 1; iArea <= nareas(); iArea++)
	  	    for (iSeason = 1; iSeason <= nseasons(); iSeason++)
     		    for (iUnit = 1; iUnit <= nunits(); iUnit++)
	    		    for (iYear = minyr(); iYear <= maxyr(); iYear++)
			 		    for (iAge = minquant(); iAge <= maxquant(); iAge++)
			      			    REAL(v)[j++] = data[i(iAge,iYear,iUnit,iSeason,iArea,iIter)]; 
                   
    //Set slot
    Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(11);
    
    return Quant;
    }

FLStock_pointer::FLStock_pointer(void)      
    {
    InitFlag = false;
    }

FLStock_pointer::FLStock_pointer(SEXP x)      
    {
    InitFlag = false;

    if (isFLStock(x) && !InitFlag)
       Init(x);
    }
   
void FLStock_pointer::Init(SEXP x)
   {
   minquant = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp  = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr    = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr    = (int)REAL(GET_SLOT(x, install("range")))[4];
 
   minfbar  = minquant; 
   maxfbar  = maxquant; 
   
   if (LENGTH(GET_SLOT(x, install("range"))) >= 6)
      minfbar  = (int)REAL(GET_SLOT(x, install("range")))[5];
   if (LENGTH(GET_SLOT(x, install("range"))) >= 7)
      maxfbar  = (int)REAL(GET_SLOT(x, install("range")))[6];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;

   catch_.Init(      GET_SLOT(x, install("catch"))); 
   catch_n.Init(     GET_SLOT(x, install("catch.n"))); 
   catch_wt.Init(    GET_SLOT(x, install("catch.wt"))); 
   discards.Init(    GET_SLOT(x, install("discards"))); 
   discards_n.Init(  GET_SLOT(x, install("discards.n"))); 
   discards_wt.Init( GET_SLOT(x, install("discards.wt"))); 
   landings.Init(    GET_SLOT(x, install("landings"))); 
   landings_n.Init(  GET_SLOT(x, install("landings.n"))); 
   landings_wt.Init( GET_SLOT(x, install("landings.wt"))); 
   stock.Init(       GET_SLOT(x, install("stock"))); 
   stock_n.Init(     GET_SLOT(x, install("stock.n"))); 
   stock_wt.Init(    GET_SLOT(x, install("stock.wt"))); 
   m.Init(           GET_SLOT(x, install("m"))); 
   mat.Init(         GET_SLOT(x, install("mat"))); 
   harvest.Init(     GET_SLOT(x, install("harvest"))); 
   harvest_spwn.Init(GET_SLOT(x, install("harvest.spwn"))); 
   m_spwn.Init(      GET_SLOT(x, install("m.spwn"))); 

   niters   = m.niters();
   
   //need to check seasons, areas & units
   }

double FLStock_pointer::computeStock(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      val += stock_n( iage, iyr, iunit, iseason, iarea, iter)*
             stock_wt(iage, iyr, iunit, iseason, iarea, iter);

   return val;
   }                               

double FLStock_pointer::computeMnSz(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0,
          num = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
      val += stock_n( iage, iyr, iunit, iseason, iarea, iter)*
             stock_wt(iage, iyr, iunit, iseason, iarea, iter);
      num += stock_n( iage, iyr, iunit, iseason, iarea, iter);
      }
   
   return val/num;
   }                               

double FLStock_pointer::SSB(int iyr, int iunit, int iseason, int iarea, int iter) 
   {
   double val = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
      double deaded = exp(-m(      iage, iyr, iunit, iseason, iarea, iter)*m_spwn(      iage, iyr, iunit, iseason, iarea, iter)
                          -harvest(iage, iyr, iunit, iseason, iarea, iter)*harvest_spwn(iage, iyr, iunit, iseason, iarea, iter));
                    
      val += stock_n( iage, iyr, iunit, iseason, iarea, iter)*
             stock_wt(iage, iyr, iunit, iseason, iarea, iter)*
             mat(     iage, iyr, iunit, iseason, iarea, iter)*deaded;
      }

   return val;
   }  
                             
double FLStock_pointer::computeCatch(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
      val += catch_n( iage, iyr, iunit, iseason, iarea, iter)*
             catch_wt(iage, iyr, iunit, iseason, iarea, iter);
      }

   return val;
   } 

double FLStock_pointer::computeLandings(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
      double z = m(iage, iyr, iunit, iseason, iarea, iter) + harvest(iage, iyr, iunit, iseason, iarea, iter);

      val += landings_n( iage, iyr, iunit, iseason, iarea, iter)/
             catch_n(    iage, iyr, iunit, iseason, iarea, iter)*
             stock_n(    iage, iyr, iunit, iseason, iarea, iter)*
             harvest(    iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z))*
             landings_wt(iage, iyr, iunit, iseason, iarea, iter);
      }

   return val;
   } 

double FLStock_pointer::computeDiscards(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
      double z = m(iage, iyr, iunit, iseason, iarea, iter) + harvest(iage, iyr, iunit, iseason, iarea, iter);

      val += discards_n( iage, iyr, iunit, iseason, iarea, iter)/
             catch_n(    iage, iyr, iunit, iseason, iarea, iter)*
             stock_n(    iage, iyr, iunit, iseason, iarea, iter)*
             harvest(    iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z))*
             discards_wt(iage, iyr, iunit, iseason, iarea, iter);
      }

   return val;
   } 

double FLStock_pointer::Fbar(int iyr, int iUnit, int iSeason, int iArea, int iIter)
   {
	double val=0.0;

	for (int iAge = minfbar; iAge<= maxfbar;iAge++)
		val += harvest(iAge, iyr, iUnit,iSeason,iArea,iIter);

	return (val / (maxfbar-minfbar+1));
   }

double FLStock_pointer::Zbar(int iyr, int iUnit, int iSeason, int iArea, int iIter)
   {
	double val=0.0;

	for (int iAge = minfbar; iAge<= maxfbar;iAge++)
		val += harvest(iAge, iyr, iUnit,iSeason,iArea,iIter) +
             m(      iAge, iyr, iUnit,iSeason,iArea,iIter);

	return (val / (maxfbar-minfbar+1));
   }

double FLStock_pointer::FbarLandings(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minfbar; iage<=maxfbar; iage++)
      val += harvest(   iage, iyr, iunit, iseason, iarea, iter)*
             landings_n(iage, iyr, iunit, iseason, iarea, iter)/
             catch_n(   iage, iyr, iunit, iseason, iarea, iter);

   return val/(maxfbar-minfbar+1);
   }                               


double FLStock_pointer::FbarDiscards(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minfbar; iage<=maxfbar; iage++)
      val += harvest(   iage, iyr, iunit, iseason, iarea, iter)*
             discards_n(iage, iyr, iunit, iseason, iarea, iter)/
             catch_n(   iage, iyr, iunit, iseason, iarea, iter);

   return val/(maxfbar-minfbar+1);
   }   


FLStock_pointer::~FLStock_pointer(void)      
   {
   ; //unalloc();
   }                               

SEXP FLStock_pointer::ReturnRange(void)
   {
   SEXP Range;

   Range          = PROTECT(NEW_NUMERIC(7)); 
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   REAL(Range)[5] = minfbar;
   REAL(Range)[6] = maxfbar;

   SEXP names;
    
   PROTECT(names = allocVector(STRSXP, 7));
   SET_STRING_ELT(names, 0, mkChar("min"));
   SET_STRING_ELT(names, 1, mkChar("max"));
   SET_STRING_ELT(names, 2, mkChar("plusgroup"));
   SET_STRING_ELT(names, 3, mkChar("minyear"));
   SET_STRING_ELT(names, 4, mkChar("maxyear"));
   SET_STRING_ELT(names, 5, mkChar("minfbar"));
   SET_STRING_ELT(names, 6, mkChar("maxfbar"));
   setAttrib(Range, R_NamesSymbol, names);
   
   return Range;
   }
         
SEXP FLStock_pointer::Return(void)
   {
   SEXP Stock, Range;

   PROTECT(Stock  = NEW_OBJECT(MAKE_CLASS("FLStock")));
   Range          = ReturnRange(); 

   SET_SLOT(Stock, install("catch"),       catch_.Return());
   SET_SLOT(Stock, install("catch.n"),     catch_n.Return());
   SET_SLOT(Stock, install("catch.wt"),    catch_wt.Return());
   SET_SLOT(Stock, install("discards"),    discards.Return());
   SET_SLOT(Stock, install("discards.n"),  discards_n.Return());
   SET_SLOT(Stock, install("discards.wt"), discards_wt.Return());
   SET_SLOT(Stock, install("landings"),    landings.Return());
   SET_SLOT(Stock, install("landings.n"),  landings_n.Return());
   SET_SLOT(Stock, install("landings.wt"), landings_wt.Return());
   SET_SLOT(Stock, install("stock"),       stock.Return());
   SET_SLOT(Stock, install("stock.n"),     stock_n.Return());
   SET_SLOT(Stock, install("stock.wt"),    stock_wt.Return());
   SET_SLOT(Stock, install("mat"),         mat.Return());
   SET_SLOT(Stock, install("harvest"),     harvest.Return()); 
   SET_SLOT(Stock, install("harvest.spwn"),harvest_spwn.Return());
   SET_SLOT(Stock, install("m"),           m.Return()); 
   SET_SLOT(Stock, install("m.spwn"),      m_spwn.Return());
   SET_SLOT(Stock, install("range"),       Range);
      
   UNPROTECT(3);

   return Stock;
   }
