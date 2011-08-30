bool isFLIndex(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLIndex")==0;
   }

FLIndex::FLIndex(void)      
    {
    InitFlag = false;
    }

FLIndex::FLIndex(SEXP x)      
    {
    InitFlag = false;

    if (isFLIndex(x) && !InitFlag)
       Init(x);
    }
   
void FLIndex::Init(SEXP x)
   {
   minquant   = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant   = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp    = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr      = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr      = (int)REAL(GET_SLOT(x, install("range")))[4];
   start      = (int)REAL(GET_SLOT(x, install("range")))[5];
   end        = (int)REAL(GET_SLOT(x, install("range")))[6];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   niters   = 1;

   index.Init(    0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);
   index_var.Init(0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);
   index.Init(    GET_SLOT(x, install("index"))); 
   index_var.Init(GET_SLOT(x, install("index"))); 
   
   //need to check seasons, areas, units & iterations
   }

FLIndex::~FLIndex(void)      
   {
   //unalloc();
   }                               

SEXP FLIndex::Return(void)
   {
   SEXP Index, Range;

   PROTECT(Index  = NEW_OBJECT(MAKE_CLASS("FLIndex")));
   Range          = PROTECT(NEW_NUMERIC(7)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   REAL(Range)[5] = start;
   REAL(Range)[6] = end;
   
   SET_SLOT(Index, install("range"),      Range);
   SET_SLOT(Index, install("index"),      index.Return());
   SET_SLOT(Index, install("index.var"),  index_var.Return());
      
   UNPROTECT(2);

   return Index;
   }
