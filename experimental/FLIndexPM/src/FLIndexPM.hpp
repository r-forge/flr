
class FLIndex 
{
public:        
   FLIndex(void);      
   FLIndex(SEXP);      
  ~FLIndex(void);      

   void Init(SEXP);      
   SEXP Return(void);      

   FLQuant index_var,
           index,
           catch_n,
           catch_wt,
           effort,
           sel_pattern,
           index_q;   

protected: 
   bool InitFlag;

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters;

   double start, end;
   void alloc(void);      
   };                  

