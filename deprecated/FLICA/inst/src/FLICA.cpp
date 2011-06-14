   //D:\FLR\Packages\FLICA\inst\libs\FLICA.dll
   //C:\Program Files\R\R-2.4.1\library\FLICA\libs\FLICA.dll

#define __MSVC32__
   
#include "flica.hpp"


extern "C" VOIDDLLExport ExecuteCallback(long cbAddress) 
   {
   // Declare the function pointer, with one string argument.
   typedef void (__stdcall *FUNCPTR)(BSTR pbstr);
   FUNCPTR vbFunc;

   // Point the function pointer at the passed-in address.
   vbFunc = (FUNCPTR)cbAddress;

   // Call the function through the function pointer.
   vbFunc(SysAllocString(L"Hi// This messAGE came from your DLL//"));
   }

extern "C" SEXPDLLExport FLVersion(void)
   {
   char szBuf[255];

   wsprintf((LPSTR)szBuf, " FLRICA.DLL version 0.5-1 built on %s at %s", __DATE__, __TIME__);

   SEXP ReturnObject = R_NilValue;

   PROTECT(ReturnObject = NEW_CHARACTER(1));
   SET_STRING_ELT(ReturnObject, 0, COPY_TO_USER_STRING(szBuf));

   UNPROTECT(1);

   return ReturnObject;
   }

extern "C" SEXPDLLExport FLICA(SEXP xStock, SEXP xCPUE, SEXP xControl) 
   {
   SEXP ReturnObject = R_NilValue;
   FLTypeCPUEFlags CPUEFlags;

   //Function mixes file text file IO (for checking) with R IO
   //THe intention is to replace Test IO with R
         
   //initialises data and parameters

   ICA_INIT();
   
   // Flag to indicate whether DLL if true turns off cosole IO
   CONTROL_mp_DLLFLAG = true; 
   
   //inputs stock data from text files
//   ICA_STOCK_INPUT();
   //input stock data from R 
   if (!InputStock(xStock)) return ReturnObject;   

   //inputs fleet data from text files
//   ICA_CPUE_INPUT();
   //input CPUE data from R 
   if (!InputCPUE(xCPUE,xControl,CPUEFlags)) return ReturnObject;   

   //sets options etc from text file   
//   ICA_CONTROL_INPUT();
   //Input Control from R
   if (!InputControl(xControl,CPUEFlags)) ReturnObject;

//  ICAWRAPPER();
   
   //runs ICA
   ICA_RUN(); 


   //Outputs text file summaries
//  ICA_OUTPUT();

  ReturnObject = CreateFLICA(xControl, CPUEFlags);

   return ReturnObject; 
   }


/*
     
!!Separable model : F by year                                                     
 params(1)= log(0.3392) 
 params(2)= log(0.2882) 
 params(3)= log(0.2968) 
 params(4)= log(0.3251) 
 params(5)= log(0.4037) 
!Separable Model: Selection (S) by age                                           
 params(6)= log(0.1309)
 params(7)= log(0.2280)
 params(8)= log(0.4403)
 params(9)= log(0.7063)
 params(10)= 1.0678
!!Separable)= model: Populations in year 2005                                    
 params(11)= log(21651873.0)
 params(12)= log(7665396.0)
 params(13)= log(2323541.0)
 params(14)= log(2537549.0)
 params(15)= log(4646459.0)
 params(16)= log(1357972.0)
 params(17)= log(1479248.0)
!!Separable)= model: Populations at age 
 params(18)= log(318904.0)
 params(19)= log(666903.0)
 params(20)= log(398034.0)
 params(21)= log(611431.0)
!!Recruitment in year 2006                                                     
 params(22)= log(26871244.0)
!!SSB Index)= catchabilities  MLAI                                  
!!Power model fitted. Slopes (Q) and logonents (K) at age                         
 params(23)= log(2.768   )  
 params(24)= log(7601E-04) 
!Age-structured index catchabilities    Acoustic survey 1-9+ wr                 
!Linear model fitted. Slopes at age :                                            
 params(25)= log(1.112)    
 params(26)= log(1.552)    
 params(27)= log(1.845)    
 params(28)= log(1.949)    
 params(29)= log(2.177)    
 params(30)= log(2.599)    
 params(31)= log(3.079)    
!!IBTS1: 1-)=5+ wr                          
!!Linear model fitted. Slopes at age :                                            
 params(32)= log(.1594E-03)
 params(33)= log(.1765E-03)
 params(34)= log(.1298E-03)
 params(35)= log(.7725E-04)
 params(36)= log(.4855E-04)
!!MIK 0-wr                  
!!Linear model fitted. Slopes at age :                                            
 params(37)= log(.3528E-05)
!Parameters of the stock-recruit relationship                                    
 params(38)= log(.5850E+08)
 params(39)= log(.3270E+06)


*/