#include "fwd.hpp"

double calcSP(FLRConstBD model, double stock, double *params)
   {
   double sp=0.0;

   switch(model) {
      case FLRConst_BDPellaT:
  	     sp = params[0]*stock;
		     break;
		  }

   if (sp<0) sp=1e-25;

   return sp;}

// 
// extern "C" SEXPDLLExport SP(SEXP xModel, SEXP xPar, SEXP xStock, SEXP xCatch, SEXP xHarvest, SEXP xPE, SEXP xMult)
//    {
//    SEXP RtnVal = R_NilValue;
// 
//    FLQuant catch_(xCatch);
//    FLQuant catch_(xHarvest);
//    FLQuant catch_(xStock);
//    FLQuant catch_(xPE);
//    FLQuant par(xPar);
// 
// 
//    FLRConstBD *model;
// 
//    if (!isInteger(xModel)){
//       UNPROTECT(2);
//       return RtnVal;}
// 
//    // Check that we have a model specified for each year & stock
//    if (LENGTH(xModel)<(ssb.maxyr()-ssb.minyr()+1)){
//       UNPROTECT(3);
//       return RtnVal;}
// 
//    int *dmodel = INTEGER_POINTER(xModel);
//    model = new FLRConstSRR[1] - 1;
// 
//    model[1]=(FLRConstSRR)((int)dmodel[0]);
// 
//    // Recruits as a function of SSB
//    for (int iIter = 1; iIter<=ssb.niters(); iIter++)
//      for (int iArea = 1; iArea <= ssb.nareas(); iArea++)
//       for (int iSeason = 1; iSeason <= ssb.nseasons(); iSeason++)
//  		    for (int iUnit = 1; iUnit <= ssb.nunits(); iUnit++)
// 				  for (int iYr =ssb.minyr(); iYr<=ssb.maxyr(); iYr++){
// 
//        	    //para [0] = a, para[1] = b, ... para[n] = rho
// 	          int nPar = par.maxquant()-par.minquant() + 1;
// 				    double* para = new double[nPar];
// 
//             for (int i=1; i<=nPar;i++)
//  	            para[i-1] = par(i,1,iUnit,iSeason,iArea,iIter);
// 
//             double ssb_=ssb(1,iYr,iUnit,iSeason,iArea,iIter);
//   				  double rec_=srr((FLRConstSRR)model[1], ssb_, para);
// 
//             rec(1,iYr,iUnit,iSeason,iArea,iIter)=rec_;}
// 
//     delete [] (model+1);
// 
//     return rec.Return();}

//extern "C" SEXPDLLExport fwdFLBioDym(SEXP xModel, SEXP xParams, SEXP xCatch, SEXP xHarvest, SEXP xPE, SEXP xMult){
//   SEXP biomass = R_NilValue;
//   
//   FLQuant  _catch(xCatch);
//   FLQuant  harvest(xHarvest);
//   FLQuant  PE(xPE);
//
//   return biomass;}

// setMethod("fwd", signature(object="FLBioDym", fleets = "missing"),
//  function(object, catch=NULL, harvest=NULL, pe=NULL, peMult=TRUE) {
//
//    ## catch or harvest?
//    ctcNull=is.null(catch) 
//    if(ctcNull & is.null(harvest))
//      stop("must supply catch or harvest")
//      
//    ## check year range
//    if (!ctcNull) {
//      if (!(all(dimnames(catch)$year %in% dimnames(catch(object))$year)))
//        stop("years in catch & stock dont match")
//      catch(object)[,dimnames(catch)$year] <- catch
//      yrs <- dimnames(catch)$year
//    } else {
//      if (!(all(dimnames(harvest)$year %in% dimnames(catch(object))$year)))
//        stop("years in harvest & stock dont match")
//      yrs <- dimnames(harvest)$year
//    }
//
//      ## B0 in year 1?
//      if (as.numeric(yrs[1]) == range(object,"minyear"))
//        stock(object)[,ac(range(object,"minyear"))] <-
//          params(object)["K"] * params(object)["b0"]
//
//      ## maxyear
//      if (max(as.numeric(yrs)) == range(object,"maxyear"))
//        stock(object) <- window(stock(object),end=range(object,"maxyear")+1)
//
//     nits=max(dims(object)$iter,dims(harvest)$iter)
//     if (nits>1){ 
//               catch(object)=propagate(catch(object),nits)
//               stock(object)=propagate(stock(object),nits)
//               params(object)=propagate(params(object),nits)}   
//    
//      for(y in as.numeric(yrs)) {
//        if (ctcNull)
//           catch(object)[,ac(y)] <- stock(object)[,ac(y)]*harvest[,ac(y)]
//        
//         if (!is.null(ps)) {
//             if (peMult) sp.=sp(object)[, ac(y)]*pe[, ac(y)] 
//             else        sp.=sp(object)[, ac(y)]+pe[, ac(y)]
//         }else sp.=sp(object)[, ac(y)]
//         stock(object)[,ac(y+1)] <- stock(object)[,ac(y)] - catch(object)[,ac(y)] + sp.
//         }
//
//    stock(object)[stock(object) < 0] = 0
//
//    return(object)
//  }

