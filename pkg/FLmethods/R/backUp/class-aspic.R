
validAspic <- function(object) {
  ## Catch must be continous
  yrs<-dimnames(catch(object))$year
  
  if (!all(yrs == ac(dims(catch(object))$minyear:dims(catch(object))$maxyear)))
      return("years in catch not continous")

  # range
  dims <-dims(object)
  range<-as.list(object@range)

  return(TRUE)}


#### Convergence ###############################################################
# 1 100000   						## 0=no MC search, 1=search, 2=repeated srch; N trials #
# 1.00000e-08 					## Convergence crit. for simplex                       #  
# 3.00000e-08 6					## Convergence crit. for restarts, N restarts          #
# 1.00000e-04 0 				## Convergence crit. for estimating effort; N steps/yr #
# 8.00000 							## Maximum F allowed in estimating effort              #
################################################################################


models=c("LOGISTIC", #Schaefer
         "GENGRID",  #generalized model at grid of values or at one specified value
         "FOX",      #Fox
         "GENFIT")   #Fit the generalized model and estimate its exponent directly.

conditioning=c("YLD", #Condition fitting on yield (recommended for most analyses).
               "EFT") #Condition fitting on fishing-effort rate

objFn=c("SSE",     #Sum of squared errors (recommended default).
        "WTDSSE",  #SSE with annual data weighting
        "LAV")     #Least absolute values (robust objective function).

cpueCode=c("CE", "Fishing effort rate, catch (weight)",                  "Effort rate: annual average,Catch: annual total",
           "CC", "CPUE (weight-based), catch (weight)",                  "CPUE: annual average,Catch: annual total",
           "B0", "Estimate of biomass Effort rate: annual average",      "Start of year",
           "B1", "Estimate of biomass Catch: annual total",              "Annual average",
           "B2", "Estimate of biomass CPUE: annual average",             "End of year",
           "I0", "Index of biomass Catch: annual total",                 "Start of year",
           "I1", "Index of biomass Start of year",                       "Annual average", 
           "I2", "Index of biomass Annual average",                      "End of year")

cpueCode=t(array(cpueCode, dim=c(3,8),dimnames=list(c("code","desc","timing"),NULL)))
dimnames(cpueCode)[[1]]=cpueCode[,1]
cpueCode=transform(cpueCode[,-1],startf=c(0,0,0,0,1,0,0,1),
                                 endf  =c(1,1,0,1,1,0,1,1),
                                 ncol  =c(3,3,2,2,2,2,2,2),
                                 col2  =c("effort","index","biomass","biomass","biomass","index","index","index"),
                                 col3  =c("catch", "catch","",       "",       "",       "",     "",     ""))

optionsFit=c(search=1,trials=100000,simplex=1e-8,restarts=3e-8,nrestarts=6,effort=1e-4,nsteps=0,maxf=8.0)
options   =c(LOGISTIC,YLD,SSE) 

setClass('aspic', representation(
    "FLComp",
    model         ="character",
    catch         ='FLQuant',
    stock         ='FLQuant',
    harvest       ='FLQuant',
    ctrl          ='aspicControl',
    params        ='FLPar',
    vcov          ='array',
    hessian       ='array',
    diags         ="data.frame",
    logLik        ='numeric',
    rsdlVar       ='numeric',
    dof           ='array',
    stopmess      ="character"),
  prototype(
    range         =unlist(list(minyear=as.numeric(NA), maxyear=as.numeric(NA))),
    model         ="schaefer",
    catch         =FLQuant(),
    stock         =FLQuant(),
    harvest       =FLQuant(),
    ctrl          =aspicControl(),
    params        =FLPar(NA,dimnames=list(param=c("msy","k","b0"),iter=1))
    ),
  validity=validAspic) # }}}
  
is.aspic <- function(x)
  return(inherits(x, "aspic"))

setClass("aspics",
   representation(
      "FLlst"))

      setGeneric('aspics', function(object, ...)
    standardGeneric('aspics'))

setMethod("aspics", signature(object="aspic"), function(object, ...) {
    lst <- c(object, list(...))
    aspics(lst)})

setMethod("aspics", signature(object="missing"),
  function(...) {
    # empty
    if(missing(...)){
	  	new("aspics")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('aspics',  c(list(object=object), args))}})

setMethod("aspics", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="aspics", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(do.call('new', args))})


setGeneric("aspicControl", function(object, ...){
    value  <-  standardGeneric("aspicControl")
    return(value)})

validAspicControl <- function(object){
  return(TRUE)
  
  # Everything is fine
  return(TRUE)}

setClass("aspicControl",
  representation(
		bounds      ="array",
    conditioning="character",
    objFn       ="character",
    conv        ="numeric",  
    search      ="numeric",
    nRestart    ="numeric",                                                                       
    nSteps      ="numeric", 
    maxF        ="numeric",                                                                                            
    wt          ="numeric",
    rnd         ="numeric"),
	prototype=prototype(
		bounds      =array(NA,c(length(c(c("b0","k","msy"),paste("q",seq(1),sep=""))),5),dimnames=list(params=c(c("b0","k","msy"),paste("q",seq(1),sep="")),c("fit","min","start","max","lambda"))),
    conditioning="YLD",
    objFn       ="SSE",
    conv        =c("simplex"=1.0000E-08,"restart"=3.0000E-08,"F"=1.0000E-04),
    search      =0,                                                                       
    nRestart    =6,                                                                       
    nSteps      =0, 
    maxF        =8.0000,                                                                                            
    wt          =0,
    rnd         =6745260
  ),
	validity=validAspicControl
  )
    
        

setMethod("aspicControl", signature(object="missing"),
aspicControl.<-function(object,...){

  res=new("aspicControl")

  return(res)})

is.aspicControl <- function(x)
  return(inherits(x, "aspicControl"))
 ## control
    aspicC=function(file) {  
          inp=scan(file,sep="\n",what=character())
          ctrl=mlply(inp[5:21], function(x) {
                          tmp=strsplit(x," ")
                          tmp=unlist(tmp)[nchar(unlist(tmp))>0]})
          ctrl=llply(ctrl,as.numeric)
          ctrl=llply(ctrl,function(x) x[!is.na(x)])

        return(ctrl)}
 
setMethod("aspicControl", signature(object="character"),
aspicControl.<-function(object,...){
  
  res   =new("aspicControl")
     
  ctrl=suppressWarnings(aspicC(object))
      
  #  [8] "7  ## Number of fisheries (data series)"                                                                                           
  n     =ctrl[[8]]  
  params=FLPar("b0"=NA,"k"=NA,"msy"=NA)
  parNms=c(c("b0","k","msy"),paste("q",seq(n),sep=""))
  res@bounds=array(NA,c(length(c(c("b0","k","msy"),paste("q",seq(n),sep=""))),5),dimnames=list(params=parNms,c("fit","min","start","max","lambda")))
  
  # [10] "1.00000  ## B1/K (starting guess, usually 0 to 1)"                                                                                 
  res@bounds["b0", "start"]=ctrl[[10]][1]
  # [11] "3.0000E+04  ## MSY (starting guess)"                                                                                               
  res@bounds["msy","start"]=ctrl[[11]]
  # [12] "2.6700E+05  ## K (carrying capacity) (starting guess)"                                                                             
  res@bounds["k", "start"]=ctrl[[12]]
  # [13] "2.1126E-06  6.0195E-06  9.7627E-06  1.4944E-04  2.9980E-06  4.2138E-04  8.3406E-04    ## q (starting guesses -- 1 per data series)"
  res@bounds[parNms[-(1:3)],"start"]=ctrl[[13]][1:n]

  # [14] "0  1  1  1  1  1  1  1  1  1    ## Estimate flags (0 or 1) (B1/K,MSY,K,q1...qn)"                                                   
  res@bounds[,"fit"]=ctrl[[14]]

  # [15] "1.0000E+02  1.0000E+07  ## Min and max constraints -- MSY"                                                                         
  res@bounds["msy",c("min","max")]=ctrl[[15]]
  # [16] "1.0000E+04  2.0000E+07  ## Min and max constraints -- K"                                                                           
  res@bounds["k",  c("min","max")]=ctrl[[16]]
  
  res@bounds[parNms[-(1:3)],"min"]=res@bounds[parNms[-(1:3)],"start"]*0.01  
  res@bounds[parNms[-(1:3)],"max"]=res@bounds[parNms[-(1:3)],"start"]*100  
  res@bounds["b0","min"]=0.01  
  res@bounds["b0","max"]=1  
    
  #  [9] "1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E-02  1.0000E+00  1.0000E-02    ## Statistical weights for data series"      
  if (length(ctrl[[9]])==0)
    res@bounds[parNms[-(1:3)],"lambda"][]=1.0 else
    res@bounds[parNms[-(1:3)],"lambda"]=ctrl[[9]]
   
  # [17] "6745260  ## Random number seed
  res@rnd   =ctrl[[17]]
    
  #  [3] "1.0000E-08  ## Convergence crit. for simplex"  
  if (length(ctrl[[3]])==0) ctrl[[3]]=1.0-08  
  res@conv["simplex"]=ctrl[[3]]
    
  #  [4] "3.0000E-08  6  ## Convergence crit. for restarts, N restarts"                                                                      
  res@conv["restart"]=ctrl[[4]][1]
  res@nRestart=ctrl[[4]][1]
 
  #  [5] "1.0000E-04  0  ## Conv. crit. for F; N steps/yr for gen. model"                                                                    
  res@conv["F"]=ctrl[[5]][1]
  res@nSteps   =ctrl[[5]][1]
    
  #  [6] "8.0000  ## Maximum F when cond. on yield"                                                                                          
  res@maxF    =ctrl[[6]]
  #  [7] "0.0  ## Stat weight for B1>K as residual (usually 0 or 1)" 
  res@wt      =ctrl[[7]][1]
    
  return(res)})
    

cpueCode=c("CE", "Fishing effort rate, catch (weight)",                  "Effort rate: annual average,Catch: annual total",
           "CC", "CPUE (weight-based), catch (weight)",                  "CPUE: annual average,Catch: annual total",
           "B0", "Estimate of biomass Effort rate: annual average",      "Start of year",
           "B1", "Estimate of biomass Catch: annual total",              "Annual average",
           "B2", "Estimate of biomass CPUE: annual average",             "End of year",
           "I0", "Index of biomass Catch: annual total",                 "Start of year",
           "I1", "Index of biomass Start of year",                       "Annual average", 
           "I2", "Index of biomass Annual average",                      "End of year")

cpueCode=t(array(cpueCode, dim=c(3,8),dimnames=list(c("code","desc","timing"),NULL)))
dimnames(cpueCode)[[1]]=cpueCode[,1]
cpueCode=transform(cpueCode[,-1],startf=c(0,0,0,0,1,0,0,1),
                                 endf  =c(1,1,0,1,1,0,1,1),
                                 ncol  =c(3,3,2,2,2,2,2,2),
                                 col2  =c("effort","index","biomass","biomass","biomass","index","index","index"),
                                 col3  =c("catch", "catch","",       "",       "",       "",     "",     ""))

aspicCpue=function(file){
                   
  uN=scan(file,sep="\n",what=character())[22]
  uN=substr(uN,1,regexpr("#",uN)[1]-1)  
  uN=as.numeric(strsplit(uN," ")[[1]])
  uN=uN[!is.na(uN)]
  
  #uN=gsub("(\\s+)", " ", uN, perl=TRUE)
  #uN=strsplit(uN," ")[[1]]
  #uN=as.numeric(uN)
  
  us=scan(file,sep="\n",what=character())[-(1:22)]
  
  pos=cumsum(c(rbind(1,1,uN)))
  
  nms=us[pos[seq(1,length(pos),3)]]
  cde=us[pos[seq(2,length(pos),3)]]
  cde=gsub("(\\s+)", "", cde, perl=TRUE)
  
  pos=mlply(data.frame(from=pos[seq(2,length(pos),3)]+1,
                       to  =pos[seq(3,length(pos),3)]),seq)
  
  
  cpue=llply(pos,function(x) us[x])
  
  cpueFn=function(nm,dat,cd){
          
        res=mdply(data.frame(x=dat), function(x)  as.numeric(strsplit(gsub("\\s+"," ", x, perl=T)," ")[[1]]))[,-1]
        
        res[res<0]=NA
          
        names(res)[1]="year"
        
        if (cpueCode[cd,"ncol"]>=2)
          names(res)[2]=ac(cpueCode[cd,"col2"])
       
        if (cpueCode[cd,"ncol"]>=3)
          names(res)[3]=ac(cpueCode[cd,"col3"])
         
        idx= FLIndex(index=FLQuant(NA,dimnames=list(year=res$year)),
                     name =nm,
                     desc =ac(cpueCode[cd,"desc"]),
                     type =cd,
                     range=c(minyear=min(res$year),maxyear=max(res$year),unlist(c(cpueCode[cd,c("startf","endf")])))
                     )
        
        if (cpueCode[cd,"col2"]=="index")
          index(idx)[]=res[,2]
        
        if (cpueCode[cd,"col2"]=="biomass")
          index(idx)[]=res[,2]
        
        if (cpueCode[cd,"col2"]=="effort")
          index(idx)[]=res[,2]/res[,3]
       
        if (cpueCode[cd,"col3"]=="catch")
          catch.n(idx)[]=res[,3]
       
        return(idx)}
  
  res=FLIndices(mlply(seq(length(nms)), function(x,nms,cpue,cds) {print(x); cpueFn(nms[x],cpue[[x]],cde[x])}, nms=nms, cpue=cpue,cds=cde))
  
  return(res)}
  
writeAspicInp<-function(ctl,idxs,fl="/home/lkell/tmp/ass.txt"){
    
    cat("FIT"                                           ,"\n",file=fl,append=FALSE)
    cat("FLR generated"                                 ,"\n",file=fl,append=TRUE)
    cat("LOGISTIC", ctl@conditioning, ctl@objFn         ,"\n",file=fl,append=TRUE)
    cat("102"                                           ,"\n",file=fl,append=TRUE)
    cat("1"                                             ,"\n",file=fl,append=TRUE)
    cat(ctl@search,ctl@nSteps                           ,"\n",file=fl,append=TRUE)
    cat(ctl@conv                                        ,"\n",file=fl,append=TRUE)
    cat(ctl@nRestart                                    ,"\n",file=fl,append=TRUE)
    cat(ctl@nSteps                                      ,"\n",file=fl,append=TRUE)
    cat(ctl@maxF                                        ,"\n",file=fl,append=TRUE)
    cat(0                                               ,"\n",file=fl,append=TRUE)
    cat(dim(ctl@bounds)[1]-3                            ,"\n",file=fl,append=TRUE)
    cat(ctl@bounds[-(1:3),"lambda"]                     ,"\n",file=fl,append=TRUE)
    cat(ctl@bounds["b0",  "start"]                      ,"\n",file=fl,append=TRUE)
    cat(ctl@bounds["msy", "start"]                      ,"\n",file=fl,append=TRUE)
    cat(ctl@bounds["k",   "start"]                      ,"\n",file=fl,append=TRUE)
    cat(ctl@bounds[-(1:3),"start"]                      ,"\n",file=fl,append=TRUE)
    cat(ctl@bounds[      ,"fit"]                        ,"\n",file=fl,append=TRUE)
    cat(ctl@bounds["msy",c("min","max")]                ,"\n",file=fl,append=TRUE)
    cat(ctl@bounds["k",  c("min","max")]                ,"\n",file=fl,append=TRUE)
    cat(ctl@rnd                                         ,"\n",file=fl,append=TRUE)
    cat(laply(idxs, function(x) dims(x)$year)           ,"\n",file=fl,append=TRUE)
    
    l_ply(idxs, 
        function(idx){
            cat(name(idx)                                       ,"\n",file=fl,append=TRUE)
            cat(type(idx)                                       ,"\n",file=fl,append=TRUE)
            
            mm=model.frame(FLQuants(col2=index(idx),col3=catch.n(idx)),drop=T)
            mm[is.na(mm)]=-1
                 
            cat(paste(t(apply(as.matrix(mm),1,paste,collapse=" ")),"\n"),file=fl,append=TRUE)})

        
    return()}
