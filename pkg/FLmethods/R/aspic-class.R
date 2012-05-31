#### Convergence ###############################################################
# 1 100000     					## 0=no MC search, 1=search, 2=repeated srch; N trials #
# 1.00000e-08 					## Convergence crit. for simplex                       #  
# 3.00000e-08 6					## Convergence crit. for restarts, N restarts          #
# 1.00000e-04 0 				## Convergence crit. for estimating effort; N steps/yr #
# 8.00000 							## Maximum F allowed in estimating effort              #
################################################################################

model=c("LOGISTIC", #Schaefer
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


validAspic <- function(object) {
  ## Catch must be continous
  yrs<-dimnames(catch(object))$year
  
  if (!all(yrs == ac(dims(catch(object))$minyear:dims(catch(object))$maxyear)))
      return("years in catch not continous")

  # range
  dims <-dims(object)
  range<-as.list(object@range)

  return(TRUE)}

setClass('aspic', representation(
    "FLComp",
    model         ="factor",
    objFn         ="factor",
    conditioning  ="factor",
    options       ="numeric",
    
    catch         ='FLQuant',
    stock         ='FLQuant',
    
    params        ='FLPar',
    vcov          ='FLPar',
    hessian       ='FLPar',
    dof           ='numeric',
    stopmess      ="character"),
  prototype(
    range         =unlist(list(minyear=as.numeric(NA),   maxyear=as.numeric(NA))),
    model         =factor("LOGISTIC",levels=model,       labels=model),
    objFn         =factor("SSE",     levels=objFn,       labels=objFn),
    conditioning  =factor("YLD",     levels=conditioning,labels=conditioning),
    options       =c(search=1,trials=100000,simplex=1e-8,restarts=3e-8,nrestarts=6,effort=1e-4,nsteps=0,maxf=8.0),
   
    catch         =FLQuant(),
    stock         =FLQuant(),

    params        =FLPar(NA,dimnames=list(param=c("msy","k","b0"),iter=1)),
    vcov          =FLPar(NA,dimnames=list(param=c("msy","k","b0"),param=c("msy","k","b0"),iter=1)),
    hessian       =FLPar(NA,dimnames=list(param=c("msy","k","b0"),param=c("msy","k","b0"),iter=1)),
    dof           =as.numeric(NA),
    stopmess      ="not ran"),
  validity=validAspic)

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
