setGeneric('admbWrite', function(object,...)
   standardGeneric('admbWrite'))
setMethod('admbWrite', signature(object='FLsz'),
  function(object,pathNm,cmdOps=paste("-maxfn 500")) .admbWrite(object,pathNm,cmdOps))
    
setGeneric('admbRead', function(object,...)
   standardGeneric('admbRead'))
setMethod('admbRead', signature(object='FLsz'),
  function(object,pathNm) .admbWrite(object,pathNm))
    
.admbWrite = function(object, pathNm, cmdOps) {

    ## Data
    ObsLength    = object@obs
    SampleSize   = object@n
   
    ObsLength[ is.na(ObsLength)]=mean(ObsLength,na.rm=T)
    SampleSize[is.na(SampleSize)]=0
 
    ## Parameters
    nbreaks= dim(object@params)[1]/2 -1
    zguess =array(c(object@bounds[,"initial"][1:(nbreaks+1)],
                    object@bounds[,  "phase"][1:(nbreaks+1)]),c(nbreaks+1,2))
 
    yguess =array(c(object@bounds[,"initial"][1+nbreaks+(1:(nbreaks))],
                    object@bounds[,  "phase"][1+nbreaks+(1:(nbreaks))]),c(nbreaks,2))
 
    sigma  =rev(object@bounds[,"initial"])[1]
   
    ## Growth
    dimnames(object@grw)$params  = tolower(dimnames(object@grw)$params)
    KParm        = object@grw["k"]
    LInf         = object@grw["linf"]
    Lc           = object@grw["lc"]
        
    ## Output data file
    dat<-       list("Number of Breaks"                            =nbreaks,
                     "First Year of Data"                          =range(object)["minyear"],
                     "Last Year of Data"                           =range(object)["maxyear"],
        
                     "(1,NYears Observed Mean Lengths)"            =ObsLength,
                     "(1,NYears Observed Sample Sizes)"            =SampleSize,
          
                     "VB K Parameter"                              =KParm,
                     "VB L-Infinity"                               =LInf,
          
                     "Critical Length - Length at first capture"   =Lc,
        
                     "(1,NBreaks+1,1,2)"                           =c(t(zguess)),
                     "(1,NBreaks,  1,2)"                           =c(t(yguess)),
          
                     "sigma"                                       =sigma,
                     "stepsize"                                    =5,
                     "casenum"                                     =10)
   
  writeADMB(dat, paste(pathNm,"seine.dat",sep="/"))}
  
.admbRead = function(object,pathNm) {

     rep=readADMB(paste(pathNm,"/seine.rep",sep=""))

     std=read.table(paste(pathNm,"/seine.std",sep=""),skip=1)[,-1]
     names(std)=c("param","value","sd")
 
     params(object)=bounds(object)[,"initial"]
     params(object)[bounds(object)[,"phase"]>0]=std[,"value"]
     
     object@se[]= 0
     object@se[object@bounds[,"phase"]>0]=std[,"sd"]
     object@hat      =FLQuant(rep$hat,      dimnames=dimnames(object@obs))
     object@residuals=FLQuant(rep$Residuals,dimnames=dimnames(object@obs))
      
      # object@vcov    =rep$vcov     
      # object@hessian =rep$hessian      
      # object@logLik  =rep$logLik     
      # object@rsdlvar =rep$rsdlVar         
      # object@dof     =rep$dof    
      # object@stopmess=rep$stopmess
      # object@aic     =rep$aic

     return(object)}


setMethod('admbBD', signature(object='FLsz'),
  function(object, cmdOps=paste("-maxfn 500"), admbNm="seine", dir=tempdir()) {

    # Linux
    if (R.version$os=="linux-gnu") {
      # executable
      exe <- paste(system.file("bin", "linux", package="FLBioDym", mustWork=TRUE),
        admbNm, sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "/", sep="")

    # Wind0ws
    } else if (.Platform$OS.type == "windows") {
      # executable
      exe <- paste(system.file("bin", "windows", package="FLBioDym", mustWork=TRUE),
        paste(admbNm, ".exe", sep=""), sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "\\", sep="")
    }
    # Mac OSX
    # or fail!
    else {
      stop()
    }

    # create input files
    # ctl file
    ctl <- object@bounds 
    ctl[,2:4] <- log(ctl[,2:4])
    ctl <- alply(ctl,1)
    names(ctl) <- dimnames(object@bounds)$params
    writeADMB(ctl, paste(path, "/", admbNm, ".ctl", sep=""))
    
    # prr file
    prr <- object@priors 
    prr <- alply(prr,1)
    names(prr) <- dimnames(object@priors)$params
    writeADMB(prr, paste(path, "/", admbNm, ".prr", sep=""))
   
    # change wd to avoid ADMB case bug
    oldwd <- getwd()
    setwd(path)

    # propagate as needed
    its <- dims(object)$iter
    # params
    params(object) <- propagate(params(object)[,1], its)
    # fitted
    fitted(object) <- FLQuant(dimnames=dimnames(index(object))[1:5], iter=its)
    # stock
    stock(object) <- FLQuant(dimnames=dimnames(stock(object))[1:5], iter=its)

    # vcov
    vcov(object)=FLPar(array(NA, dim=c(dim(params(object))[1],dim(params(object))[1],
      dims(object)$iter), dimnames=list(params=dimnames(params(object))[[1]],
      params=dimnames(params(object))[[1]],iter=1:its)))
    
    # call across iters
    # TODO foreach
    # res <- foreach(i = seq(its), .combine='combine') %dopar% runADMBBioDym(FLCore::iter(object, i), path, admbNm, cmdOps)
    for(i in seq(its)) {
      res <- runADMBBioDym(iter(object, i), path, admbNm, cmdOps)
      iter(stock(object), i)  <- res@stock
      iter(fitted(object), i) <- res@fitted
      iter(params(object), i) <- res@params[,1]
    }

    setwd(oldwd)
  
    return(object)}) 

