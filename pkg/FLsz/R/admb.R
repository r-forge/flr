setGeneric('admbDat', function(object,...)
   standardGeneric('admbDat'))
setMethod('admbDat', signature(object='FLsz'),
  function(object,admbNm="seine") seineDat(object,admbNm))
    
setGeneric('admbRep', function(object,...)
   standardGeneric('admbRep'))
setMethod('admbRep', signature(object='FLsz'),
  function(object,i,admbNm="seine") seineRep(object,i,admbNm))
    
setGeneric('admbFit', function(object,...)
   standardGeneric('admbFit'))

setMethod('admbFit', signature(object='FLsz'),
  function(object, package=class(object), admbNm="seine", cmdOps=paste("-maxfn 500"), dir=tempdir()) {

    ##### set up temp dir with exe for data files
    # Linux
    if (R.version$os=="linux-gnu") {
      # executable
      exe <- paste(system.file("bin", "linux", package=package, mustWork=TRUE),admbNm, sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "/", sep="")

    # Wind0ws
    } else if (.Platform$OS.type == "windows") {
      # executable
      exe <- paste(system.file("bin", "windows", package=package, mustWork=TRUE),
        paste(admbNm, ".exe", sep=""), sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "\\", sep="")
    
    # Mac OSX
    # or fail!
    }else 
      stop()
        
    oldwd <- getwd()
 
    object=chkIters(object)
    for (i in seq(dims(object)$iter)) {
        # change wd to avoid ADMB case bug
        setwd(dir)

        # create ADMB input files
        admbDat(iter(object, i))
       
        # run
        system(paste("./", admbNm, " ", cmdOps, sep=""))
 
        # read ADMB output files
        object=admbRep(object, i)
        }
    
    setwd(oldwd)
   
    return(object)})

seineDat = function(object,admbNm) {

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
   
  writeADMB(dat,paste(admbNm,".dat",sep=""))
    
  # ctl file
  ctl <- object@bounds 
  ctl[,2:4] <- log(ctl[,2:4])
  ctl <- alply(ctl,1)
  names(ctl) <- dimnames(object@bounds)$params
  writeADMB(ctl,paste(admbNm,".ctl",sep=""))
    
  # prr file
  prr <- object@priors 
  prr <- alply(prr,1)
  names(prr) <- dimnames(object@priors)$params
  writeADMB(prr,paste(admbNm,".prr",sep=""))
  }
  
seineRep = function(object,i,admbNm) {

     rep=readADMB(paste(admbNm,".rep",sep=""))

     std=read.table(paste(admbNm,".std",sep=""),skip=1)[,-1]
     names(std)=c("param","value","sd")
 
     params(object)[,i]=bounds(object)[,"initial"]
     params(object)[bounds(object)[,"phase"]>0,i]=std[,"value"]
     
     object@se[,i]= 0
     object@se[object@bounds[,"phase"]>0,i]=std[,"sd"]
     object@hat[,,,,,i]      =FLQuant(rep$hatLen,   dimnames=dimnames(iter(object@obs,i)))
     object@residuals[,,,,,i]=FLQuant(rep$Residuals,dimnames=dimnames(iter(object@obs,i)))
      
      # object@vcov    =rep$vcov     
      # object@hessian =rep$hessian      
      # object@logLik  =rep$logLik     
      # object@rsdlvar =rep$rsdlVar         
      # object@dof     =rep$dof    
      # object@stopmess=rep$stopmess
      # object@aic     =rep$aic

     return(object)}
 