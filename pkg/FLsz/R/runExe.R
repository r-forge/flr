setMethod('fit', signature(object='FLsz'),
  function(object, package="FLsz", exeNm="seine", dir=tempdir(),...) 
      runExe(object, package, exeNm, dir,...))

writeFn=function(object,exeNm="seine") {

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
   
  writeADMB(dat,paste(exeNm,".dat",sep=""))
    
  # ctl file
  ctl <- object@bounds 
  ctl[,2:4] <- log(ctl[,2:4])
  ctl <- alply(ctl,1)
  names(ctl) <- dimnames(object@bounds)$params
  writeADMB(ctl,paste(exeNm,".ctl",sep=""))
    
  # prr file
  prr <- object@priors 
  prr <- alply(prr,1)
  names(prr) <- dimnames(object@priors)$params

  writeADMB(prr,paste(exeNm,".prr",sep=""))}
  
readFn=function(object,i,exeNm="seine") {

     rep=readADMB(paste(exeNm,".rep",sep=""))

     std=read.table(paste(exeNm,".std",sep=""),skip=1)[,-1]
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
      
runExe=function(object, package="FLszS", exeNm="seine", dir=tempdir(),cmdOps=paste("-maxfn 500"),...){
   
    ##### set up temp dir with exe for data files
    # Linux
    if (R.version$os=="linux-gnu") {
      # executable
      exe <- paste(system.file("bin", "linux", package=package, mustWork=TRUE),exeNm, sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "/", sep="")

    # Windows
    } else if (.Platform$OS.type == "windows") {
      # executable
      exe <- paste(system.file("bin", "windows", package=package, mustWork=TRUE),
        paste(exeNm, ".exe", sep=""), sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "\\", sep="")
    
    # Mac OSX
    # or fail!
    }else 
      stop()
   
    oldwd <- getwd()
 
    # change wd to avoid exe case bug
    setwd(dir)

    nits=dims(object)$iter
    if (dims(    se(   object))$iter==1)        se(object)=propagate(    se(   object),nits)
    if (dims(params(   object))$iter==1)    params(object)=propagate(params(   object),nits)
    if (dims(    hat(  object))$iter==1)       hat(object)=propagate(  hat(    object),nits)
    if (dims(residuals(object))$iter==1) residuals(object)=propagate(residuals(object),nits)
    
    if (nits>1) nits=seq(nits)
    for (i in nits){
      # create exe input files
      do.call("writeFn", list(object=iter(object,i)))
         
      # run
      system(paste("./", exeNm, sep=""))
   
      # read exe output files
      object=readFn(object=object,i=i)
      }
          
    setwd(oldwd)
   
    return(object)}
 
