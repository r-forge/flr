# admbBD
# setADMBBioDym <- function(object, file) {
# runADMBBioDym <- function(object, path, admbNm, cmdOps) {

setADMBSeine = function(object, file) {

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
   
  writeADMB(res, file)}
  
runADMBBioDym <- function(object, path, admbNm, cmdOps) {
  
  # create input .dat file
  setADMBSeine(object, paste(path, admbNm,".dat",sep=""))
  
  # run
  res <- system(paste("./", admbNm, " ", cmdOps, sep=""))
  # std
  # start  

  t1 <- read.table(paste(path, admbNm,".rep",sep=""),skip =18,header=T)

  # params
  t2 <- unlist(c(read.table(paste(path,admbNm,".rep",sep=""),nrows=8)))
  object@params[c("r","K","b0","p","q","sigma")] <- t2[1:6]
      
  # fitted
  object@fitted[] <- unlist(c(t1[,"IndexFit"])) 

  # stock biomass
  object@stock[,1:dim(t1)[1]] <- unlist(c(t1["Biomass"])) 

  return(object)

setMethod('admbSeine', signature(object='FLsz'),
  function(object, cmdOps=paste("-maxfn 500"), dir=tempdir(), admbNm="seine", package="FLsz", iterStuff=c("")) {

    # Linux
    if (R.version$os=="linux-gnu") {
      # executable
      exe <- paste(system.file("bin", "linux", package=package, mustWork=TRUE),
        admbNm, sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "/", sep="")

    # Windows
    } else if (.Platform$OS.type == "windows") {
      # executable
      exe <- paste(system.file("bin", "windows", package=package, mustWork=TRUE),
        paste(admbNm, ".exe", sep=""), sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "\\", sep="")
    }
  
    # Mac OSX fail!
    else 
      stop()


    # create input files
    # ctl file
    ctl        <- object@bounds 
    ctl[,2:4]  <- log(ctl[,2:4])
    ctl        <- alply(ctl,1)
    names(ctl) <- dimnames(object@bounds)$params
    writeADMB(ctl, paste(path, "/", admbNm, ".ctl", sep=""))
    
    # prr file
    prr        <- object@priors 
    prr        <- alply(prr,1)
    names(prr) <- dimnames(object@priors)$params
    writeADMB(prr, paste(path, "/", admbNm, ".prr", sep=""))
   
    # change wd to avoid ADMB case bug
    oldwd <- getwd()
    setwd(path)

    # propagate as needed
    its <- dims(object)$iter
    # params
    params(object) <- propagate(params(object)[,1], its)
    fitted(object) <- FLQuant(dimnames=dimnames(index(object))[1:5], iter=its)
    stock(object)  <- FLQuant(dimnames=dimnames(stock(object))[1:5], iter=its)

    # vcov
    vcov(object)=FLPar(array(NA, dim=c(dim(params(object))[1],dim(params(object))[1],
      dims(object)$iter), dimnames=list(params=dimnames(params(object))[[1]],
      params=dimnames(params(object))[[1]],iter=1:its)))
    
    # call across iters
    # TODO foreach
    # res <- foreach(i = seq(its), .combine='combine') %dopar% runADMBBioDym(FLCore::iter(object, i), path, admbNm, cmdOps)
    for(i in seq(its)) {
      res <- admb(iter(object, i), path, admbNm, cmdOps)
      iter(stock(object), i)  <- res@stock
      iter(fitted(object), i) <- res@fitted
      iter(params(object), i) <- res@params[,1]}

    setwd(oldwd)
  
    return(object)})


runADMBSeine <- function(object, iter, path, admbNm, cmdOps) {
      
      # create input .dat file
      idxYrs <- setADMBSeine(iter(object, iter),paste(path, admbNm,".dat",sep=""))
      
      # run
      res <- system(paste("./", admbNm, " ", cmdOps, sep=""))
      # std
      # start  
    
      t1 <- read.table(paste(path, admbNm,".rep",sep=""),skip =18,header=T)

      # params
      t2 <- unlist(c(read.table(paste(path,admbNm,".rep",sep=""),nrows=8)))
      object@params[c("r","K","b0","p","q","sigma"), iter] <- t2[1:6]
      
#       t3 =read.table(paste(path,admbNm,".std",sep=""),skip=1,nrows=8)[,4]
#       
#       object@vcov["r","r",iter]   <- t2[1:6]
#       object@vcov["K","r",iter]   <- t2[1:6]
#       object@vcov["b","r",iter]   <- t2[1:6]
#       object@vcov["p","r",iter]   <- t2[1:6]
#       object@vcov["q","r",iter]   <- t2[1:6]
#       object@vcov["r","r",iter]   <- t2[1:6]
#      
      # fitted
      object@fitted[,ac(idxYrs),,,,iter][] <- unlist(c(t1[,"IndexFit"]))
    
      # stock biomass
      object@stock[,1:dim(t1)[1],,,,iter] <- unlist(c(t1["Biomass"]))
      
      object <<- object
      }

setMethod('admb', signature(object='FLsz'),
  function(object, cmdOps=paste("-maxfn 500"), dir=tempdir(), admbNm="pella") {

    # Linux
    if (R.version$os=="linux-gnu") {
      # executable
      exe <- paste(system.file("bin", "linux", package="FLSeine", mustWork=TRUE),
        admbNm, sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "/", sep="")

    # Wind0ws
    } else if (.Platform$OS.type == "windows") {
      # executable
      exe <- paste(system.file("bin", "windows", package="FLSeine", mustWork=TRUE),
        paste(admbNm, ".exe", sep=""), sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "\\", sep="")
    
    # Mac OSX
    # or fail!
    }else{
      stop()}

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

# runADMBSeine {{{
runADMBSeine <- function(object, path, admbNm, cmdOps) {
  
  # create input .dat file
  idxYrs <- setADMBSeine(object, paste(path, admbNm,".dat",sep=""))
  
  # run
  res <- system(paste("./", admbNm, " ", cmdOps, sep=""))
  # std
  # start  

  t1 <- read.table(paste(path, admbNm,".rep",sep=""),skip =18,header=T)

  # params
  t2 <- unlist(c(read.table(paste(path,admbNm,".rep",sep=""),nrows=8)))
  object@params[c("r","K","b0","p","q","sigma")] <- t2[1:6]
      
  # fitted
  object@fitted[,ac(idxYrs)][] <- unlist(c(t1[,"IndexFit"]))

  # stock biomass
  object@stock[,1:dim(t1)[1]] <- unlist(c(t1["Biomass"]))

  return(object@stock)      

} # }}}


    # propagate as needed
    its <- dims(object)$iter
    # params
    params(object) <- propagate(iter(params(object), 1), its)
    # fitted
    fitted(object) <- FLQuant(dimnames=dimnames(index(object))[1:5], iter=its)
    # stock
    stock(object) <- FLQuant(dimnames=dimnames(stock(object))[1:5], iter=its)

    # vcov
    vcov(object)=FLPar(array(NA,dim=     c(dim(params(object))[1],dim(params(object))[1],dims(object)$iter), 
                                dimnames=list(params=dimnames(params(object))[[1]],params=dimnames(params(object))[[1]],iter=1:dims(object)$iter)))

    # call across iters
    # TODO foreach
    res <- foreach(i = seq(its), .combine='combine') %dopar% runADMBSeine(iter(object, i), path, admbNm, cmdOps)

    
    setwd(oldwd)
  
    return(res)
  }
) # }}}

# setADMBSeine {{{
setADMBSeine <- function(object, file) {
  
  #
  ctc <- as.list(model.frame(object[["catch"]], drop=TRUE))
  ctc <- c(nYrs=length(ctc[[1]]), ctc)

  #
  idx <- as.list(model.frame(object[["index"]], drop=TRUE))
  idx$year <-idx$year[ !is.na(idx$index)]
  idx$index<-idx$index[!is.na(idx$index)]
 
  #
  res <- c(ctc, c(nYrs=length(idx[[1]]), idx))
  
  writeADMB(res, file)
   
  return(idx$year)
} # }}}

setGeneric('seine', function(object,...)
   standardGeneric('seine'))
setMethod('seine', signature(object='FLsz'),
  function(object,cmdOps=paste("-maxfn 500"),admbNm="seine")
  {  
  object=window(object,start=range(object)["minyear"],end=range(object)["maxyear"])
  dimnames(object@grw)$params=tolower(dimnames(object@grw)$params)  
  
  fn=function(object,cmdOps,admbNm){  
    
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
  
     #### Run ADMB.exe
     writeADMB(dat,file=paste(pathNm,"/seine.dat",sep=""))
               
     pathOrg<-getwd()
     setwd(pathNm)
          
     sys.result=system(paste("./", admbNm, " ", cmdOps, sep=""))
             
     setwd(pathOrg)
        
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
      
  ### TO DO #####################################################
  ##  doesnt yet work with iterations!
  ##  If either n or obs have more than 1 iteration then need to 
  ##  make iters match for other slots i.e.  
    
  iterSlots=c("obs","hat","n","residuals",
              "grw",
              "params","se","vcov","hessian",
              "logLik","rsdlVar","stopmess")
  
  iters=mlply(iterSlots, function(x,object) dimnames(slot(object,x))$iter, object=object)

  if (dim(object@obs)[6]>1 | dim(object@n)[6]>1) {

    ## check all iters are 1 or n            
    nits=laply(iters,length)
              
    ## make 1s Ns                                   
    #   m_ply(iterSlots[nits>1], function(x,object,nIts) { 
    #           if (dim( slot(object,x))|=nits)    
    #              slot(object,x)=propagate(slot(object,x),nits)) 
    #          object        <<-object},
    #           object=object,nIts=max(nits))
              
    ## then do an FLComp::apply over iter iter dim   
    ##res=qqply(object, 6,  fn(object),cmdOps=cmdOps,admbNm=admbNm)
    }
  
  ##############################################################
  object=fn(object,cmdOps=cmdOps,admbNm=admbNm)
                       
  return(object)})
