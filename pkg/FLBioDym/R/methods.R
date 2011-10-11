# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, ICCAT
# $Id:  $

# harvest {{{
setMethod('harvest', signature(object='FLBioDym'),
  function(object) {
    res <- catch(object)/stock(object)[,dimnames(catch(object))$year]
    units(res) <- "hr"
    return(res)
  }
) # }}}

# residuals {{{
setMethod('residuals', signature(object='FLBioDym'),
  function(object) {
    if (object@distribution == "log")
      res <- (log(object@index)-log(fitted(object)[,dimnames(object@index)$year]))
    else
      res <- (object@index-fitted(object)[,dimnames(object@index)$year])
    return(res)
  }
) # }}}

# admbBD {{{
setMethod('admbBD', signature(object='FLBioDym'),
  function(object, cmdOps=paste("-maxfn 500"), dir=tempdir(), admbNm="pella") {

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
  
    return(object)
  }
) # }}}

# setADMBBioDym {{{
setADMBBioDym <- function(object, file) {

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

# runADMBBioDym {{{
runADMBBioDym <- function(object, path, admbNm, cmdOps) {
  
  # create input .dat file
  idxYrs <- setADMBBioDym(object, paste(path, admbNm,".dat",sep=""))
  
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

} # }}}

# combine(FLBioDym) {{{
setMethod("combine", signature(x="FLBioDym", y="FLBioDym"),
  function(x, y) {

    # FLQuants
    catch(x)  <- combine(catch(x), catch(y))
    stock(x)  <- combine(stock(x), stock(y))
    index(x)  <- combine(index(x), index(y))
    fitted(x) <- combine(fitted(x), fitted(y))

    # params
    params(x) <- cbind(params(x), params(y))

    return(x)
  }
)

# }}}


calcSigma<-function(obs,hat=rep(0,length(obs)),error="log"){
   yrs=dimnames(obs)$year
   yrs=yrs[yrs %in% dimnames(hat)$year]
   hat=hat[,yrs]
   obs=obs[,yrs]
 
   SS =sum((obs-hat)^2,na.rm=T)

   return((SS/length(hat))^.5)
   }

#### Calculate Q for use in constricted likelihoods etc
calcQ<-function(bio,idx,error="log"){

   ####  Biomass mid year
   mnBio<-function(x) (x[,-dim(x)[2],,,,,drop=FALSE]+x[,-1,,,,,drop=FALSE])/2

   ####  Biomass mid year
   bio<-mnBio(bio)
   yrs<-dimnames(idx)$year[dimnames(idx)$year %in% dimnames(bio)$year]

   bio<-bio[,yrs,,,,,drop=FALSE]
   idx<-idx[,yrs,,,,,drop=FALSE]

   if (error=="log"){
      q <- sum(bio*idx, na.rm=T)/sum(bio*bio, na.rm=T)}
   else if (error=="normal"){
      q <- exp(sum(log(idx)-log(bio), na.rm=T)/(sum(ifelse(is.na(c(idx)),1,0))))}
   else if (error=="cv"){
      res   <-sum(idx/bio)
      sigma2<-calcSigma(res)
      q     <-(-res+(res^2+4*length(idx)*sigma2*sum((idx/bio)^2)))/(2*length(idx)*sigma2)
      }

   return(q)}

calcB0<-function(index,params,nyrB0,error="log"){
   if (is.null(nyrB0)) return(params["b0"])

   if (error=="log")
      t.<-sweep(log(index[,1:nyrB0,,,,,drop=FALSE]),c(1,6),params["q"],"/")
   else if (error=="normal")
      t.<-sweep(index[,1:nyrB0,,,,,drop=FALSE],c(1,6),params["q"],"/")

   return(exp(apply(t.,c(1,6),mean))/params["K"])}

setInit=function(object){
  params(object)["K"]     =mean(catch(object))*10
  params(object)["b0"]    =0.5
  params(object)["r"]     =0.5
  params(object)["p"]     =1.0
 
#   bio                     =stock(fwd(object,catch=catch(object)))
#   params(object)["q"]     =calcQ(    bio,index(object))
#   params(object)["sigma"] =calcSigma(bio,index(object))
#   
  params(object)["q"]     =1
  params(object)["sigma"] =0.3

  object@bounds[,"start"]=params(object)
  object@bounds[,"lower"]=object@bounds[,"start"]*0.1
  object@bounds[,"upper"]=object@bounds[,"start"]*10.0
    
  object@bounds["p", "phase"]=-1
  object@bounds["b0","phase"]=-1
  object@priors[,1]=-1
 
  return(object)}