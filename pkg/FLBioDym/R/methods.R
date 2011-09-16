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
      iter(stock(object), i) <- res@stock
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
