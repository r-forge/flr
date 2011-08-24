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
      # cmd <- paste(paste(dir, admbNm, sep="/"), cmdOps)
      path <- paste(dir, "/", sep="")

    # Wind0ws
    } else if (.Platform$OS.type == "windows") {
      # executable
      exe <- paste(system.file("bin", "windows", package="FLBioDym", mustWork=TRUE),
        admbNm, sep="\"")
      file.copy(exe, dir)
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

# runADMBBioDym {{{
runADMBBioDym <- function(object, iter, path, admbNm, cmdOps) {

  # create input .dat file
  idxYrs <- setADMBBioDym(iter(object, iter),paste(path, admbNm,".dat",sep=""))
  
  # run
  res <- system(paste("./", admbNm, " ", cmdOps, sep=""))

  t1 <- read.table(paste(path, admbNm,".rep",sep=""),skip =18,header=T)

  # params
  t2 <- unlist(c(read.table(paste(path,admbNm,".rep",sep=""),nrows=8)))
  object@params[c("r","K","b0","p","q","sigma"), iter] <- t2[1:6]
      
  # fitted
  object@fitted[,ac(idxYrs),,,,iter][] <- unlist(c(t1[,"IndexFit"]))

  # stock biomass
  object@stock[,1:dim(t1)[1],,,,iter] <- unlist(c(t1["Biomass"]))
        
  object <<- object

} # }}}

    # run
    runADMBBioDym(object, 1, path, admbNm, cmdOps)

    # call across iters
    res <- m_ply(data.frame(x=seq(dims(object)$iter)), function(x) runADMBBioDym(object, x, path, admbNm, cmdOps))

    setwd(oldwd)
  
    return(object)
  }
) # }}}

# setADMBBioDym {{{
setADMBBioDym <- function(object, file) {
  
  #
  ctc <- as.list(drop(model.frame(object[["catch"]])))
  ctc <- c(nYrs=length(ctc[[1]]), ctc)

  #
  idx <- as.list(drop(model.frame(object[["index"]])))
  idx$year <-idx$year[ !is.na(idx$index)]
  idx$index<-idx$index[!is.na(idx$index)]
 
  #
  res <- c(ctc, c(nYrs=length(idx[[1]]), idx))
  
  writeADMB(res, file)
   
  return(idx$year)
} # }}}
