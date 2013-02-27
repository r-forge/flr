# operators.R - DESC
# operators.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $


# FLQuant, FLQuant {{{
# %*% {{{
# Multiply two FLQuant objects by matching dimensions, expands 1 to n
setMethod("%*%", signature(x="FLQuant", y="FLQuant"),
          function(x, y) {
            
            # get dims
            dx <- dim(x)
            dy <- dim(y)
            
            # final dims
            di <- pmax(dx, dy)
            dli <- lapply(as.list(di), function(x) rep(1, x))
            
            # TEST: No expansion n -> m allowed, must be originally 1
            if(any(di != dx &  dx != 1) | any(di != dy &  dy != 1))
              stop("dims to be expanded in cannot be of length > 1")
            
            # new x
            dlx <- lapply(as.list(dx), seq)
            dlx[di > dx] <- dli[di > dx]
            
            rx <- do.call('[', c(list(x=x@.Data, drop=FALSE), dlx))
            
            # new y
            dly <- lapply(as.list(dy), seq)
            dly[di > dy] <- dli[di > dy]
            
            ry <- do.call('[', c(list(x=y@.Data, drop=FALSE), dly))
            
            # dimnames
            dni <- dimnames(x)
            dni[di > dx] <- dimnames(y)[di > dx]
            
            return(FLQuant(rx * ry, dimnames=dni, units=paste(units(x), units(y), sep="*")))
          }
) # }}}

# %/% {{{
# Divide two FLQuant objects by matching dimensions, expands 1 to n
setMethod("%/%", signature(e1="FLQuant", e2="FLQuant"),
          function(e1, e2) {
            
            # get dims
            de1 <- dim(e1)
            de2 <- dim(e2)
            
            # final dims
            di <- pmax(de1, de2)
            dli <- lapply(as.list(di), function(x) rep(1, x))
            
            # TEST: No expansion n -> m allowed, must be originally 1
            if(any(di != de1 &  de1 != 1) | any(di != de2 &  de2 != 1))
              stop("dims to be expanded cannot be of length > 1")
            
            # new x
            dle1 <- lapply(as.list(de1), seq)
            dle1[di > de1] <- dli[di > de1]
            
            re1 <- do.call('[', c(list(x=e1@.Data, drop=FALSE), dle1))
            
            # new y
            dle2 <- lapply(as.list(de2), seq)
            dle2[di > de2] <- dli[di > de2]
            
            re2 <- do.call('[', c(list(x=e2@.Data, drop=FALSE), dle2))
            
            # dimnames
            dni <- dimnames(e1)
            dni[di > de1] <- dimnames(e2)[di > de1]
            
            return(FLQuant(re1 / re2, dimnames=dni, units=paste(units(e1), units(e2), sep="/")))
          }
) # }}}

# %+% {{{
# Add two FLQuant objects by matching dimensions, expands 1 to n
setMethod("%+%", signature(x="FLQuant", y="FLQuant"),
          function(x, y) {
            
            # get dims
            dx <- dim(x)
            dy <- dim(y)
            
            # final dims
            di <- pmax(dx, dy)
            dli <- lapply(as.list(di), function(x) rep(1, x))
            
            # TEST: No expansion n -> m allowed, must be originally 1
            if(any(di != dx &  dx != 1) | any(di != dy &  dy != 1))
              stop("dims to be expanded in cannot be of length > 1")
            
            # new x
            dlx <- lapply(as.list(dx), seq)
            dlx[di > dx] <- dli[di > dx]
            
            rx <- do.call('[', c(list(x=x@.Data, drop=FALSE), dlx))
            
            # new y
            dly <- lapply(as.list(dy), seq)
            dly[di > dy] <- dli[di > dy]
            
            ry <- do.call('[', c(list(x=y@.Data, drop=FALSE), dly))
            
            # dimnames
            dni <- dimnames(x)
            dni[di > dx] <- dimnames(y)[di > dx]
            
            return(FLQuant(rx + ry, dimnames=dni, units=paste(units(x), units(y), sep="+")))
          }
) # }}}

# %-% {{{
# Sustract two FLQuant objects by matching dimensions, expands 1 to n
setMethod("%-%", signature(x="FLQuant", y="FLQuant"),
          function(x, y) {
            
            # get dims
            dx <- dim(x)
            dy <- dim(y)
            
            # final dims
            di <- pmax(dx, dy)
            dli <- lapply(as.list(di), function(x) rep(1, x))
            
            # TEST: No expansion n -> m allowed, must be originally 1
            if(any(di != dx &  dx != 1) | any(di != dy &  dy != 1))
              stop("dims to be expanded in cannot be of length > 1")
            
            # new x
            dlx <- lapply(as.list(dx), seq)
            dlx[di > dx] <- dli[di > dx]
            
            rx <- do.call('[', c(list(x=x@.Data, drop=FALSE), dlx))
            
            # new y
            dly <- lapply(as.list(dy), seq)
            dly[di > dy] <- dli[di > dy]
            
            ry <- do.call('[', c(list(x=y@.Data, drop=FALSE), dly))
            
            # dimnames
            dni <- dimnames(x)
            dni[di > dx] <- dimnames(y)[di > dx]
            
            return(FLQuant(rx - ry, dimnames=dni, units=paste(units(x), units(y), sep="-")))
          }
) # }}}

# }}}

# FLPar, FLQuant {{{
# %*% {{{
# Multiply FLPar against FLQuant by matching dimnames, expands 1 to n
setMethod("%*%", signature(x="FLPar", y="FLQuant"),
          function(x, y) {
            
            # dims & dimnames
            dx <- dim(x)
            dnx <- dimnames(x)
            dy <- dim(y)
            dny <- dimnames(y)
            
            # TEST: non-matching dims in x should be of length 1
            idy <- !names(dnx) %in% names(dny)
            if(any(dx[idy] > 1))
              stop("dimensions in 'x' not matching those in 'y' must be of length=1")
            
            # aperm if FLPar dimnames sorted differently to FLQuant's
            idx <- matchDimnames(dnx, dny)
            if(any(idx != sort(idx))) {
              x <- aperm(x, idx)
              dx <- dx[idx]
              dnx <- dnx[idx]
            }
            
            # tmp FLQuant dims
            di <- rep(1, 6)
            di[names(dny) %in% names(dnx)] <- dx[names(dnx) %in% names(dny)]
            
            # x data in 6D array
            rx <- array(x@.Data, dim=di)
            
            # expansion done in %*%(FLQuant, FLQuant)
            return(FLQuant(rx) %*% y)
          }
) # }}}

# %/% {{{
# Divide FLPar against FLQuant by matching dimnames, expands 1 to n
setMethod("%/%", signature(e1="FLPar", e2="FLQuant"),
          function(e1, e2) {
            
            # dims & dimnames
            de1 <- dim(e1)
            dne1 <- dimnames(e1)
            de2 <- dim(e2)
            dne2 <- dimnames(e2)
            
            # TEST: non-matching dims in e1 should be of length 1
            ide2 <- !names(dne1) %in% names(dne2)
            if(any(de1[ide2] > 1))
              stop("dimensions in 'e1' not matching those in 'e2' must be of length=1")
            
            # aperm if FLPar dimnames sorted differently to FLQuant's
            ide1 <- matchDimnames(dne1, dne2)
            if(any(ide1 != sort(ide1))) {
              e1 <- aperm(e1, ide1)
              de1 <- de1[ide1]
              dne1 <- dne1[ide1]
            }
            
            # tmp FLQuant dims
            di <- rep(1, 6)
            di[names(dne2) %in% names(dne1)] <- de1[names(dne1) %in% names(dne2)]
            
            # e1 data in 6D array
            re1 <- array(e1@.Data, dim=di)
            
            # expansion done in %/%(FLQuant, FLQuant)
            return(FLQuant(re1) %/% e2)
          }
) # }}}

# %+% {{{
# Add FLPar and FLQuant by matching dimnames, expands 1 to n
setMethod("%+%", signature(x="FLPar", y="FLQuant"),
          function(x, y) {
            
            # dims & dimnames
            dx <- dim(x)
            dnx <- dimnames(x)
            dy <- dim(y)
            dny <- dimnames(y)
            
            # TEST: non-matching dims in x should be of length 1
            idy <- !names(dnx) %in% names(dny)
            if(any(dx[idy] > 1))
              stop("dimensions in 'x' not matching those in 'y' must be of length=1")
            
            # aperm if FLPar dimnames sorted differently to FLQuant's
            idx <- matchDimnames(dnx, dny)
            if(any(idx != sort(idx))) {
              x <- aperm(x, idx)
              dx <- dx[idx]
              dnx <- dnx[idx]
            }
            
            # tmp FLQuant dims
            di <- rep(1, 6)
            di[names(dny) %in% names(dnx)] <- dx[names(dnx) %in% names(dny)]
            
            # x data in 6D array
            rx <- array(x@.Data, dim=di)
            
            # expansion done in %+%(FLQuant, FLQuant)
            return(FLQuant(rx) %+% y)
          }
) # }}}

# %-% {{{
# Substract FLPar and FLQuant by matching dimnames, expands 1 to n
setMethod("%-%", signature(x="FLPar", y="FLQuant"),
          function(x, y) {
            
            # dims & dimnames
            dx <- dim(x)
            dnx <- dimnames(x)
            dy <- dim(y)
            dny <- dimnames(y)
            
            # TEST: non-matching dims in x should be of length 1
            idy <- !names(dnx) %in% names(dny)
            if(any(dx[idy] > 1))
              stop("dimensions in 'x' not matching those in 'y' must be of length=1")
            
            # aperm if FLPar dimnames sorted differently to FLQuant's
            idx <- matchDimnames(dnx, dny)
            if(any(idx != sort(idx))) {
              x <- aperm(x, idx)
              dx <- dx[idx]
              dnx <- dnx[idx]
            }
            
            # tmp FLQuant dims
            di <- rep(1, 6)
            di[names(dny) %in% names(dnx)] <- dx[names(dnx) %in% names(dny)]
            
            # x data in 6D array
            rx <- array(x@.Data, dim=di)
            
            # expansion done in %-%(FLQuant, FLQuant)
            return(FLQuant(rx) %-% y)
          }
) # }}}
# }}}

# FLQuant, FLPar {{{
# %*% {{{
# Multiply FLQuant against FLPar by matching dimnames, expands 1 to n
setMethod("%*%", signature(x="FLQuant", y="FLPar"),
          function(x, y) {
            
            # dims & dimnames
            dx <- dim(x)
            dnx <- dimnames(x)
            dy <- dim(y)
            dny <- dimnames(y)
            
            # TEST: non-matching dims in y should be of length 1
            idx <- !names(dny) %in% names(dnx)
            if(any(dy[idx] > 1))
              stop("dimensions in 'y' not matching those in 'x' must be of length=1")
            
            # aperm if FLPar dimnames sorted differently to FLQuant's
            idy <- matchDimnames(dny, dnx)
            if(any(idy != sort(idy))) {
              y <- aperm(y, idy)
              dy <- dy[idy]
              dny <- dny[idy]
            }
            
            # tmp FLQuant dims
            di <- rep(1, 6)
            di[names(dnx) %in% names(dny)] <- dy[names(dny) %in% names(dnx)]
            
            # y data in 6D array
            ry <- array(y@.Data, dim=di)
            
            # expansion done in %*%(FLQuant, FLQuant)
            return(x %*% FLQuant(ry))
          }
) # }}}

# %/% {{{
# Divide FLPar against FLQuant by matching dimnames, expands 1 to n
setMethod("%/%", signature(e1="FLQuant", e2="FLPar"),
          function(e1, e2) {
            
            # dims & dimnames
            de1 <- dim(e1)
            dne1 <- dimnames(e1)
            de2 <- dim(e2)
            dne2 <- dimnames(e2)
            
            # TEST: non-matching dims in e1 should be of length 1
            ide1 <- !names(dne2) %in% names(dne1)
            if(any(de2[ide1] > 1))
              stop("dimensions in 'e2' not matching those in 'e1' must be of length=1")
            
            # aperm if FLPar dimnames sorted differently to FLQuant's
            ide2 <- matchDimnames(dne2, dne1)
            if(any(ide2 != sort(ide2))) {
              e2 <- aperm(e2, ide2)
              de2 <- de2[ide2]
              dne2 <- dne2[ide2]
            }
            
            # tmp FLQuant dims
            di <- rep(1, 6)
            di[names(dne1) %in% names(dne2)] <- de2[names(dne2) %in% names(dne1)]
            
            # e2 data in 6D array
            re2 <- array(e2@.Data, dim=di)
            
            # expansion done in %/%(FLQuant, FLQuant)
            return(e1 %/% FLQuant(re2))
          }
) # }}}

# %+% {{{
# Add FLQuant and FLPar by matching dimnames, expands 1 to n
setMethod("%+%", signature(x="FLQuant", y="FLPar"),
          function(x, y) {
            
            # dims & dimnames
            dx <- dim(x)
            dnx <- dimnames(x)
            dy <- dim(y)
            dny <- dimnames(y)
            
            # TEST: non-matching dims in y should be of length 1
            idx <- !names(dny) %in% names(dnx)
            if(any(dy[idx] > 1))
              stop("dimensions in 'y' not matching those in 'x' must be of length=1")
            
            # aperm if FLPar dimnames sorted differently to FLQuant's
            idy <- matchDimnames(dny, dnx)
            if(any(idy != sort(idy))) {
              y <- aperm(y, idy)
              dy <- dy[idy]
              dny <- dny[idy]
            }
            
            # tmp FLQuant dims
            di <- rep(1, 6)
            di[names(dnx) %in% names(dny)] <- dy[names(dny) %in% names(dnx)]
            
            # y data in 6D array
            ry <- array(y@.Data, dim=di)
            
            # expansion done in %+%(FLQuant, FLQuant)
            return(x %+% FLQuant(ry))
          }
) # }}}

# %-% {{{
# Substract FLQuant and FLPar by matching dimnames, expands 1 to n
setMethod("%-%", signature(x="FLQuant", y="FLPar"),
          function(x, y) {
            
            # dims & dimnames
            dx <- dim(x)
            dnx <- dimnames(x)
            dy <- dim(y)
            dny <- dimnames(y)
            
            # TEST: non-matching dims in y should be of length 1
            idx <- !names(dny) %in% names(dnx)
            if(any(dy[idx] > 1))
              stop("dimensions in 'y' not matching those in 'x' must be of length=1")
            
            # aperm if FLPar dimnames sorted differently to FLQuant's
            idy <- matchDimnames(dny, dnx)
            if(any(idy != sort(idy))) {
              y <- aperm(y, idy)
              dy <- dy[idy]
              dny <- dny[idy]
            }
            
            # tmp FLQuant dims
            di <- rep(1, 6)
            di[names(dnx) %in% names(dny)] <- dy[names(dny) %in% names(dnx)]
            
            # y data in 6D array
            ry <- array(y@.Data, dim=di)
            
            # expansion done in %-%(FLQuant, FLQuant)
            return(x %-% FLQuant(ry))
          }
) # }}}

# }}}

# FLPar, FLPar {{{
# Multiply FLPar against FLPar by matching dimnames, expands 1 to n, creates missing
setMethod("%*%", signature(x="FLPar", y="FLPar"),
          function(x, y) {
            
            # dims & dimnames
            dnx <- dimnames(x)
            dny <- dimnames(y)
            
            # vector of final dim
            dnsx <- unlist(lapply(dnx, length))
            dnsy <- unlist(lapply(dny, length))
            dnd <- rbind(dnsx, dnsy)
            
            
            dr <- pmax(dnsx, dnsy)
            
            # select dimnames from larger FLPar
            if(length(dnx) > length(dny)) {
              dnr <- names(dnx)
              dnmr <- dnx
            }
            else {
              dnr <- names(dny)
              dnmr <- dny
            }
            
            # TEST: non-matching dnames in x or y should be of length 1
            if(any(apply(dnd, 2, function(x) all(x > 0) && max(x)/min(x) != max(x))))
              stop("dimensions in 'x' not matching in length those in 'y' must be of length=1")
            
            # TODO expand & aperm FLPars
            FLPar(
              array(x@.Data, dim=dr, dimnames=dnmr)
              * 
                array(y@.Data, dim=dr, dimnames=dnmr)
            )
          }
) # }}}

# matchDimnames {{{
matchDimnames <- function(dnp, dnq) {
  
  # too tricky to explain ...
  idx <- match(names(dnq)[sort(match(names(dnp), names(dnq)))], names(dnp))
  sx <- seq(names(dnp))
  sx[sx %in% idx] <- idx
  
  return(sx)
} # }}}


hcr.=  function(object, params=FLPar(Ftar=0.8,  Btrig=0.75, Fmin=0.025, Blim=0.25),
                        msy   =    c(Ftar=TRUE, Btrig=TRUE, Fmin=TRUE,  Blim=TRUE),
                        refpt ="missing",
                        bndTac=NULL, bndF=NULL,lag=1,...){
  
  if (dims(params)$iter==1 & dims(refpts(object))$iter>1)
         params=propagate(params,dims(refpts(object))$iter)
      ## Reference Points
      if (refpt=="missing")
         refpt=refpts(object)

      if (msy["Btrig"]) params["Btrig"]=refpt["bmsy"]*params["Btrig"]
      if (msy["Blim"])  params["Blim"] =refpt["bmsy"]*params["Blim"]
      if (msy["Fmin"])  params["Fmin"] =refpt["fmsy"]*params["Fmin"]
      if (msy["Ftar"])  params["Ftar"] =refpt["fmsy"]*params["Ftar"]

      ## HCR
      #if (Blim>=Btrig) stop("Btrig must be greater than Blim")
      a=(params["Ftar"]-params["Fmin"])/(params["Btrig"]-params["Blim"])
      b= params["Ftar"]-a*params["Btrig"]

      ## Calc F
      #SSB =apply(stock(object)[,ac(as.numeric(dims(object)$year-lag))],6,sum)
      yrTac=dims(catch(object))$maxyear+1
      yrRef=ac(yrTac-lag)
      yrTac=ac(yrTac)


      SSB =apply(stock(object)[,yrRef],6,sum)
#print("bug")
#return(list("SSB"=SSB,"a"=a,"b"=b))
# bug
      #val=(SSB%*%a) %+% b
      val=sweep((SSB%*%a),6,b,"+")
#print("bug")
#print(val)

      for (i in seq(dim(val)[6])){
        val[,,,,,i]=max(val[,,,,,i],params["Fmin",min(dim(params)[2],i)])
        val[,,,,,i]=min(val[,,,,,i],params["Ftar",min(dim(params)[2],i)])}

      dimnames(val)$year=yrTac

      return(val)}

##############################################################
#' Harvest Control Rule
#'
#' Calculates catch quota or Total Allowable Catch (TAC) based on a hockey stock harvest control rule.
#'
#' @param  \code{object}, an object of class \code{biodyn} or
#'
#' @param  \code{ctrl}, a string or factor that species the model
#'
#' @param \code{msy}, an \code{FLPar} object with model parameters
#'
#' @return a \code{FLPar} object with value(s) for TAC
#' 
#' @seealso \code{\link{msy}},  \code{\link{bmsy}}, \code{\link{fmsy}} and  \code{\link{refpts}}
#' 
#' @export
#' @docType methods
#' @rdname hcr
#'
#' @examples
#' #hcr("logistic",FLPar(msy=100,k=500))
#'
setMethod('hcr', signature(object='biodyn'),
           function(object, params=FLPar(Ftar=0.8,  Btrig=0.75, Fmin=0.025, Blim=0.25),
                            msy   =    c(Ftar=TRUE, Btrig=TRUE, Fmin=TRUE,  Blim=TRUE),
                            refpt ="missing",
                            bndTac=NULL, 
                            bndF  =NULL,
                            lag   =1,...) 
   hcr.(object,params,msy,refpt=refpt,bndTac=bndTac,bndF=bndF,lag=lag,...))

# setGeneric('hcrJK', function(object, ...) standardGeneric('hcrJK'))
# setMethod( 'hcrJK', signature(object='biodyn'),
#   function(object,Ftar=0.8,Btrig=0.75,Fmin=0.025,Blim=0.25,Fpct=0.35,Bpct=0.85,...){
#     
#     #Ftar=0.8;Btrig=0.75;Fmin=0.025;Blim=0.25;Fpct=0.75;Bpct=0.75
#       val=NULL
#       
#       ## Jacknife
#       for (i in 1:max(dims(catch(object))$iter,dims(index(object))$iter)){
#          object.        =iter(object,i)
#          index(object.) =jacknife(index(object.))
#          object.        =admbBD(object.)
#          
#          ## Reference Points
#          rp =refpts(object.)
#          rp  =model.frame(FLQuants(llply(jackSummary(rp),FLQuant)))
#          Fmsy=qnorm(Fpct,rp[2,"mean"],rp[2,"se"])
#          Bmsy=qnorm(Bpct,rp[3,"mean"],rp[3,"se"])
#          
#          Btrig.=Bmsy*Btrig
#          Blim. =Bmsy*Blim
#          Fmin. =Fmsy*Fmin
#          Ftar. =Fmsy*Ftar         
#       
#          ## HCR
#          #if (Blim>=Btrig) stop("Btrig must be greater than Blim")
#          a= FLPar((Ftar.-Fmin.)/(Btrig.-Blim.))
#          b= FLPar(Ftar.-a*Btrig.)
#       
#          ## Calc F
#          SSB    =stock(iter(object.,1))[,dims(object.)$year-1]
#          val=c(val, qmax(qmin(sweep(sweep(SSB,6,a,"*"),6,b,"+"),Ftar.),Fmin.))         
#          }
#       
#       dmns=dimnames(SSB)
#       dmns$year=as.numeric(dmns$year)+1
#       dmns$iter=1:length(val)
#       val =FLQuant(val,dimnames=dmns)
#       return(val)})


##############################################################
#' Total Allowable Catch
#'
#' Calculates the catch for a given harvest rate and stock biomass
#'
#' @param  \code{object}, an object of class \code{biodyn} or
#'
#' @param  \code{harvest}, an \code{FLQuant} object with harvest rate
#'
#' @return a \code{FLQuant} object with value(s) for catches
#' 
#' @seealso \code{\link{hcr}},  \code{\link{fwd}}
#' 
#' @export
#' @docType methods
#' @rdname tac
#'
#' @examples
#' #tac("logistic",FLPar(msy=100,k=500))
#'
setMethod( 'tac', signature(object='biodyn'),
function(object,harvest,...){
  ## gets tac
  yr    =dims(harvest)$maxyear
  catch(object)=propagate(catch(object),dims(object)$iter)
  object=fwd(window(object,end=yr), harvest=harvest)

  return(catch(object)[,ac(yr)])})

#   object=biodyn("pellat",FLPar(r=.5,k=10000))
hcrFn=function(object,par=FLPar(Ftar=0.7, Btrig=0.8, Fmin=0.025, Blim=0.20) ,maxB=1){
  
  pts=rbind(cbind(refpt="Target",model.frame(rbind(bmsy(object)*c(par["Btrig"]),
                                                   fmsy(object)*c(par["Ftar"])))),
            cbind(refpt="Limit", model.frame(rbind(bmsy(object)*c(par["Blim"]),
                                                   fmsy(object)*c(par["Fmin"])))))
  pts.=pts
  pts.[1,"bmsy"]=params(object)["k"]*maxB
  pts.[2,"bmsy"]=0
  pts.[,1]=c("")
  
  pts=rbind(pts.[1,],pts[1:2,],pts.[2,])
  
  names(pts)[2:3]=c("biomass","harvest")
  pts[,"biomass"]=pts[,"biomass"]/bmsy(object)
  pts[,"harvest"]=pts[,"harvest"]/fmsy(object)
  
  pts}


