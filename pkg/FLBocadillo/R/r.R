# calculates the intrinsic rate of increase from the Leslie-transition matrix or the Euler-Lotka equation
# by year or by cohort
# estimates r by year or cohort using Leslie matrix ideas
if (!isGeneric("r")) {
	setGeneric("r", function(object, ...){
		value  <-  standardGeneric("r")
		value})}

setMethod("r", signature(object="FLQuant"),
   function(object,fec,by='year',method='el',...){
       r.(object,fec,by=by,method=method,...)})

setMethod("r", signature(object="FLBiol"),
   function(object,by='year',method='el',...){
       r(m(object),fec=fec(object),by=by,method=method,...)})

setMethod("r", signature(object="FLStock"),
   function(object,by='year',method='el',...){
       r(m(object),fec=mat(object),by=by,method=method,...)})

setMethod("r", signature(object="FLBRP"),
   function(object,by='year',method='el',...){
       r(m(object),fec=mat(object),by=by,method=method,...)})

survprob<-function(m, by = 'year',...) {

		ps <- m
		mm <- m

		# estimate by year
		if(by == 'year') {
			ps[1,,,,,] <- 1
			for(a in 2:dim(ps)[1])
				ps[a,,,,,] <- ps[a-1,,,,,]*exp(-mm[a-1,,,,,])
      }

		# estimate by cohort
		if(by == 'cohort') {
		   }

		return(ps)
	  }


r.<-function(m, f, by = 'year', method = 'el',...) {

		if(by != 'year' && by != 'cohort')
			stop("Error in r: direction of estimation is neither year nor cohort")

		if(method != 'leslie' && method != 'el')
			stop("Error in r: method used is neither Leslie matrix or Euler-Lotka")

		# call survprob to convert M to survival probabilities
		ps <- survprob(m, f,by)

		# estimate by year
		if(by == 'year') {

			dmf <- dim(f)
			dmps <- dim(ps)

			age <- as.numeric(dimnames(f)$age)

			# solve Euler-Lotka equation
			if(method == 'el') {

				r.func <- function(ff,p,age) {

					# solve Euler-Lotka using optimise
					elfn <- function(x) {
						return((sum(exp(-x[1]*age)*p*ff)-1)^2)}

					res.r <- optimise(elfn,interval=c(-10,10))[[1]]

					return(res.r)
				}

				if(dmf[6] > 1 && dmps[6] > 1 && (dmf[6] != dmps[6]))
					stop("Error in r: iteration dimensions are not the same in FLBiol object")

				if(dmf[6] > 1 && dmps[6] > 1)
					nits <- dmf[6]

				if(dmf[6] == 1 && dmps[6] == 1)
					nits <- 1

				if(dmf[6] > 1 && dmps[6] == 1) {

					tmp <- ps
					ps <- f
					ps[] <- tmp[]
					rm(tmp)
					nits <- dmf[6]
				}

				if(dmf[6] == 1 && dmps[6] > 1) {

					tmp <- f
					f <- ps
					f[] <- tmp[]
					rm(tmp)
					nits <- dmps[6]
				}

				r.ret <- FLQuant(quant='all',dim=c(1,dmf[2],1,1,1,nits))
				dimnames(r.ret) <- dimnames(quantMeans(f))

				# define required variables for the estimation
				for(y in 1:dmf[2]) {

					# loop over the iterations

					for(i in 1:nits) {

						ff <- as.vector(f[,y,,,,i])
						p <- as.vector(ps[,y,,,,i])

						r.ret[,y,,,,i] <- r.func(ff,p,age)
					}
				}
			}

			# use Leslie matrix lead eigenvalues
			if(method == 'leslie') {

				ps <- exp(-m)
				f <- mat

				# define function to construct leslie matrix and calculate r
				r.func <- function(ff,p) {

					# construct the leslie matrix
					lesm <- matrix(ncol=length(ff),nrow=length(ff))

					lesm[,] <- 0
					lesm[1,] <- ff[]
					na <- length(ff)
					for(a in 1:(na-1))
						lesm[a+1,a] <- p[a+1]

					# calculate log of real part of the lead eigenvalue of the leslie matrix
					res.r <- log(max(Re(eigen(lesm)[['values']])))

					return(res.r)
				}

				if(dmf[6] > 1 && dmps[6] > 1 && (dmf[6] != dmps[6]))
					stop("Error in r: iteration dimensions are not the same in FLBiol object")

				if(dmf[6] > 1 && dmps[6] > 1)
					nits <- dmf[6]

				if(dmf[6] == 1 && dmps[6] == 1)
					nits <- 1

				if(dmf[6] > 1 && dmps[6] == 1) {

					tmp <- ps
					ps <- f
					ps[] <- tmp[]
					rm(tmp)
					nits <- dmf[6]
				}

				if(dmf[6] == 1 && dmps[6] > 1) {

					tmp <- f
					f <- ps
					f[] <- tmp[]
					rm(tmp)
					nits <- dmps[6]
				}

				r.ret <- FLQuant(quant='all',dim=c(1,dmf[2],1,1,1,nits))
				dimnames(r.ret) <- dimnames(quantMeans(f))

				for(y in 1:dmf[2]) {

					# loop over the iterations

					for(i in 1:nits) {

						ff <- as.vector(f[,y,,,,i])
						p <- as.vector(ps[,y,,,,i])

						r.ret[,y,,,,i] <- r.func(ff,p)
					}
				}
			}
		}

		# estimate by cohort
		if(by == 'cohort') {

		}

		return(r.ret)
	  }