FLAdapt=function(FLStock, FLIndices, control=FLAdaptControl(), desc, run=TRUE){

  if (!validObject(FLStock))
		stop("FLStock is not valid!")
  if (!validObject(FLIndices))
		stop("FLIndices is not valid!")
  if (!validObject(control))
		stop("control is not valid!")
    
  res@desc <-as.character(desc)  
  res@call <-as.character(match.call())
  
	#res <- .Call("FLAdapt", FLStock, FLIndices, control)

	res         <- new("FLAdapt")
	res@stock.n <- adpt@stock.n
	res@harvest <- adpt@harvest
	res@control <- adpt@control
	res@call    <- adpt@call
  res@desc    <- adpt@desc
	
	return(res)}

FLAdaptControl <- function(
  FLAdapt      = NULL,
  seed         =as.integer(-911),
	maxit        =as.integer(20), 
	check        =as.logical(TRUE),   
	pdev         =as.numeric(1.0),   
	season       =as.numeric(0.0),         
	option.f     =as.logical(TRUE),  
	q.est        =as.logical(FALSE),
	q.scale      =as.logical(TRUE),   
  q.cv         =as.numeric(1.0),         
  q.add        =as.logical(FALSE),     
	sel.penalty  =as.logical(FALSE),
	sel.sigma    =as.numeric(1.0),         
	sel.minage   =as.integer(NA),
	sel.maxage   =as.integer(NA),
	sel.nyr      =as.integer(NA),
	sr.penalty   =as.logical(TRUE),
	sr.sigma     =as.numeric(1.0),         
	sr.nyr       =as.integer(NA),
	sr.pdf       =as.character("log"),
	rec.penalty  =as.logical(TRUE),
	rec.sigma    =as.numeric(1.0),         
	rec.minyr    =as.integer(NA),
	rec.maxyr    =as.integer(NA),
	catch.penalty=as.logical(FALSE),         
	catch.sigma  =as.numeric(1.0),         
	catch.pdf    =as.character("log")){
	
	if (is.null(FLAdapt)){
		res <- new("FLAdaptControl")
	} else {	# We reuse an FLAdaptControl object embedded in an FLAdapt object
		if (!inherits(FLAdapt, "FLAdapt"))
			stop("FLAdapt must be an 'FLAdapt' object!")
		res <- FLAdapt@control

	# ... and possibly redefine some of its parameters
	if (!missing(seed)){
		res@seed        <- as.integer(seed)
		}
	if (!missing(maxit)){
		res@maxit       <- as.integer(maxit)
		if (res@maxit<0){ 
			warning("maxit < 0, reset to 0")
			res@maxit<-as.integer(0)
			}
		}
	if (!missing(check)){
		res@check       <- as.logical(check)
		}
	if (!missing(season)){
		res@season        <- as.numeric(season)
		if (res@season<0){ 
			warning("season < 0.0, reset to 0.0")
			res@season<-as.numeric(0.0)
			}
		else if (res@season>1.0){ 
			warning("season > 1.0, reset to 1.0")
			res@season<-as.numeric(1.0)
			}
		}
	if (!missing(pdev)){
		res@pdev        <- as.numeric(pdev)
		if (res@pdev<0){ 
			warning("pdev < 0, reset to 0.0")
			res@pdev<-0
			}
		}
	if (!missing(option.f)){
		res@option.f    <- as.logical(option.f)
		}
	if (!missing(q.scale)){     
		res@scale       <- as.logical(scale)
		}
	if (!missing(q.cv)){         
		res@q.cv          <- as.numeric(q.cv)
		if (res@q.cv<0){ 
			warning("q.cv < 0, reset to 0.0")
			res@q.cv<-as.numeric(0.0)
			}
		}
	if (!missing(q.add)){
		res@q.var     <- as.logical(q.var)
		}
	if (!missing(q.est)){
		res@q.est       <- as.logical(q.est)
		}
	if (!missing(sel.penalty)){
		res@sel.penalty    <- as.logical(sel.penalty)
		}
	if (!missing(sel.sigma)){         
		res@sel.sigma          <- as.numeric(sel.sigma)
		if (res@sel.sigma<0){ 
			warning("sel.sigma < 0, reset to 0.0")
			res@sel.sigma<-as.numeric(0.0)
			}
		}
	if (!missing(sel.minage)){
		res@sel.minage    <- as.integer(sel.minage)
		}
	if (!missing(sel.maxage)){
		res@sel.maxage    <- as.integer(sel.maxage)
		}
    if (sel.minage>sel.maxage)
		stop("sel.minage>sel.maxage")
	if (!missing(sr.penalty)){
		res@sr          <- as.logical(sr.penalty)
		}
	if (!missing(sel.nyr)){
		res@sel.nyr    <- as.integer(sel.nyr)
		}
	if (!missing(sr.nyr)){
		res@sr.nyr    <- as.integer(sr.nyr)
		}
	
	if (sr.nyr<0){
		sr.nyr<-as.integer(0)
		sr.penalty<-as.logical(FALSE)
		}
    if (!missing(sr.pdf)){
		res@sr.pdf      <- as.character(sr.pdf)
		}
	if (!missing(sr.sigma)){
		res@sr.sigma   <- as.numeric(sr.sigma)
		}
	
	if (!missing(rec.sigma)){
		res@rec.sigma   <- as.numeric(rec.sigma)
		}
	if (!missing(rec.penalty)){
		res@rec.limk    <- as.logical(rec.penalty)
		}
	if (!missing(rec.minyr)){
		res@rec.minyr    <- as.integer(rec.minyr)
		}
	if (!missing(rec.maxyr)){
		res@rec.maxyr    <- as.integer(rec.maxyr)
		}
    if (rec.minyr>rec.maxyr)
		stop("rec.minyr>rec.maxyr")
	if (!missing(catch.penalty)){
		res@catch.penalty    <- as.logical(catch.penalty)
		}
	if (!missing(catch.sigma)){
		res@catch.sigma <- as.numeric(catch.sigma)
		}
	if (!missing(catch.pdf)){
		res@catch.pdf   <- as.character(catch.pdf)
		}

	# Verify that this object is valid
	test <- validObject(res)
	if (!test)
		stop("Invalid object:", test)
      }
   return(res)
}

read.FLAdaptControl <- function(file=""){
	
	set.param<-function(vals){
		param<-data.frame(cbind(lower =vals[,1],
								best  =vals[,2],
								upper =vals[,3],
								method=vals[,4],
								sigma =vals[,5],
								n     =1))

		param
		}

	res <- .Call("readFLAdaptControl", file);
	
	res@param.termage<-set.param(res@param.termage)
	res@param.fratio <-set.param(res@param.fratio)
	res@param.srr    <-set.param(res@param.srr)
	res@param.var    <-set.param(res@param.var)
	res@param.q      <-set.param(res@param.q)

		
	return(res)}
