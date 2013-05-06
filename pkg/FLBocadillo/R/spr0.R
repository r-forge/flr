## calcs spawner per recruit at F=0.0
if (!isGeneric("spr0"))
  setGeneric("spr0", function(ssb, rec, fbar, ...)
	  standardGeneric("spr0"))

setMethod('spr0', signature(ssb='FLQuant', rec='FLQuant', fbar='FLQuant'),
   function(ssb, rec, fbar,log=TRUE){
   
    if  (any(dim(ssb)[3:5]>1)) "stop multiple units, seasons, areas not allowed yet"
    if  (any(dim(rec)[3:5]>1)) "stop multiple units, seasons, areas not allowed yet"
    if  (any(dim(fbar)[3:5]>1)) "stop multiple units, seasons, areas not allowed yet"

    # years: corrects length if mismatch
    minyear <- max(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) min(as.numeric(dimnames(x)$year)))))
    maxyear <- min(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) max(as.numeric(dimnames(x)$year)))))

    # ssb & f
    ssb  <- ssb[ 1, as.character(seq(minyear, maxyear)), drop=TRUE]
    rec  <- rec[ 1, as.character(seq(minyear, maxyear)), drop=TRUE]
    fbar <- fbar[1, as.character(seq(minyear, maxyear)), drop=TRUE]

    # spr0
    dmns     <-dimnames(ssb(x))
    dmns$year<-1
    res      <-FLQuant(NA,dimnames=dmns)

    if (log) {
       calcSpr0 <-function(x) exp(lm(log(c(ssb(x)/rec(x)))~c(fbar(x)))$coefficients[1])
       for (i in dmns$iter)
         res[,,,,,i]<-calcSpr0(iter(x,i))}
    else {
       calcSpr0 <-function(x) lm(c(ssb(x)/rec(x))~c(fbar(x)))$coefficients[1]
       for (i in dmns$year)
         res[,,,,,i]<-calcSpr0(iter(x,i))}

    return(res)})

setMethod('spr0', signature(ssb='FLStock', rec='missing', fbar='missing'),
  function(ssb){
    sr <-as.FLSR(ssb)
    res<-spr0(ssb=ssb(ssb), rec=rec(sr), fbar=fbar(ssb))

    return(res)})

setMethod('spr0', signature(ssb='FLSR', rec='missing', fbar='FLQuant'),
  function(ssb, fbar){
    res<-spr0(ssb=ssb(ssb), rec=rec(ssb), fbar=fbar)

    return(res)})

