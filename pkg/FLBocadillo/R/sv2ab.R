# sv2ab & ab2sv {{{
# calc steepness & virgin biomass from alpha & beta, given SSB per R at F=0
sv2ab <- function(steepness, vbiomass, spr0, model)
{
   bh.sv2ab<-function(steepness,vbiomass,spr0)
      {
      a <- vbiomass*4*steepness/(spr0*(5*steepness-1.0))
      b  <- a*spr0*(1.0/steepness - 1.0)/4.0

			res       <-rbind(a,b)
#			names(res)<-c("a","b")
			return(res)
			}

   rk.sv2ab<-function(steepness,vbiomass,spr0){
      b  <- log(5.0*steepness)/(vbiomass*0.8);
      a <- exp(b*vbiomass)/spr0;

			res       <-rbind(a,b)
#			names(res)<-c("a","b")
			return(res)
			}

   if (model=="bevholt") return(bh.sv2ab(steepness,vbiomass,spr0))
   if (model=="ricker")  return(rk.sv2ab(steepness,vbiomass,spr0))
}

#Get a & b from Steepness & virgin biomass
ab2sv<-function(a,b,spr0,model)
   {
   bh.ab2sv<-function(a,b,spr0){
			steepness <- a*spr0/(4*b+a*spr0)
			vbiomass  <- (spr0*a*(5*steepness-1))/(4*steepness)

			res       <-rbind(steepness,vbiomass)
#			names(res)<-c("steepness","vbiomass")
			return(res)
      }

   rk.ab2sv<-function(a,b,spr0){
			vbiomass <- log(spr0 * a)/b
			steepness<- 0.2*exp(b*(vbiomass)*0.8);

			res       <-rbind(steepness,vbiomass)
#			names(res)<-c("steepness","vbiomass")
			return(res)
      }

   if (model=="bevholt") return(bh.ab2sv(a,b,spr0))
   if (model=="ricker")  return(rk.ab2sv(a,b,spr0))
}
# }}}

if (!isGeneric("alphaBeta"))
  setGeneric("alphaBeta", function(x, ...)
	  standardGeneric("alphaBeta"))

setMethod('alphaBeta', signature(x='FLPar'),
 function(x, model,...){

    res<-sv2ab(x["s",,drop=T],x["v",,drop=T],x["spr0",,drop=T],model)

    res<-array(res,c(2,dim(x)[2]),dimnames=list(params=c("a","b"),dimnames(x)$iter))

    x[c("s","v"),]<-res

    dimnames(x)[[1]][1:2]<-c("a","b")
    
    return(x)
    })

setMethod('alphaBeta', signature(x='FLSR'),
 function(x,...){

    mdl<-SRModelName(x@model)
    
    if (mdl=="bevholt.sv") mdl<-"bevholt"  else
    if (mdl=="ricker.sv")  mdl<-"ricker"   else
    stop("model must be 'bevholt' or 'ricker'")

    return(alphaBeta(params(x),mdl))
    })

if (!isGeneric("steepVirgin"))
  setGeneric("steepVirgin", function(x, ...)
	  standardGeneric("steepVirgin"))

setMethod('steepVirgin', signature(x='FLPar'),
    function(x, model,...){

    res<-ab2sv(x["a",,drop=T],x["b",,drop=T],x["spr0",,drop=T],model)

    res<-array(res,c(2,dim(x)[2]),dimnames=list(params=c("s","v"),dimnames(x)$iter))

    x[c("a","b"),]<-res

    dimnames(x)[[1]][1:2]<-c("s","v")

    return(x)
    })

setMethod('steepVirgin', signature(x='FLSR'),
    function(x,...){

    model<-SRModelName(model(x))

    if (model=="bevholt.sv") model<-"bevholt"  else
    if (model=="ricker.sv")  model<-"ricker"   else
    stop("model must be 'bevholt' or 'ricker'")

    return(steepVirgin(params(x),model))
    })

setMethod('steepVirgin', signature(x='FLBRP'),
    function(x,...){

    model<-SRModelName(model(x))

    if (model=="bevholt.sv") model<-"bevholt"  else
    if (model=="ricker.sv")  model<-"ricker"

    if (!(model %in% c("ricker","bevholt")))
       stop("model must be 'bevholt' or 'ricker'")

    res<-ab2sv(params(x)["a",,drop=T],params(x)["b",,drop=T],spr0(x),model)

    return(res)
    })


    # ab {{{
setGeneric('ab', function(object, ...)
		standardGeneric('ab'))

setMethod('ab', signature(object='FLSR'),
  function(object, plusgroup=dims(object)$max, ...){

  if (!(SRModelName(model(object)) %in% c("bevholt.sv","ricker.sv")))
  return(object)

  dmns<-dimnames(params(object))
  dmns$params<-c("a","b")

  if (SRModelName(model(object)) == "bevholt.sv")
     model<-"bevholt"
  else if (SRModelName(model(object)) == "ricker.sv")
     model<-"ricker"

  par<-FLPar(sv2ab(params(object)["s",],params(object)["v",],params(object)["spr0",],model=model),dimnames=dmns)

  if (SRModelName(model(object)) == "bevholt.sv")
     model(object)<-bevholt()
  else if (SRModelName(model(object)) == "ricker.sv")
     model(object)<-ricker()

  params(object)<-par

  return(object)
  })  # }}}
