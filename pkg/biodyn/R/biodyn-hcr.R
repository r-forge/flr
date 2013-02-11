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
      val=(SSB%*%a) + b
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
                             msy  =    c(Ftar=TRUE, Btrig=TRUE, Fmin=TRUE,  Blim=TRUE),...) 
   hcr.(object,params,msy,...))

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
hockeyStick=function(object,target=c(0.7,0.7),limit=c(0.2,0.1),maxB=1){
  
  pts=rbind(cbind(refpt="Target",model.frame(rbind(bmsy(object)*target[1],
                                                   fmsy(object)*target[2]))),
            cbind(refpt="Limit", model.frame(rbind(bmsy(object)*limit[1],
                                                   fmsy(object)*limit[2]))))
  pts.=pts
  pts.[1,"bmsy"]=params(object)["k"]*maxB
  pts.[2,"bmsy"]=0
  pts.[,1]=c("")
  
  pts=rbind(pts.[1,],pts[1:2,],pts.[2,])
  
  names(pts)[2:3]=c("biomass","harvest")
  
  pts}

