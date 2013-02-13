
#' The length of a string (in characters).
#'
#' @param An aspic object
#' @return An aspic object with parameter estimate
#' @seealso \code{\link{biodyn}}
#' @seealso \code{\link{boot}}
#' @seealso \code{\link{jk}}
#' @export
#' @examples
#' seq(10)
setMethod('fit',  signature(object='aspic'),
          function(object, package=class(object), exeNm="aspic", dir=tempdir(),jk=FALSE)
            runExe(object=object,package=package, exeNm=exeNm, dir=dir,jk=jk))


#' The length of a string (in characters).
#'
#' @param An aspic object
#' @return An aspic object with parameter estimate
#' @seealso \code{\link{biodyn}}
#' @seealso \code{\link{boot}}
#' @seealso \code{\link{fit}}
#' @export
#' @examples
#' seq(10)
setMethod('jk',  signature(object='aspic'),
          function(object, package=class(object), exeNm="aspic", dir=tempdir())
            runExe(object=object,package=package, exeNm=exeNm, dir=dir,jk=TRUE))

#' The length of a string (in characters).
#'
#' @param An aspic object
#' @return An aspic object with parameter estimate
#' @seealso \code{\link{biodyn}}
#' @seealso \code{\link{fit}}
#' @seealso \code{\link{jk}}
#' @method boot
#' @export
#' @examples
#' seq(10)
setMethod('boot', signature(object='aspic'),
          function(object, package=class(object), exeNm="aspic", dir=tempdir(),boot=500)
            runBoot(object=object,package=package, exeNm=exeNm, dir=dir,boot=boot))

utils::globalVariables(c("year","swon","year","B","obs"))


chkIters=function(object){
  N=max(dims(object)$iter,dims(object@control)$iter,dims(object@params)$iter)
  
  #params(object)=FLPar(object@control[,"val",drop=T])
  
  if (N>1){
    object@stock=propagate(stock( object),N)
    if (dims(object@params)[1]!=N) 
        object@params=propagate(object@params,N)
    object@objFn  =propagate(object@objFn,N)
  }
  
  object@stock=propagate(stock(object),dims(object)$iter)
  object@stock=window(stock(object),end=max(as.numeric(dimnames(catch(object))$year))+1)
  
  return(object)}

jkIdx=function(x) dimnames(x)[[1]][ !is.na(x$index)]

runExe=function(object,package="aspic",exeNm=package,dir=tempdir(),jk=FALSE){
 
  if (any(is.na(object@catch))){
       tmp=ddply(object@index, .(year), with, mean(catch,na.rm=TRUE))
       object@catch=as.FLQuant(tmp[,"V1"], dimnames=list(year=tmp[,"year"]))
       dmns=dimnames(object@catch)
       dmns$year=c(dmns$year,as.numeric(max(dmns$year))+1)
       object@stock=FLQuant(NA,dimnames=dmns)
       }
  
  #oldwd =setExe(exeNm,package,dir)
  oldwd=getwd()
  setwd(dir)
  biodyn:::exe("aspic")
  
  ## Jack knife if wished 
  j=1
  if (jk){
    object=propagate(object,length(jkIdx(object@index)))
    object@params=propagate(object@params,length(jkIdx(object@index)))
    j   = jkIdx(object@index)
    index=object@index}
 
    object=chkIters(object)
  
    for (i in seq(dims(object)$iter)){  
        m_ply(c("prn","rdat","bio","inp","fit","sum","rdatb","det","sum","bot"), function(x)
           if (file.exists(paste(exeNm,".",x,sep=""))) system(paste("rm ",exeNm,".",x,sep="")))
    
        if (jk){
               object@index=index
               object@index[j[i],"index"]=NA
               }
        
        # create exe input files
        .writeAspicInp(iter(object,i),what="FIT",niter=1,fl=paste(exeNm,".inp",sep=""))
    
        # run
        #system(paste("./", exeNm, paste(" ",exeNm,".inp",sep=""),sep=""))
        system(paste(exeNm, paste(" ",exeNm,".inp",sep=""),sep=""))
        
        rdat=dget(paste(exeNm,"rdat",sep="."))
        
        #rdat$estimates
        object@params[c("b0","msy","k"),i]=rdat$estimates[c("B1.K","MSY","K")]       
        object@params[4:dim(object@params)[1],i]=rdat$estimates[8+seq(length(names(rdat$estimates))-length(rdat$estimates)+1)]

        names(rdat$t.series)=tolower(names(rdat$t.series))
        iter(object@stock,i)=as.FLQuant(transform(rdat$t.series[,c("year","b")],data=b)[c("year","data")])
        
        if (.Platform$OS!="windows"){
        try(object@objFn[2,i]<-rdat$diagnostics$obj.fn.value)
        try(object@objFn[1,i]<-rdat$diagnostics$rsquare) }            
        }
  
    if (dims(object)$iter==1){
      rtn=try(readAspic(paste(exeNm,"prn",sep=".")))
      if (is.data.frame(rtn)) object@diags=rtn
    
      object@diags=transform(object@diags,stock.  =  hat/c(object@params[grep("q",dimnames(params(object))$params)])[name],
                                          stockHat=index/c(object@params[grep("q",dimnames(params(object))$params)])[name])
      object@diags=merge(object@diags,model.frame(mcf(FLQuants(stock=object@stock,harvest=harvest(object))),drop=TRUE),all=T)
      object@diags$stock=object@diags$stock.
      object@diags=object@diags[,-10]
      }
  
    setwd(oldwd)
   
    return(object)}
  
runBoot=function(object, package="aspic", exeNm=package, dir=tempdir(),boot=500){
  if (boot<3) {
      boot=3
      warning("Requires a minimum of 3 bootstraps so boot option changed")
      }

  ## add catch baed on index catches if missing
  if (any(is.na(object@catch))){
    tmp=ddply(object@index, .(year), with, mean(catch,na.rm=TRUE))
    object@catch=as.FLQuant(tmp[,"V1"], dimnames=list(year=tmp[,"year"]))
    }
  
  ## add catch baed on index catches if missing
  if (dim(object@control)[3] >1)    stop("control can only have iter dim of 1")
  if (dim(object@params)[2]==1)    object@params=propagate(object@params,boot)
  if (dim(object@params)[2]!=boot) stop("params iters either have to be 1 or same as number of boot")
  if (dims(object@catch)$iter>1)   stop("catch must only have 1 iter")

  dmns=dimnames(object@catch)
  dmns$year=c(dmns$year,as.numeric(max(dmns$year))+1)
  object@stock=propagate(FLQuant(NA,dimnames=dmns),boot)

  oldwd=biodyn:::setExe(package,exeNm,dir)
  
  m_ply(c("prn","rdat","bio","inp","fit","sum","rdatb","det","sum","bot"), function(x)
      if (file.exists(paste(exeNm,".",x,sep=""))) system(paste("rm ",exeNm,".",x,sep="")))
      
  # create exe input files
  .writeAspicInp(object,what="BOT",niter=boot,fl=paste(exeNm,".inp",sep=""))

  # run
  system(paste("./", exeNm, paste(" ",exeNm,".inp",sep=""),sep=""))

  det=aspicDet(paste(exeNm,"det",sep="."))
  bio=aspicBio(paste(exeNm,"bio",sep="."))

  coerceDP=function(x)  FLPar(unlist(c(t(x))),params=names(x),iter=dim(x)[1])
 
  parNms=c("b0",biodyn:::modelParams(tolower(model(object))))
   
  object@params[parNms,]=coerceDP(det[,parNms])
  qNms=names(det)[!(names(det) %in% c("iter","stock","harvest","r","trial","loss","msy","bmsy","brel","frel","b1.k",parNms))]

  object@params[-(seq(length(parNms)))][]=unlist(c(det[,qNms]))
  object@stock=bio[["stock"]]%*%bio[["bmsy"]]

  setwd(oldwd)
  
  
  return(object)}
