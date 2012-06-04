setGeneric('fit',   function(object,cpue,...)     standardGeneric('fit'))
setGeneric('boot',  function(object,cpue,...)     standardGeneric('boot'))
setGeneric('prj',   function(object,ctrl,...)     standardGeneric('prj'))

rcpp_hello_world_aspic  <- function(){.Call( "runAspic", PACKAGE = "aspic" )}
rcpp_hello_world_aspicp <- function(){.Call( "runAspicp", PACKAGE = "aspic" )}

setMethod('fit',    signature(object='aspic',cpue="FLIndex"),
  function(object, cpue=FLIndices(cpue), package=attributes(class(object))$package, exeNm="aspic", dir=tempdir())
      runExe(object=object,cpue=cpue,package=package, exeNm=exeNm, dir=dir))

setMethod('fit',    signature(object='aspic',cpue="FLIndices"),
  function(object, cpue,   package=attributes(class(object))$package, exeNm="aspic", dir=tempdir(),...)
      runExe(object=object,cpue=cpue,package=package, exeNm=exeNm, dir=dir,...))

runExe=function(object, cpue, package="aspic", exeNm=package, dir=tempdir()){
       
    ##### set up temp dir with exe for data files
    # Linux
    if (R.version$os=="linux-gnu") {
      exe <- paste(system.file("bin", "linux", package=package, mustWork=TRUE),exeNm, sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "/", sep="")

     # Wind0ws
     } else if (.Platform$OS.type == "windows") {
       exe <- paste(system.file("bin", "windows", package=package, mustWork=TRUE),
         paste(exeNm, ".exe", sep=""), sep="/")
       file.copy(exe, dir)
       path <- paste(dir, "\\", sep="")
    
    # Mac OSX
     }else 
       stop()
        
    oldwd <- getwd()
 
    # change wd to avoid exe case bug
    setwd(dir)

    chkIters=function(object,cpue){
        aN=dims(object)$iter
        uN=laply(cpue, function(x) dims(x)$iter)
        
        if (length(unique(uN))>2) stop("FLIndex must have either 1 or N iters")
        if (aN>1 & max(uN)>1 & aN!=max(uN)) stop("aspic and FLindex objects have different numbers of iters")
        
        N=max(c(aN,uN))
        
        if (N>1){
          stock(object) =propagate(stock( object),N)
          params(object)=propagate(params(object),N)
          object@ll     =rep(as.numeric(NA),N)
          }
    
        stock(object)=propagate(FLQuant(NA,dimnames=dimnames(catch(object))),dims(object)$iter)
        stock(object)=window(stock(object),end=max(as.numeric(dimnames(catch(object))$year))+1)
  
       return(object)}

    object=chkIters(object,cpue)
       
    for (i in seq(dims(object)$iter)){  
        m_ply(c("prn","rdat","bio","inp","fit","sum"), function(x)
           if (file.exists(paste(exeNm,".",x,sep=""))) system(paste("rm ",exeNm,".",x,sep="")))
        
        # create exe input files
        u=FLIndices(llply(cpue, function(x,i) iter(x,i), i=i)) 
        writeAspic(iter(object,i),u,what="FIT",niter=1,fl=paste(exeNm,".inp",sep=""))
    
        # run
        system(paste("./", exeNm, paste(" ",exeNm,".inp",sep=""),sep=""))
     
        rdat=dget(paste(exeNm,"rdat",sep="."))
        
        iter(stock(object),i)=as.FLQuant(transform(rdat$t.series[,c("year","B")],data=B)[c("year","data")])
        params(object)[c("msy","k","b0"),i]=rdat$estimates[c("MSY","K","B1.K")]
        object@ll[i]=rdat$diagnostics$obj.fn.value
        }

#     if (dims(object)$iter==1){
#        rtn=try(readAspic(paste(exeNm,"prn",sep=".")))
#        if (is.data.frame(rtn)) object@diags=rtn}
  
    setwd(oldwd)
   
    return(object)}
  
