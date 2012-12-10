setGeneric('fit',   function(object,...)     standardGeneric('fit'))

setMethod('fit',  signature(object='aspic'),
          function(object, package=class(object), exeNm="aspic", dir=tempdir())
              runExe(object=object,package=package, exeNm=exeNm, dir=dir))

runExe=function(object, package="aspic", exeNm=package, dir=tempdir()){
 
  if (any(is.na(catch(object)))){
       tmp=ddply(object@cpue, .(year), with, mean(catch,na.rm=TRUE))
       object@catch=as.FLQuant(tmp[,"V1"], dimnames=list(year=tmp[,"year"]))
       }
  
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

    chkIters=function(object){
        N=dims(object)$iter
        
        params(object)=FLPar(object@bounds[,"start"])
        
        if (N>1){
          stock(object) =propagate(stock( object),N)
          params(object)=propagate(params(object),N)
          object@objFn  =rep(as.numeric(NA),N)
          }
    
        stock(object)=propagate(FLQuant(NA,dimnames=dimnames(catch(object))),dims(object)$iter)
        stock(object)=window(stock(object),end=max(as.numeric(dimnames(catch(object))$year))+1)
  
       return(object)}

    object=chkIters(object)
       
    for (i in seq(dims(object)$iter)){  
        m_ply(c("prn","rdat","bio","inp","fit","sum"), function(x)
           if (file.exists(paste(exeNm,".",x,sep=""))) system(paste("rm ",exeNm,".",x,sep="")))
        
        # create exe input files
        .writeAspicInp(iter(object,i),what="FIT",niter=1,fl=paste(exeNm,".inp",sep=""))
    
        # run
        system(paste("./", exeNm, paste(" ",exeNm,".inp",sep=""),sep=""))
     
        rdat=dget(paste(exeNm,"rdat",sep="."))
        #rdat$estimates
     
        params(object)[c("b0","msy","k"),i]=rdat$estimates[c("B1.K","MSY","K")]
        params(object)[4:dim(params(object))[1],i]=rdat$estimates[8+seq(length(names(rdat$estimates))-12)]
     
        iter(stock(object),i)=as.FLQuant(transform(rdat$t.series[,c("year","B")],data=B)[c("year","data")])
        object@objFn[i]=rdat$diagnostics$obj.fn.value
        }

     if (dims(object)$iter==1){
        rtn=try(readAspic(paste(exeNm,"prn",sep=".")))
        if (is.data.frame(rtn)) object@diags=rtn}
  
    setwd(oldwd)
   
    return(object)}
  
