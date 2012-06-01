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

    m_ply(c("prn","rdat","bio","inp","fit","sum"), function(x)
        if (file.exists(paste(exeNm,".",x,sep=""))) system(paste("rm ",exeNm,".",x,sep="")))

    # create exe input files
    writeAspic(object,cpue,what="FIT",niter=1,fl=paste(exeNm,".inp",sep=""))

    # run
    system(paste("./", exeNm, paste(" ",exeNm,".inp",sep=""),sep=""))
 
#    "info"        "inputs"      "diagnostics" "estimates"   "t.series"  

    rdat=dget(paste(exeNm,"rdat",sep="."))
    
    stock(object)=as.FLQuant(transform(rdat$t.series[,c("year","B")],data=B)[,c("year","data")])
    params(object)[c("msy","k","b0")]=rdat$estimates[c("MSY","K","B1.K")]

    object@diags=readAspic(paste(exeNm,"prn",sep="."))
  
    setwd(oldwd)
   
    return(object)}


