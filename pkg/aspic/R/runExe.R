setGeneric('fit',   function(object,cpues,ctrl,...)     standardGeneric('fit'))
setGeneric('boot',  function(object,cpues,ctrl,...)     standardGeneric('boot'))
setGeneric('diags', function(object,cpues,ctrl,...)     standardGeneric('diags'))
setGeneric('aspicp',function(object,      ctrl,...)     standardGeneric('aspicp'))


rcpp_hello_world_aspic  <- function(){.Call( "runAspic", PACKAGE = "aspic" )}
rcpp_hello_world_aspicp <- function(){.Call( "runAspicp", PACKAGE = "aspic" )}


writeFn=function(aspic,cpues,ctrl,file) {
   writeAspicControl(aspic,cpues,ctrl,file)
   return()}
writePrj=function(aspic,cpues,ctrl,file) {
   writeAspicControl(aspic,ctrl,file)
   return()}
  
readAspic=function(dir) {
     file=paste(dir, "\\test.bio", sep="")
 
     return()}

readDiags=function(dir) {
     file=paste(dir, "\\test.bio", sep="")
 
     return()}

readBoot=function(dir) {
     file=paste(dir, "\\test.bio", sep="")
 
     return()}

readAspicp=function(dir) {
     file=paste(dir, "\\test.bio", sep="")
 
     return()}
     
#### Fits
setMethod('fit',    signature(object='aspic',cpues="cpues",ctrl="aspicControl"),
  function(object, cpues, ctrl,              package=attributes(class(object[[1]]))$package, exeNm=aspic, dir=tempdir(),...)
      runExe(list(aspic=object,cpues=cpues,ctrl=ctrl), 
             write  =writeFn,
             read   =readAspic,
             package="aspic", exeNm="aspic", dir,...))

setMethod('fit',    signature(object='aspic',cpues="cpue",ctrl="aspicControl"),
  function(object, cpues, ctrl,              package=attributes(class(object[[1]]))$package, exeNm=aspic, dir=tempdir(),...)
      runExe(list(aspic=object,cpues=cpues(cpues),ctrl=ctrl), 
             write  =writeFn,
             read   =readAspic,
             package="aspic", exeNm="aspic", dir,...))

setMethod('fit',    signature(object='aspic',cpues="cpues",ctrl="missing"),
  function(object, cpues, ctrl,              package=attributes(class(object[[1]]))$package, exeNm=aspic, dir=tempdir(),...)
      runExe(list(aspic=object,cpues=cpues,ctrl=ctrl(aspic)), 
             write  =writeAspic,
             read   =readFn,
             package="aspic", exeNm="aspic", dir,...))

setMethod('fit',    signature(object='aspic',cpues="cpue",ctrl="missing"),
  function(object, cpues, ctrl,              package=attributes(class(object[[1]]))$package, exeNm=aspic, dir=tempdir(),...)
      runExe(list(aspic=object,cpues=cpues(cpues),ctrl=ctrl(aspic)), 
             write  =writeFn,
             read   =readAspic,
             package="aspic", exeNm="aspic", dir,...))

runExe=function(object, write, read, package, exeNm, dir,...){
   
    ##### set up temp dir with exe for data files
    # Linux
    if (R.version$os=="linux-gnu") {
      # executable
      exe <- paste(system.file("bin", "linux", package=package, mustWork=TRUE),exeNm, sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "/", sep="")

    # Wind0ws
    } else if (.Platform$OS.type == "windows") {
      # executable
      exe <- paste(system.file("bin", "windows", package=package, mustWork=TRUE),
        paste(exeNm, ".exe", sep=""), sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "\\", sep="")
    
    # Mac OSX
    # or fail!
    }else 
      stop()
        
    oldwd <- getwd()
 
    # change wd to avoid exe case bug
    setwd(dir)

    # create exe input files
    object$file = paste(dir, "\\aspic.inp", sep="")
    do.call("write", object)
       
    # run
    print(paste(exeNm, "currently disabled picking up existing outputs"))
    ##system(paste("./", exeNm, sep=""))
 
    # read exe output files
    res=do.call("read", list(dir=dir))

    setwd(oldwd)
   
    return(res)}


writeAspicControl=function(aspic,cpues,ctrl,file="aspic.inp",boot=0){
    comment=rep("",22)
    comment[ 1]= "\n"                                                                                               
    comment[ 2]= "\n"                                                                                               
    comment[ 3]= "\n"                                                                                                            
    comment[ 4]= "\t212 ## Verbosity\n"                                                                                                                  
    comment[ 5]= "\t## Number of bootstrap trials, <= 1000\n"                                                                                     
    comment[ 6]= "\t## 0=no MC search, 1=search, 2=repeated srch; N trials\n"                                                                  
    comment[ 7]= "\t## Convergence crit. for simplex\n"                                                                                      
    comment[ 8]= "\t## Convergence crit. for restarts, N restarts\n"                                                                      
    comment[ 9]= "\t## Conv. crit. for F; N steps/yr for gen. model\n"                                                                    
    comment[10]= "\t## Maximum F when cond. on yield\n"                                                                                          
    comment[11]= "\t## Stat weight for B1>K as residual (usually 0 or 1)\n"                                                                         
    comment[12]= "\t## Number of fisheries (data series)\n"                                                                                           
    comment[13]= "\t## Statistical weights for data series\n"      
    comment[14]= "\t## B1/K (starting guess, usually 0 to 1)\n"                                                                                 
    comment[15]= "\t## MSY (starting guess)\n"                                                                                               
    comment[16]= "\t## K (carrying capacity) (starting guess)\n"                                                                             
    comment[17]= "\t## q (starting guesses -- 1 per data series)\n"
    comment[18]= "\t## Estimate flags (0 or 1) (B1/K,MSY,K,q1...qn)\n"                                                   
    comment[19]= "\t## Min and max constraints -- MSY\n"                                                                         
    comment[20]= "\t## Min and max constraints -- K\n"                                                                           
    comment[21]= "\t## Random number seed\n"                                                                                                    
    comment[22]= "\t## Number of years of data in each series\n" 

    cat(aspic@desc,comment[ 1],file=file,append=FALSE)
    cat(aspic@name,comment[ 2],file=file,append=TRUE)
    cat(model(object),conditioning(ctrl(object)),objFn(ctrl(object)),comment[ 3],file=file,append=TRUE)

    cat(comment[ 4],                       file=file,append=TRUE)
    cat(boot,                comment[ 5],  file=file,append=TRUE)
    cat(search(ctrl(object)),comment[ 6],                     file=file,append=TRUE)
    cat(ctrl@conv["simplex"],comment[ 7],         file=file,append=TRUE)
    cat(ctrl@conv["restart"],ctrl@nRestart,comment[ 8],         file=file,append=TRUE)
    cat(ctrl@conv["F"],      comment[ 9],         file=file,append=TRUE)
    cat(ctrl(aspC)@wt,comment[10],         file=file,append=TRUE)
    cat(1,comment[11],         file=file,append=TRUE)
    cat(1,comment[12],         file=file,append=TRUE)
    cat(1,comment[13],         file=file,append=TRUE)
    cat(1,comment[14],         file=file,append=TRUE)
    cat(1,comment[15],         file=file,append=TRUE)
    cat(1,comment[16],         file=file,append=TRUE)
    cat(1,comment[17],         file=file,append=TRUE)
    
    cat(ctrl@bounds[,"fit"],              comment[18],         file=file,append=TRUE)
    cat(ctrl@bounds["msy",c("min","max")],comment[19],         file=file,append=TRUE)
    cat(ctrl@bounds["msy",c("min","max")],comment[20],         file=file,append=TRUE)
    cat(aspic@rnd,                        comment[21],         file=file,append=TRUE)
    }

 
