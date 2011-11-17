setMethod('fit', signature(object='FLAdapt'),
  function(object, package="adapt", exeNm="VPA-2BOX", dir=tempdir(),...) 
      runExe(object, package, exeNm, dir,...))

setMethod('fwd', signature(object='FLAdapt'),
  function(object, package="adapt", exeNm="pro-2box", dir=tempdir(),...) 
      runExe(object, package, exeNm, dir,...))

setMethod('boot', signature(object='FLAdapt'),
  function(object, package="adapt", exeNm="VPA-2BOX", dir=tempdir(),...) 
      runExe(object, package, exeNm, dir,...))

writeFn=function(object,exeNm="adapt") {
   return()}
  
readFn=function(object,exeNm="adapt") {

     return()}
      
runExe=function(object, package="adapt", exeNm="adapt", dir=tempdir(),...){
   
    ##### set up temp dir with exe for data files
    # Linux
    if (R.version$os=="linux-gnu") {
      # executable
      exe <- paste(system.file("bin", "linux", package=package, mustWork=TRUE),exeNm, sep="/")
      file.copy(exe, dir)
      path <- paste(dir, "/", sep="")

    # Windows
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
    do.call("writeFn", list(object=object))
         
    # run
    #system(paste("./", exeNm, sep=""))
   
    # read exe output files
    object=readFn(object=object)
          
    setwd(oldwd)
   
    residuals(object)[is.na(obs(object))]=NA
    
    return(object)}
 
