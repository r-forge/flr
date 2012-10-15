
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


