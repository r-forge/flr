setGeneric('runExe', function(object,cpues,...)
   standardGeneric('runExe'))

setMethod('runExe', signature(object='aspic',cpues='cpue'),
  function(object, cpues=cpues(cpues), package=class(object), exeNm="aspic", dir=tempdir()) 
    runExe(object, cpues, package=class(object), exeNm="aspic", dir=tempdir())) 
   
setMethod('runExe', signature(object='aspic',cpues='cpues'),
  function(object, cpues, package=class(object), exeNm="seine", cmdOps=paste("-maxfn 500"), dir=tempdir()) {

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
 
    object=chkIters(object)
    for (i in seq(dims(object)$iter)) {
        # change wd to avoid exe case bug
        setwd(dir)

        # create exe input files
        writeDat(iter(object, i))
       
        # run
        system(paste("./", exeNm, " ", cmdOps, sep=""))
 
        # read exe output files
        object=readRep(object, i)
        }
    
    setwd(oldwd)
   
    return(object)})
 