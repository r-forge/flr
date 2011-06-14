### ======================================================================================================
### write.FLICA.out - Used to generate a "pseudo-ica.out" file from FLICA
### ======================================================================================================
FLICA.out   <-  function(FLStock,FLIndices,FLICA,format="TABLE %i.") {
  #Check  validity of objects
  if(!validObject(FLStock) | !is.FLStock(FLStock)) {stop("\"FLStock\" argument is not a valid FLStock object")}
  if(!validObject(FLIndices) | !is.FLIndices(FLIndices)) {stop("\"FLIndices\" argument is not a valid FLIndices object")}
  if(!validObject(FLICA) | !is.FLICA(FLICA)) {stop("\"FLICA\" argument is not a valid FLICA object")}
  #Also check that the FLICA object corresponds to the FLStock and FLIndices object (somehow)
  
  #Initialise key values
  counter <- 1 
  opt     <- NULL
  
  #First go through the input data
  output.structure <- rbind(c("catch.n","CATCH IN NUMBER"),
                        c("catch.wt","WEIGHTS AT AGE IN THE CATCH"),
                        c("stock.wt","WEIGHTS AT AGE IN THE STOCK"),
                        c("m","NATURAL MORTALITY"),
                        c("mat","PROPORTION MATURE"),
                        c("harvest.spwn","FRACTION OF HARVEST BEFORE SPAWNING"),
                        c("m.spwn","FRACTION OF NATURAL MORTALITY BEFORE SPAWNING"))
  for(i in 1:nrow(output.structure)) {
    #First the title line, then the data
    opt <- c(opt, paste(sprintf(format,counter),output.structure[i,2]),"")
    opt <- c(opt, format.quant(slot(FLStock,output.structure[i,1])),"","")
    counter <- counter +1
  }  

  #Then the indices 
  opt <- c(opt, paste(sprintf(format,counter),"SURVEY INDICES"),"")
  for(idx in FLIndices) {
    #First the survey configuration, then the index values, then the variances
    opt <- c(opt, paste(idx@name,"- Configuration"),"")
    opt <- c(opt, paste("\"",idx@desc,"\"",sep="",collapse=" "))
    opt <- c(opt, capture.output(print(idx@range)))
    opt <- c(opt, paste("Index type :",idx@type),"")
    opt <- c(opt, paste(idx@name,"- Index Values"),"")
    opt <- c(opt, format.quant(idx@index),"")
    opt <- c(opt, paste(idx@name,"- Index Variance (Inverse Weights) "),"")
    opt <- c(opt, format.quant(idx@index.var),"")
  }  
  opt <- c(opt,"")
  counter <- counter +1
  
  #Now information about how the stock object is configured
  opt <- c(opt, paste(sprintf(format,counter),"STOCK OBJECT CONFIGURATION"),"")
  opt <- c(opt, capture.output(FLStock@range),"","")
  counter <- counter + 1

  #Now the configuration of FLICA
  opt <- c(opt, paste(sprintf(format,counter),"FLICA CONFIGURATION SETTINGS"),"")
  longest.name  <-  max(sapply(slotNames(FLICA@control),nchar))
  for(sltName in slotNames(FLICA@control)) {
    padded.name <-  paste(c(sltName,rep(" ",longest.name-nchar(sltName))),collapse="")
    opt <- c(opt,paste(padded.name,":",paste(slot(FLICA@control,sltName),collapse=" ")))}
  opt <- c(opt,"","")
  counter <- counter + 1
  
  #FLR and R Package Information
  descrips <- list(packageDescription("FLICA"),packageDescription("FLAssess"),packageDescription("FLCore"))
  descrips <- lapply(descrips, function(b) {
    rbind(paste("Package  :",b$Package),
          paste("Version  :",b$Version),
          paste("Packaged :",b$Packaged),
          paste("Built    :",b$Built),
          "")
  })
  descrip.str <- do.call(rbind,c(R.Version()$version.string,"",descrips))
  opt <- c(opt, paste(sprintf(format,counter),"FLR, R SOFTWARE VERSIONS"),"",
            capture.output(write.table(descrip.str,row.names=FALSE,quote=FALSE,col.names=FALSE)),"")
  counter <- counter + 1

  #Stock summary - the tricky one!
  opt <- c(opt, paste(sprintf(format,counter),"STOCK SUMMARY"),"")
  ss.df <-  cbind(year=format(as.data.frame(ssb(FLStock))$year),
                  recs=format(round(as.data.frame(FLStock@stock.n[1,])$data,0)),
                  tsb=format(round(as.data.frame(FLStock@stock)$data,0)),
                  ssb=format(round(as.data.frame(ssb(FLStock))$data,0)),
                  fbar=format(round(as.data.frame(fbar(FLStock))$data,4)),
                  landings=format(round(as.data.frame(FLStock@landings)$data,0)),
                  sop=format(round(as.data.frame(sop(FLStock,"landings"))$data,4)))
  ss.units  <-  c("",units(FLStock@stock.n),"","",units(fbar(FLStock)),units(FLStock@landings),"")
  ss.units  <-  ifelse(ss.units=="NA","",ss.units)
  ss.df <-  rbind(c("Year","Recruitment","TSB","SSB","Fbar","Landings","Landings"),
                  c("",paste("Age",dims(FLStock)$min),"","",paste("(Ages ",FLStock@range["minfbar"],"-",FLStock@range["maxfbar"],")",sep=""),"","SOP"),
                  ss.units,ss.df)                  
  ss.df <-  apply(ss.df,2,"format",justify="right")
  opt  <- c(opt,capture.output(write.table(ss.df,row.names=FALSE,quote=FALSE,col.names=FALSE)),"","")
  counter <- counter + 1

  #Now, the estimated f and N (taken from the stock object, rather than the ica object
  #so that short term forcasts can be included 
  output.structure <- rbind(c("harvest","ESTIMATED FISHING MORTALITY"),
                            c("stock.n","ESTIMATED POPULATION ABUNDANCE"))
  for(i in 1:nrow(output.structure)) {
    #First the title line, then the data
    opt <- c(opt, paste(sprintf(format,counter),output.structure[i,2]),"")
    opt <- c(opt, format.quant(slot(FLStock,output.structure[i,1])),"","")
    counter <- counter +1
  }  
  
  #And now, the rest of the outputs from the assessment
  output.structure <- rbind(c("survivors","SURVIVORS AFTER TERMINAL YEAR"),
                            c("sel","FITTED SELECTION PATTERN"),
                            c("catch.n","PREDICTED CATCH IN NUMBERS"),
                            c("catch.res","CATCH RESIDUALS"))
  for(i in 1:nrow(output.structure)) {
    #First the title line, then the data
    opt <- c(opt, paste(sprintf(format,counter),output.structure[i,2]),"")
    opt <- c(opt, format.quant(slot(FLICA,output.structure[i,1])),"","")
    counter <- counter +1
  }  


  #Predicted index values
  opt <- c(opt, paste(sprintf(format,counter),"PREDICTED INDEX VALUES"),"")
  for(i in 1:length(FLICA@index.hat)) {
    #First the survey configuration, then the index values, then the variances
    opt <- c(opt, names(FLICA@index.hat)[i],"")
    opt <- c(opt, format.quant(FLICA@index.hat[[i]]),"")
  }  
  opt <- c(opt,"")
  counter <- counter +1

  #Index residuals
  opt <- c(opt, paste(sprintf(format,counter),"INDEX RESIDUALS"),"")
  for(i in 1:length(FLICA@index.res)) {
    #First the survey configuration, then the index values, then the variances
    opt <- c(opt, names(FLICA@index.res)[i],"")
    opt <- c(opt, format.quant(FLICA@index.res[[i]]),"")
  }  
  opt <- c(opt,"")
  counter <- counter +1

  #Parameter estimates
  opt <- c(opt, paste(sprintf(format,counter),"FIT PARAMETERS"),"")
  opt <-  c(opt,capture.output(FLICA@param[,c(8,7,9,10)]),"")
  opt <- c(opt,"")
  counter <- counter + 1
  
  #And finish  
  return(opt)

}

ica.out   <-  function(...) {
  FLICA.out(...)
}


format.quant <- function(x) {
  #Drop extraneous dimensions
  mat.obj <-  drop(x@.Data)   
  #Check that we haven't "overdropped", and are left with at least quant vs year
  if(is.null(dim(mat.obj))) {
    mat.obj <-  array(as.vector(mat.obj),dim=dim(x@.Data)[1:2],dimnames=dimnames(x@.Data)[1:2])
  }
  
  #Write output
  opt <-  capture.output({
    cat(paste("Units  : ",units(x),"\n"))
    print.default(mat.obj,quote=FALSE,right=TRUE)
    })  #End  capture output
  
  return(opt)
    
}   #End function, end setMethod
