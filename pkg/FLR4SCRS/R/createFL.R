#### Exported in FLCore NAMESPACE
#readVPA2Box
#readVPAFile
#readFLIndex
#read.FLIndex
#readFLIndices
#read.FLIndices
#readFLStock
#read.FLStock
#readMFCL
#
##### New ones
#createFLStock
#createFLStocks
#createFLIndex
#createFLIndices
##
#Internal routines
############## io.FLindices
#writeIndicesVPA(FLIndices., file.)
#writeIndicesICA(FLIndices., file., ssb.)
#readIndicesVPA(file., sep="", quiet=TRUE, cchar='#', na.strings=na.strings)
#readIndicesAdapt(file.,na.strings="NA")
#readIndicesCSA(file.,na.strings="NA")
#readIndicesICA(file, file2, sep="", na.strings=na.strings)
#readIndicesICA.ssb(file., sep="",na.strings=na.strings)
#readFLIndices(file, file2, type="VPA", index.names, descs,
#readFLIndex(file, type="VPA", index.names, descs,
#set.index(smry.,index.,p.,l.,range)
#read.FLIndex(...)
#read.FLIndices(...)
#
### io.FLStock.R
#readFLStockfunction(file, type = "VPA", name, desc = paste("Imported from a")
#readAdaptFile(file., m. = m)
#readCSAFile(file.)
#readPAFile(file.)
#readVPAFile(file, sep = "", units = "NA", quiet = TRUE)
#readVPA(file, sep = "", quiet=TRUE)
#writeFLStock(FLStock, output.file=FLStock@name, type="VPA")
#read.FLStock(...)
#
#
##### New code
### ASPIC
#getBio(file)
#getPRB(filePrb)
#
### VPA2Box
#readRefpts(x,data.frame=FALSE)
#readPro2Sta(x,file="SSBIO-1.STA",flq=FALSE)
#readPro2Out(x,minyear=1,flq=FALSE)
#readFLStock(x,y,m=NA,mat=NA,swt=NULL,cwt=swt,m.spwn=NA,harvest.spwn=m.spwn,nits=501)
#diags(x)
#readBinary(x,dmns=list(),size=4)
#readVPA2BoxOld(data, results, m=as.numeric(NA), no.discards=TRUE)
#getFLQ(filename,pos1, pos2)
#getM(filename, ages)
#getNF(filename)
#readVPA2Box(data, results, m=as.numeric(NA), no.discards=TRUE)
#getNF(filename)
#getFLQ(filename,pos1, pos2)
#getM(filename, ages)
#
#
## Multifan-CL
#readMFCL(repfile, parfile)


#### Utility functions
legalFileType<-function(type,valid=toupper(c("adapt","vpa2box","vpa","mfcl")))
    if (!(type %in% valid)) cat(type, "is not a valid file type, valid types are",valid,"\n")

getDir<-function(file){
  if (!grepl(.Platform$file.sep,file)) res<-getwd()
  else                                 res<-substr(file,1,max(gregexpr(.Platform$file.sep,file)[[1]])-1)

  return(res)}

getFile<-function(file){
  res<-substr(file,max(gregexpr(.Platform$file.sep,file)[[1]])+1,nchar(file))

  return(res)}

skip.hash<-function(i,file) {
    i<-i+1
    while (substr(scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)=="#")
        i<-i+1

    return(i)}

skip.until.hash<-function(i,file) {
    i<-i+1
    while (substr(scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)!="#")
        i<-i+1

    return(i)}

## read in from VPA2Box output "R" files
posFile<-function(i,filename,char="-"){
   while (TRUE){
     firstChar<-substr(scan(filename, skip = i, nlines = 1, what = ("character"), quiet = TRUE)[1], 1, 1)

     if (!is.na(firstChar))
        if (firstChar == char) break

     i<-i+1}

   return(i)}

getFLQ<-function(filename,pos1, pos2){
    nyrs <-pos2-pos1-1
    t.   <-scan(filename, skip = pos1+1, nlines=nyrs, quiet = TRUE)
    nages<-length(t.)/nyrs
    t.   <-array(t.,c(nages,nyrs))

    yrs <-array(t.,c(nages,nyrs))[1,]
    ages<-scan(filename, skip = pos1-1, nlines=1, quiet = TRUE)

    flq<-FLQuant(t.[-1,],dimnames=list(age=ages,year=yrs))

    return(flq)}

vpa2BoxFiles<-function(file){
   i <- skip.hash(0,file)
   j <- skip.until.hash(i,file)

   gsub(" ","",gsub("'","",substr(scan(file,skip=i+1,nlines=j-i-1,quiet=TRUE,what=character(),sep="\n"),1,20)))}

setGeneric("createFLStock",   function(x,type,...)
	standardGeneric("createFLStock"))
setGeneric("createFLStocks",  function(x,type,...)
	standardGeneric("createFLStocks"))

setMethod("createFLStock",   signature(x="character",type="character"),
    function(x,type,
             name ="",
             desc = paste("Imported from a", type, "file. (", x, "). ", date()),
             quant="age",no.discards=FALSE,
             quiet=TRUE, sep="",nits=501,...){

    if (!all(file.exists(x))) stop(cat(file[!file.exists(x)],"does not exist \n"))
    type<-toupper(type)
    legalFileType(type)

	  res<-switch(type,
		            "ADAPT"  =readVPA2Box(x,nits),
                "VPA2BOX"=readVPA2Box(x,nits),
                "VPA"    =readFLStock(x,type,name,desc,quant,quiet,no.discards,sep),
 		            "MFCL"   =readMFCL(x[1], x[2]))

    return(res)})

setMethod("createFLStocks",  signature(x="character",type="character"),
function(x,type,n,...){
    type<-toupper(type)
    legalFileType(type,valid=toupper(c("adapt","vpa2box")))

    if (!all(file.exists(x))) stop(cat(file[!file.exists(x)],"does not exist \n"))

    vpa2BoxRetros<-function(x){
      dir <-getDir(x)
      file<-vpa2BoxFiles(x)[3]

      stk   <-readVPA2Box(x,nits=501)
      fileNm<-substr(file,1,max(gregexpr("\\.",file)[[1]])-2)

      getRetros(dir,stk,fileNm,n)}

	  res<-switch(type,
		            "ADAPT"  =vpa2BoxRetros(x),
                "VPA2BOX"=vpa2BoxRetros(x))

    return(res)})

readFLStock <- function (file, type="VPA", name,
                         desc = paste("Imported from a", type, "file. (", file, "). ", date()),
                         quant="age",no.discards=FALSE,quiet=TRUE, sep=""){
    
    ow <- options()$warn
    options(warn = -1)
    on.exit(options(warn = ow))
    res <- switch(type,
                  VPA   = readVPA(      file, quiet=quiet, sep=sep),
                  ADAPT = readAdaptFile(file),
                  PA    = readPAFile(   file),
                  CSA   = readCSAFile(  file),
                  stop("type must be either 'VPA', 'ADAPT', 'PA' or 'CSA'!"))

    Mat <- res@stock.wt
    Mat[, , , , ] <- NA
    Dim <- dim(Mat)
    Mat0 <- Mat
    Mat0[, , , , ] <- 0
    
    if (is.null(res@landings.n)  || !all(dim(res@landings.n)  == Dim))
        res@landings.n  <- Mat
    if (is.null(res@landings.wt) || !all(dim(res@landings.wt) == Dim))
        res@landings.wt <- Mat
    if (is.null(res@catch.n)     || !all(dim(res@catch.n)     == Dim))
        res@catch.n     <- Mat
    if (is.null(res@catch.wt)    || !all(dim(res@catch.wt)    == Dim))
        res@catch.wt    <- Mat
    if (is.null(res@discards.n)  || !all(dim(res@discards.n)  == Dim))
        res@discards.n  <- Mat
    if (is.null(res@discards.wt) || !all(dim(res@discards.wt) == Dim))
        res@discards.wt <- Mat
    if (is.null(res@m)           || !all(dim(res@m)           == Dim))
        res@m           <- Mat
    if (is.null(res@stock.wt)    || !all(dim(res@stock.wt)    == Dim))
        res@stock.wt    <- Mat
    if (is.null(res@mat)         || !all(dim(res@mat)         == Dim))
        res@mat         <- Mat
    if (is.null(res@stock.n)     || !all(dim(res@stock.n)     == Dim))
        res@stock.n     <- Mat
    if (is.null(res@harvest)           || !all(dim(res@harvest)     == Dim))
        res@harvest     <- Mat
    if (is.null(res@harvest.spwn)      || !all(dim(res@harvest.spwn)== Dim))
        res@harvest.spwn<- Mat
    if (is.null(res@m.spwn)      || !all(dim(res@m.spwn)      == Dim))
        res@m.spwn      <- Mat
    Mat <- Mat[1, , , , ]

    if (is.null(res@catch) || !all(dim(res@catch) == dim(Mat)))
        res@catch <- Mat
    if (is.null(res@discards) || !all(dim(res@discards) == dim(Mat)))
        res@discards <- Mat
    if (is.null(res@landings) || !all(dim(res@landings) == dim(Mat)))
        res@landings <- Mat
    pars <- dims(res@stock.wt)
    res@range <- unlist(list(min = pars$min, max = pars$max,
        plusgroup = NA, minyear = pars$minyear, maxyear = pars$maxyear,minfbar = pars$min, maxfbar = pars$max))
    if (length(res@name) < 1 | !missing(name))
        res@name <- as.character(name)
    if (!is.null(desc))
        res@desc <- as.character(desc)

    names. <- names(getSlots(class(res))[getSlots(class(res))=="FLQuant"])

    for (s. in names.)
        quant(slot(res, s.)) <- quant

   stock(res) <- computeStock(res)

    if(no.discards){
      discards(res)     <-0
      discards.n(res)   <-0
      discards.wt(res)  <-0
      catch(res)        <-computeCatch(res, 'all')}

    return(res)}

readVPA <-function(file, sep = "", quiet=TRUE) {

    if (!file.exists(file)){
        if(quiet==TRUE) stop()
        if(quiet!=TRUE) stop(paste("VPA index file", file, "does not exist"))}

    dir <- dirname(file)
    files. <- scan(file, what = "character", skip = 2, sep = sep, quiet=quiet)
    files. <- file.path(dir, files., fsep = .Platform$file.sep)

    range1 <- scan(files.[1], skip = 2, nlines = 1, sep = sep, quiet=quiet)
    range2 <- scan(files.[1], skip = 3, nlines = 1, sep = sep, quiet=quiet)
    range  <- c(range1[1:2],range2[1:2])

    ages <- range[3:4]
    yrs <- range[1:2]

    FLStock. <- FLStock(catch.n=FLQuant(NA, dimnames = list(age = ages[1]:ages[2], year = yrs[1]:yrs[2], unit = "unique", season = "all",  area = "unique")))

    for (i in files.) {
        if (!file.exists(i)){
           if(quiet != TRUE) cat("File ", i, "does not exist", "\n")
           }
        if (file.exists(i)) {
            a.   <-  readVPAFile(i, sep=sep, quiet=quiet)

            switch(as.character(scan(i, skip = 1, nlines = 1, sep = sep, comment.char='#', quiet=TRUE)[2]),
            "1" = FLStock.@landings    <-a.,
            "2" = FLStock.@landings.n  <-a.,
            "3" = FLStock.@landings.wt <-a.,
            "4" = FLStock.@stock.wt    <-a.,
            "5" = FLStock.@m           <-a.,
            "6" = FLStock.@mat         <-a.,
            "7" = FLStock.@harvest.spwn<-a.,
            "8" = FLStock.@m.spwn      <-a.,
            "21"= FLStock.@discards    <-a.,
            "22"= FLStock.@discards.n  <-a.,
            "23"= FLStock.@discards.wt <-a.,
            "24"= FLStock.@catch       <-a.,
            "25"= FLStock.@catch.n     <-a.,
            "26"= FLStock.@catch.wt    <-a.,
            "27"= FLStock.@harvest     <-a.,
            "28"= FLStock.@stock.n     <-a. )
        }
    }

    FLStock.@range <- c(min = ages[1], max = ages[2],
        plusgroup = ages[2], minyear = yrs[1], maxyear = yrs[2])
    FLStock.@desc <- paste("Imported from a VPA file (",
        file, "). ", date(), sep="")
    FLStock.@name <- scan(file, nlines = 1, what = character(0),
        sep = "\n", quiet=TRUE)

    return(FLStock.)}
    
#### reads binary files output by VPA2Box & Pro2Box
readBinary<-function(x,dmns=list(),size=4){

         ## Specify dims
         if ( "iter" %in% names(dmns))
             dmns<-list(year=dmns$year,age=dmns$age,unit="unique",season="all",area="unique",iter=dmns$iter) else
             dmns<-list(year=dmns$year,age=dmns$age,unit="unique",season="all",area="unique",iter=1)

         ## Get binary data
         res <-readBin(x,what=double(),size=size,prod(unlist(lapply(dmns,length))))

         ## create array and swap year & age
         res <-array(res,lapply(dmns,length),dmns)
         res <-FLQuant(aperm(res,c(2,1,3,4,5,6)))

         return(res)}

## reads in data from VPA2Box files
readVPA2Box<-function(x,nits=501,...){
    files<-vpa2BoxFiles(x)
    
    dir<-getDir(x)
    dat<-scan(paste(dir,.Platform$file.sep,files[5],sep=""),what="",sep="\n")
    dat<-dat[nchar(dat)>0]

    ln <-c(F=grep("F",dat)[1],
           N=grep("N",dat)[1],
           C=grep("C",dat)[1],
           W=grep("W",dat)[1],
           I=grep("I",dat)[2])

    aaIn<-function(aa,minage=1){
           aa<-aa[nchar(aa)>1]
           N <-length(aa)
           aa<-unlist(strsplit(aa," +"))
           aa<-aa[nchar(aa)>0]

           dms<-c(length(aa)/N,N)

           aa<-array(as.numeric(aa),dim=dms)

           FLQuant(c(aa[-1,]),dimnames=list(age=minage+(0:(dms[1]-2)),year=aa[1,]))}

      getFLQuant<-function(x,dimnames=NULL){
          ## assume is a file name
          if (is.character(x))  return()  else
          if (is(x, 'FLQuant')) return(x) else
          if(is.vector(x))
            if(length(x) == 1)
              return(FLQuant(x, dimnames=dimnames))
            else{
              if (length(dimnames[[1]])!=length(x)) warning("vector being recycled in FLQuant creator")
              return(FLQuant(x, dimnames=dimnames))}
          else if(is.data.frame(x))
              return(as.FLQuant(x))
          else if(is.matrix(x))
              stop("not implemented yet")
          else if(is.array(x))
              stop("not implemented yet")}

   stk<-FLStock(stock.n=aaIn(dat[(ln[2]+1):(ln[3]-1)]))

   harvest.   =aaIn(dat[(ln[1]+1):(ln[2]-1)])
   landings.n.=aaIn(dat[(ln[3]+1):(ln[4]-1)])
   stock.wt.  =aaIn(dat[(ln[4]+1):(ln[5]-1)])
   harvest(   stk) =harvest.

   landings.n(stk) =landings.n.
   stock.wt(  stk) =stock.wt.
   discards.wt(stk)[]<-0

   dmns     <-dimnames(stock.n(stk))
   dmns$iter<-1:nits

   if (file.exists(paste(dir,"MAA.OUT",sep="/")))
      m(stk)      <-readBinary(paste(dir,"MAA.OUT",sep="/"), dmns)
   if (file.exists(paste(dir,"FAA.OUT",sep="/")))
      harvest(stk)<-readBinary(paste(dir,"FAA.OUT",sep="/"), dmns)
   if (file.exists(paste(dir,"NAA.OUT",sep="/")))
     stk@stock.n  <-readBinary(paste(dir,"NAA.OUT",sep="/"), dmns)
   if (file.exists(paste(dir,"CAA.OUT",sep="/")))
     catch.n(stk) <-readBinary(paste(dir,"CAA.OUT",sep="/"), dmns)
   else
     catch.n(stk)     <-stock.n(stk)*harvest(stk)/(harvest(stk)+m(stk))*(1-exp(-((harvest(stk)+m(stk)))))

   units(harvest(stk))<-"f"

   catch.n(stk)     <-landings.n(stk)
   discards.n(stk)[]<-0

   if (file.exists(paste(x,"WAA.OUT",sep="/"))){
     stock.wt(   stk)<-readBinary(paste(x,"WAA.OUT",sep="/"),dimnames(stock.n(stk)))
     catch.wt(   stk)<-stock.wt(stk)
     landings.wt(stk)<-stock.wt(stk)
     discards.wt(stk)<-stock.wt(stk)}

   wt(stk)          <-stock.wt(stk)

   ## replace any slots
   args <- list(...)
   slt<-names(getSlots("FLStock"))[getSlots("FLStock")=="FLQuant"]
   for(i in names(args)[names(args) %in% slt]){
      slot(stk, i)<-args[[i]]}

   catch(stk)<-computeCatch(      stk,"all")
   landings(stk)<-computeLandings(stk)
   discards(stk)<-computeDiscards(stk)

   units(harvest(   stk))="f"

#   units(stk)<-units

   return(stk)}

## create retro stocks
getRetros<-function(dir,stk,fileNm,n){
    stks<-FLStocks()

    for (iRetro in 0:n){
       ## Start reading file
       filename<-paste(dir,.Platform$file.sep,fileNm,iRetro,".R",sep="")

       ## get Retro estimates
       i<-0
       pos1             <-posFile(i,filename)
       pos2             <-posFile(pos1,filename,char="=")
       harvest          <-getFLQ(filename,pos1, pos2)

       pos1             <-posFile(pos2,filename)
       pos2             <-posFile(pos1,filename,char="=")
       stock.n          <-getFLQ(filename,pos1, pos2-1)

       pos1             <-posFile(pos2,filename)
       pos2             <-posFile(pos1,filename,char="=")
       catch.n          <-getFLQ(filename,pos1, pos2)

       stks[[iRetro+1]]<-window(stk,end=dims(harvest)$maxyear)

       harvest(   stks[[iRetro+1]])<-harvest
       stock.n(   stks[[iRetro+1]])<-stock.n
       catch.n(   stks[[iRetro+1]])<-catch.n
       landings.n(stks[[iRetro+1]])<-catch.n
       discards.n(stks[[iRetro+1]])[]<-0
       units(harvest(stks[[iRetro+1]]))<-"f"

       catch(   stks[[iRetro+1]])<-computeCatch(   stks[[iRetro+1]],'all')
       landings(stks[[iRetro+1]])<-computeLandings(stks[[iRetro+1]])
       discards(stks[[iRetro+1]])<-computeDiscards(stks[[iRetro+1]])}

    return(stks)}

################################################################################
#                                                                              #
################################################################################

createRefpts<-function(x,data.frame=FALSE){
                          #F        #Yield    #YPR        #Recruits  #SPR       #SSB
      nmsRef <- c("Iter",	"fmsy",	  "msy",	  "yrmsy",	  "srmsy",	 "sprmsy",	"bmsy",
                          "fmax",	            "yrmax",	  "srmax",	 "sprmax",  "ssbmax",
                          "f0.1",	            "yr0.1",	  "sr0.1",	 "spr0.1",	"ssb0.1",
                          "f20",	            "yr20",	    "sr20",	              "ssb20",
                          "f30",	            "yr30",	    "sr30",	              "ssb30",
                          "f40",	            "yr40",	    "sr40",	              "ssb40",
                          "f90max",	"y90max",	"yr90max",	"sr90max",            "ssb90max",
                          "f75max", "y75max", "yr75max",	"sr75max",	          "ssb75max")

     res<-read.table(x,skip=1)
     names(res)<-nmsRef

     rfpts<-refpts(NA,refpt=c("msy","fmax","f0.1","spr.20","spr.30","spr.40","fmax.90","fmax.75"),iter=max(res[,1]+1))

     ## MSY
     rfpts["msy","harvest",res[,1]]<-res[,"fmsy"]
     rfpts["msy","ssb",    res[,1]]<-res[,"bmsy"]
     rfpts["msy","yield",  res[,1]]<-res[, "msy"]
     rfpts["msy","rec",    res[,1]]<-res[,"bmsy"]/res[,"srmsy"]

     ## FMax
     rfpts["fmax","harvest",res[,1]]<-res[,"fmax"]
     rfpts["fmax","ssb",    res[,1]]<-res[,"ssbmax"]
     rfpts["fmax","rec",    res[,1]]<-res[,"ssbmax"]/res[,"srmax"]
     rfpts["fmax","yield",  res[,1]]<-res[,"yrmax"]*(res[,"ssbmax"]/res[,"srmax"])

     ## F0.1
     rfpts["f0.1","harvest",res[,1]]<-res[,"f0.1"]
     rfpts["f0.1","ssb",    res[,1]]<-res[,"ssb0.1"]
     rfpts["f0.1","rec",    res[,1]]<-res[,"ssb0.1"]/res[,"sr0.1"]
     rfpts["f0.1","yield",  res[,1]]<-res[,"yr0.1"]*(res[,"ssb0.1"]/res[,"sr0.1"])

     ## F 20% SPR
     rfpts["spr.20","harvest",res[,1]]<-res[,"f20"]
     rfpts["spr.20","ssb",    res[,1]]<-res[,"ssb20"]
     rfpts["spr.20","rec",    res[,1]]<-res[,"ssb20"]/res[,"sr20"]
     rfpts["spr.20","yield",  res[,1]]<-res[,"yr20"]*(res[,"ssb20"]/res[,"sr20"])

     ## F 30% SPR
     rfpts["spr.30","harvest",res[,1]]<-res[,"f30"]
     rfpts["spr.30","ssb",    res[,1]]<-res[,"ssb30"]
     rfpts["spr.30","rec",    res[,1]]<-res[,"ssb30"]/res[,"sr30"]
     rfpts["spr.30","yield",  res[,1]]<-res[,"yr30"]*(res[,"ssb30"]/res[,"sr30"])

     ## F 40% SPR
     rfpts["spr.40","harvest",res[,1]]<-res[,"f40"]
     rfpts["spr.40","ssb",    res[,1]]<-res[,"ssb40"]
     rfpts["spr.40","rec",    res[,1]]<-res[,"ssb40"]/res[,"sr40"]
     rfpts["spr.40","yield",  res[,1]]<-res[,"yr40"]*(res[,"ssb40"]/res[,"sr40"])

     ## FMax 90%
     rfpts["fmax.90","harvest",res[,1]]<-res[,"f90max"]
     rfpts["fmax.90","ssb",    res[,1]]<-res[,"ssb90max"]
     rfpts["fmax.90","rec",    res[,1]]<-res[,"ssb90max"]/res[,"sr90max"]
     rfpts["fmax.90","yield",  res[,1]]<-res[,"yr90max"]*(res[,"ssb90max"]/res[,"sr90max"])

     ## FMax 75%
     rfpts["fmax.75","harvest",res[,1]]<-res[,"f75max"]
     rfpts["fmax.75","ssb",    res[,1]]<-res[,"ssb75max"]
     rfpts["fmax.75","rec",    res[,1]]<-res[,"ssb75max"]/res[,"sr75max"]
     rfpts["fmax.75","yield",  res[,1]]<-res[,"yr75max"]*(res[,"ssb75max"]/res[,"sr75max"])

     if (data.frame) return(cast(as.data.frame(rfpts), refpt+iter~quantity, mean)) else return(rfpts)}

pro2Sta<-function(x,data.frame=FALSE){
       res<-read.table(x,skip=1,header=F)

       names(res)<-c("Scen","Year","lowerCL","Median","upperCL","mean","det","sd")

       if (!data.frame){
         res2<-data.frame("quant"=rep(c("lowerCL","Median","upperCL","mean","det","sd"),each=dim(res)[1]),
                          "data"  =unlist((res[,3:8])),
                          "year"  =rep(res[,"Year"],6),
                          "Scen"  =rep(res[,"Scen"],6),row.names=NULL)

         res<-dlply(res2, "Scen", function(x) as.FLQuant(x[,1:3]))
         names(res)<-unique(res2[,"Scen"])
         }

       return(res)}

pro2Out<-function(x,minyear=1,data.frame=FALSE){
    ### Input files
    prjO<-c("BIO_f-1.OUT",  ## Biomass ? by iteration and year
            "BIO_t-1.OUT",  ## Biomass ? by iteration and year
            "Fapex-1.OUT",  ## F Apex by iteration and year
            "RECRT-1.OUT",  ## Recuits by iteration and year
            "SSBIO-1.OUT",  ## SSB by iteration and year
            "SSNUM-1.OUT",  ## ? by iteration and year
            "YIELD-1.OUT")  ## Yield by iteration and year

       res1<-read.table(paste(x,prjO[1],sep="/"))
       res2<-read.table(paste(x,prjO[2],sep="/"))
       res3<-read.table(paste(x,prjO[3],sep="/"))
       res4<-read.table(paste(x,prjO[4],sep="/"))
       res5<-read.table(paste(x,prjO[5],sep="/"))
       res6<-read.table(paste(x,prjO[6],sep="/"))
       res7<-read.table(paste(x,prjO[7],sep="/"))

       res<-data.frame(scen     =rep(res1[,1],                        dim(res1)[2]-2),
                       iter     =rep(res1[,2],                        dim(res1)[2]-2),
                       year     =rep((1:(dim(res1)[2]-2))+(minyear-1),each=dim(res1)[1]  ),
                       biomFish =unlist(res1[,3:dim(res1)[2]]),
                       biomass  =unlist(res2[,3:dim(res1)[2]]),
                       fapex    =unlist(res3[,3:dim(res1)[2]]),
                       rec      =unlist(res4[,3:dim(res1)[2]]),
                       ssb      =unlist(res5[,3:dim(res1)[2]]),
                       ssn      =unlist(res6[,3:dim(res1)[2]]),
                       yield    =unlist(res7[,3:dim(res1)[2]]))

       if (!data.frame){
          res2<-array(list(),dim=c(length(unique(res[,"scen"])),7),
                                dimnames=list(scen=unique(res[,"scen"]),
                                              val =c("biomFish","biomass","fapex","rec","ssb","ssn","yield")))

          for (i in dimnames(res2)[2]){
             res3<-res[,c("scen","year","iter",i)]
             names(res3)[4]<-"data"

          res2[,i]<-dlply(res3, "scen", function(x) as.FLQuant(x[,c("iter","year","data")]))}
          return(res2)}

       return(res)}

#### Functions for input data from ASPIC #######################################
#### Historic series
getRetros
createRefpts
pro2Sta
pro2Out
aspicTS<-function(file){
   t.  <-scan(file,skip=4)
   nits<-scan(file,skip=1,nmax=1)
   yrs <-scan(file,skip=2,nmax=2)
   nyrs<-diff(yrs)
   nval<-nyrs*2+3

   yrs <-yrs[1]:yrs[2]

   b.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+2,     1:nits,function(x,y=nyrs+1) x:(x+y-1)))],dimnames=list(year=yrs,               iter=1:nits))
   f.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+4,1:nits,function(x,y=nyrs)   x:(x+y-1)))],dimnames=list(year=yrs[-length(yrs)], iter=1:nits))
   f.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*121+63,1:nits,function(x,y=59) x:(x+y-1)))],dimnames=list(year=1950:2008,iter=1:nits))

   bmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+1,     1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(         iter=1:nits))
   fmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+3,1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(         iter=1:nits))

   return(list(b=sweep(b.,6,bmsy,"/"),f=sweep(f.,6,fmsy,"/"),bmsy=bmsy,fmsy=fmsy))}

## Projections
aspicProj<-function(file){
        ## Stuff
        nits<-scan(file,skip=1,nmax=1)
        yrs <-scan(file,skip=2,nmax=2)
        t.  <-scan(file,skip=4)
        ncol<-yrs[2]-yrs[1]+2

        ## biomass
        first<-rep((1:nits-1)*ncol*2,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
        b.   <-FLQuant(t.[first],dimnames=list(year=yrs[1]:yrs[2],iter=1:nits))
        first<-((1:nits-1)*ncol*2)+1
        bmsy <-FLQuant(t.[first],dimnames=list(iter=1:nits))
        b.   <-sweep(b.,6,bmsy,"/")

        ## F
        first<-rep((1:nits-1)*ncol*2+ncol,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
        f.   <-FLQuant(t.[first],dimnames=list(year=yrs[1]:yrs[2],iter=1:nits))[,ac(yrs[1]:(yrs[2]-1))]
        first<-((1:nits-1)*ncol*2)+ncol+1
        fmsy <-FLQuant(t.[first],dimnames=list(iter=1:nits))
        f.   <-sweep(f.,6,fmsy,"/")

        return(list(f=f.,b=b.))}
################################################################################

read.mfcl<-function(x) createFLStock(x,"mfcl")