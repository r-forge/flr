#### Exported functions in FLCore NAMESPACE
#readVPA2Box
#readVPAFile
#readFLIndex
#read.FLIndex
#readFLIndicesls()
#read.FLIndices
#readFLStock
#read.FLStock
#readMFCL

## Internal routines
#### io.FLindices
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

### io.FLStock.R
#readFLStockfunction(file, type = "VPA", name, desc = paste("Imported from a")
#readAdaptFile(file., m. = m)
#readCSAFile(file.)
#readPAFile(file.)
#readVPAFile(file, sep = "", units = "NA", quiet = TRUE)
#readVPA(file, sep = "", quiet=TRUE)
#writeFLStock(FLStock, output.file=FLStock@name, type="VPA")
#read.FLStock(...)

##### New code #####################################################################################
setGeneric("readVPASuite", function(x,type,...) standardGeneric("readVPASuite"))
setGeneric("readVPA2Box",  function(x,type,...) standardGeneric("readVPA2Box"))
setGeneric("diagVPA2Box",  function(x,type,...) standardGeneric("diagVPA2Box"))
setGeneric("readPro2Box",  function(x,type,...) standardGeneric("readPro2Box"))
setGeneric("readASPIC",    function(x,type,scen,...) standardGeneric("readASPIC"))
setGeneric("readMFCL",     function(x,type,...) standardGeneric("readMFCL"))

setMethod("readVPASuite",  signature(x="character",type="character"),  function(x,type,no.discards,sep,...)           .readLow(    x,type,no.discards=FALSE, sep="",...))
setMethod("readVPA2Box",   signature(x="character"),                   function(x,...)                                .readVPA2Box(x,args=list(...)))
setMethod("diagVPA2Box",   signature(x="character"),                   function(x)                                    .diagVPA2Box(x))
setMethod("readPro2Box",   signature(x="character", type="character"), function(x,type,minyear=1,data.frame=TRUE,...) .readPro2Box(x,type,minyear,data.frame,...))

setMethod("readASPIC",     signature(x="character", type="missing",  scen="missing"),    function(x,type,scen,...)    .readASPIC(  x,type,...))
setMethod("readASPIC",     signature(x="character", type="character",scen="missing"),    function(x,type,scen,...)    .readASPIC(  x,type,...))
setMethod("readASPIC",     signature(x="character", type="character",scen="data.frame"), function(x,type,scen,...)    .readASPIC(  x,type,scen,...))
setMethod("readASPIC",     signature(x="character", type="character",scen="character"),  function(x,type,scen,...)    .readASPIC(  x,type,scen,...))

setMethod("readMFCL",      signature(x="character"),                   function(x,...)                                .readMFCL(   x,...))

#### VPA2Box #########################################################################################
getDir<-function(file){
  if (!grepl(.Platform$file.sep,file)) res<-getwd()
  else                                 res<-substr(file,1,max(gregexpr(.Platform$file.sep,file)[[1]])-1)

  return(res)}

getFile<-function(file) substr(file,max(gregexpr(.Platform$file.sep,file)[[1]])+1,nchar(file))
getExt <-function(file) substr(file,max(gregexpr("\\.",             file)[[1]])+1,nchar(file))

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

getNBootRetro<-function(file){

    tmp<-scan(file,what=character(),sep="\n")
    tmp<-unlist(lapply(strsplit(tmp[substr(tmp,1,1)!="#"]," +"), function(x) x[x!=""][1]))

    as.numeric(tmp[length(tmp)-1:2])}

## reads in data from VPA2Box files
.readVPA2Box<-function(x,args){ 
print(args)

  ## control file ##############################################################
  dir  <-getDir(x)
  files<-paste(dir,.Platform$file.sep,vpa2BoxFiles(x),sep="")
  
  nS  <-getNBootRetro(x)
  nits<-max(1,nS[2])
  nRet<-max(1,nS[1])
  ##############################################################################

  ## "csv" file ################################################################ 
  # data 
  dat<-scan(files[5],what="",sep="\n",strip.white=TRUE )
  dat<-dat[nchar(dat)>0]

  # gets line number for start of data  
  ln <-c(F=grep("F",dat)[1],
         N=grep("N",dat)[1],
         C=grep("C",dat)[1],
         W=grep("W",dat)[1],
         I=grep("I",dat)[2])
  
  # function to converts data in "csv" file into an FLQuant
  aaIn<-function(aa,minage=1){
    aa<-aa[nchar(aa)>1]
    N <-length(aa)
    aa<-unlist(strsplit(aa," +"))
    aa<-aa[nchar(aa)>0]

    dms<-c(length(aa)/N,N)

    aa<-array(as.numeric(aa),dim=dms)
    
    FLQuant(c(aa[-1,]),dimnames=list(age=minage+(0:(dms[1]-2)),year=aa[1,]))}

  stk<-FLStock(stock.n=aaIn(dat[(ln[2]+1):(ln[3]-1)]))

  harvest.     =aaIn(dat[(ln[1]+1):(ln[2]-1)])
  landings.n.  =aaIn(dat[(ln[3]+1):(ln[4]-1)])
  stock.wt.    =aaIn(dat[(ln[4]+1):(ln[5]-1)])
  harvest(stk) =harvest.

  landings.n( stk)   =landings.n.
  stock.wt(   stk)   =stock.wt.
  landings.wt(stk)   =stock.wt.
  discards.wt(stk)[]=0
  ##############################################################################
  
  ## data file #################################################################
  ## Year range
  i    <-0
  i    <-skip.hash(i,files[1])
  yrRng<-read.table(files[1],skip=i,nrows=1,sep="\n",colClasses="character",strip.white=TRUE)[[1,1]]
  yrRng<-gsub("\t"," ",yrRng)
  yrRng<-as.integer(strsplit(yrRng," +")[[1]][1:2])

  ## age range
  i     <-skip.hash(i,files[1])
  ageRng<-read.table(files[1],skip=i,nrows=1,sep="\n",colClasses="character",strip.white=TRUE)[[1,1]]
  ageRng<-gsub("\t"," ",ageRng)
  ageRng<-as.integer(strsplit(ageRng," \t+")[[1]][1:4])

  ## number of indices
  i<-skip.hash(i,files[1])
  read.table(files[1],skip=i,nrows=1,sep="\n")

  ## xxx.spwn
  i<-skip.hash(i,files[1])
  x.spwn<-read.table(files[1],skip=i,nrows=1,sep="\n",colClasses="character",strip.white=TRUE)[[1,1]]
  x.spwn<-gsub("\t"," ",x.spwn)
  x.spwn<-as.integer(strsplit(x.spwn," +")[[1]][1])
  x.spwn<-(x.spwn)/12
  m.spwn(stk)[]     =x.spwn
  harvest.spwn(stk) =m.spwn(stk)

  ## mat
  i     <-skip.hash(i,files[1])
  mat   <-read.table(files[1],skip=i,nrows=1,sep="\n",colClasses="character",strip.white=TRUE)[[1,1]]
  mat   <-gsub("\t"," ",mat)
  mat   <-as.integer(strsplit(mat," +")[[1]])
  
  mat(   stk)[]     = mat[1:dim(mat(stk))[1]]
  ##############################################################################

  ## Binary files ##############################################################
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

   ## replace any slots
   #args <- list(...)
   slt<-names(getSlots("FLStock"))[getSlots("FLStock")=="FLQuant"]
   for(i in names(args)[names(args) %in% slt]){
      if (args[[1]])

    if (all(c("numeric","vector") %in% is(args[[i]]))) args[[i]]<-FLQuant(args[[i]],dimnames=dimnames(m(stk)))

      slot(stk, i)<-args[[i]]}

   catch(stk)   <-computeCatch(      stk,"all")
   landings(stk)<-computeLandings(stk)
   discards(stk)<-computeDiscards(stk)

   units(harvest(   stk))="f"

   if (nRet>1)  stk<-getRetros(stk,files[3],n=nRet)

#   units(stk)<-units

   return(stk)}


# create retro stocks
getRetros<-function(stk,fileNm,n){
    stks<-FLStocks()

    dir   <-getDir( fileNm)
    fileNm<-getFile(fileNm)
    fileNm<-substr( fileNm,1,gregexpr("\\.",fileNm)[[1]]-2)

    for (iRetro in 0:n){
       ## Start reading file
       filename<-paste(dir,.Platform$file.sep,fileNm,iRetro,".r",sep="")

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


#### Pro2Box ############################################################################
.readPro2Box<-function(x,type=c("ref","sta","out"),minyear=1,data.frame=TRUE){

  res<-switch(type[1],
       "ref"=createRefpts(x,data.frame),
              "sta"=pro2Sta(     x,minyear,data.frame),
              "out"=pro2Out(     x,minyear,data.frame))

  return(res)}

createRefpts<-function(x,data.frame=FALSE){
                          #F        #Yield    #YPR        #Recruits  #SPR       #SSB
      nmsRef <- c("Iter", "fmsy",   "msy",   "yrmsy",   "srmsy",  "sprmsy", "bmsy",
                                "fmax",             "yrmax",   "srmax",  "sprmax",  "ssbmax",
                          "f0.1",             "yr0.1",   "sr0.1",  "spr0.1", "ssb0.1",
                          "f20",             "yr20",     "sr20",               "ssb20",
                          "f30",             "yr30",     "sr30",               "ssb30",
                          "f40",             "yr40",     "sr40",               "ssb40",
                          "f90max", "y90max", "yr90max", "sr90max",            "ssb90max",
                          "f75max", "y75max", "yr75max", "sr75max",           "ssb75max")

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

pro2Sta<-function(x,minyear=1,data.frame=TRUE){
    ### Input files
    prj0<-c("BIO_f-1.STA",  ## Biomass ? by iteration and year
            "BIO_t-1.STA",  ## Biomass ? by iteration and year
            "Fapex-1.STA",  ## F Apex by iteration and year
            "RECRT-1.STA",  ## Recuits by iteration and year
            "SSBIO-1.STA",  ## SSB by iteration and year
            "SSNUM-1.STA",  ## ? by iteration and year
            "YIELD-1.STA")  ## Yield by iteration and year
       
       getSta<-function(x,dir){ 
           res<-read.table(paste(dir,x,sep=.Platform$file.sep),skip=1,header=F)
           names(res)<-c("scen","year","lowerCL","Median","upperCL","mean","det","sd")
           return(res)}

       res<-mdply(prj0,getSta,dir=x)

       X1<-c("biomFish","biomass","fapex","rec","ssb","ssn","yield")
       names(X1)<-1:7
       res$X1<-X1[res$X1]
       names(res)[1]<-"quantity"

       if (!data.frame){

  tmp<-melt(res,id.vars=c("quantity","scen","year"))
         names(tmp)[4:5]<-c("quant","data")

         res2<-array(list(),dim=c(length(unique(res[,"scen"])),7),
                                dimnames=list(scen=unique(res[,"scen"]),
                                              val =c("biomFish","biomass","fapex","rec","ssb","ssn","yield")))

         tmp<-dlply(tmp,.(quantity,scen),function(x) as.FLQuant(x[,3:5]))
         k<-0
         for (i in dimnames(res2)[[1]])
            for (j in dimnames(res2) [[2]]){
               k<-k+1

               res2[[i,j]]<-tmp[[k]]}

          return(res2)}

       return(res)}

pro2Out<-function(x,minyear=1,data.frame=TRUE){
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

readVPA <- function(file,no.discards=FALSE, sep="",...){
    ow <- options()$warn
    options(warn = -1)
    on.exit(options(warn = ow))

    res <-readVPA(file, quiet=TRUE, sep=sep)
                  
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

   stock(res) <- computeStock(res)

    if(no.discards){
      discards(res) <- 0
      discards.n(res) <- 0
      discards.wt(res) <- 0
      catch(res) <- computeCatch(res, 'all')}

    units(harvest(res)) <- "f"
   
    return(res)}


readPA <- function(file.) {
    getmatrix <- function(file., start, nlines, yrs, ages) {
        m. <- t(as.matrix(read.table(file = file., skip = start - 
            1, row.names = 1, nrows = nlines, sep = ",",colClasses = "numeric")[, 
            (ages[1]:ages[2]) + 1 - ages[1]]))
        return(as.FLQuant(m. <- array(m., dim = c(ages[2] - 
            ages[1] + 1, yrs[2] - yrs[1] + 1), dimnames = list(ages[1]:ages[2], 
            yrs[1]:yrs[2]))))
    }
    range <- scan(file., skip = 2, nlines = 2, sep = ",")
    ages <- range[4:5]
    yrs <- range[1:2]
    FLStock. <- FLStock()
    FLStock.@m.spwn <- getmatrix(file., 6, yrs[2] - yrs[1] + 
        1, yrs, ages)
    FLStock.@harvest.spwn <- getmatrix(file., 7 + (yrs[2] - yrs[1] + 
        1), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@harvest <- getmatrix(file., 7 + 3 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
  harvest(FLStock.) <- "f"
    FLStock.@stock.wt <- getmatrix(file., 7 + 4 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@catch.wt <- getmatrix(file., 7 + 5 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@mat <- getmatrix(file., 7 + 6 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@m <- getmatrix(file., 7 + 7 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    yrs[2] <- yrs[2] + 1
    FLStock.@stock.n <- getmatrix(file., 6 + 2 * (yrs[2] - yrs[1] + 
        1), yrs[2] - yrs[1] + 2, yrs, ages)
    FLStock.@range <- c(minage = ages[1], maxage = ages[2], 
        plusgroup = ages[2], minyear = yrs[1], maxyear = yrs[2])
    FLStock.@desc <- "read in from PA file"
    FLStock.@name <- scan(file., nlines = 1, what = character(0))[1]
    return(FLStock.)}

readLow<-function(x,type=c("vpa","pa"),no.discards=FALSE, sep="",...){

  res<-switch(substr(tolower(type[1]),1,1),
       "v"=readVPA(x,discards,sep),
              "p"=readPA( x))

  ## replace any slots
  args <- list(...)
  slt<-names(getSlots("FLStock"))[getSlots("FLStock")=="FLQuant"]
   for(i in names(args)[names(args) %in% slt]){
      slot(res, i)<-args[[i]]}

  return(res)}

.readMFCL<-function(x) FLCore::readMFCL(x[1],x[2])


.diagVPA2Box<-function(x){
    tab5<-scan(x,what="",sep="\n")
    tab5<-tab5[grep("TABLE 5.",tab5): length(tab5)]

    pos  <-grep("Chi-sq. discrepancy=",tab5)
    nms  <-substr(tab5[pos-7],10,30)
    str  <-pos+5
    end  <-grep("Selectivities",tab5)-2

    fn<-function(x) {
        x<-unlist(strsplit(x," "))
        as.numeric(x[nchar(x)>0])}

    uDiag<-tab5[unlist(mapply(seq,str,end))]
    uDiag<-fn(uDiag)
    uDiag<-as.data.frame(t(array(uDiag,c(9,length(uDiag)/9))))
    uDiag<-data.frame(cpue=unlist(mapply(rep, nms,(end-str)+1)),uDiag,row.names=NULL)

    uDiag$cpue<-factor(uDiag$cpue)
    names(uDiag)<-c("cpue","year","obs","hat","rsdl","sd","q","obs2","hat2","chi2")

    uDiag$rsdl2<-c(uDiag$rsdl[-1],NA)
    uDiag[!duplicated(uDiag[,"cpue"]),"rsdl2"]<-NA

    fnQQ<-function(object){
       qq.          =qqnorm(c(object$rsdl),plot.it=FALSE)
       qqx          =qq.$x
       qqy          =qq.$y

       res<-data.frame(qqx=qqx,qqy=qqy)

       return(res)}

    uDiag<-data.frame(uDiag,fnQQ(uDiag)[,c("qqx","qqy")])


    qqLine<-function(obj){
           x<-obj$qqx
           y<-obj$qqy

           qtlx<-quantile(x,prob=c(0.25,0.75),na.rm=T)
           qtly<-quantile(y,prob=c(0.25,0.75),na.rm=T)

           a=(qtly[1]-qtly[2])/(qtlx[1]-qtlx[2])
           b=qtly[1]-qtlx[1]*a

           res<-c(a,b)
           names(res)<-NULL
           names(res)<-c("a","b")

           return(res)
           return(data.frame(y=x*res["a"]+res["b"]))}

    par<-qqLine(uDiag)
    uDiag$qqYhat<-uDiag$qqx*par["a"]+par["b"]

    return(uDiag)}


if (FALSE){ ctl<-scan("/home/lkell/Desktop/Stuff/data/inputs/pro2box/Prj10.ctl",sep="\n",what=character())
	    ctl<-subset(ctl,substr(ctl,1,1)!="#")
	    ctl<-gsub("\t+"," ",ctl)
	    ctl<-strsplit(unlist(ctl," +")," +")

	    lns<-list()

	    #### Files
	    lns[["Files"]] <-21:30
	    typeFile       <-c("txt","bin")
	    names(typeFile)<-0:1

	    typeFile[unlist(lapply(ctl[lns[["Files"]]], function(x) strsplit(x," +")[1]))]
	    unlist(lapply(ctl[lns[["Files"]]], function(x) strsplit(x," +")[2]))}



#### ASPIC #####################################################################################
#### Historic series
aspicTS<-function(file){
   t.  <-scan(file,skip=4)
   nits<-scan(file,skip=1,nmax=1)
   yrs <-scan(file,skip=2,nmax=2)
   nyrs<-diff(yrs)
   nval<-nyrs*2+3

   yrs <-yrs[1]:yrs[2]

   b.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+2,     1:nits,function(x,y=nyrs+1) x:(x+y-1)))],dimnames=list(year=yrs,               iter=1:nits))
   f.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+4,1:nits,function(x,y=nyrs)   x:(x+y-1)))],dimnames=list(year=yrs[-length(yrs)], iter=1:nits))
   
   bmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+1,     1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(                        iter=1:nits))
   fmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+3,1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(                        iter=1:nits))

   return(FLQuants(biomass=sweep(b.,6,bmsy,"/"),harvest=sweep(f.,6,fmsy,"/"),bmsy=bmsy,fmsy=fmsy))}

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

        return(FLQuants(harvest=f.,biomass=b.))}

readASPICAssess<-function(dir,scen){
    res   <-mdply(scen, function(x,dir) as.data.frame(readASPIC(paste(dir,"/",scen,".bio",sep=""))), dir=dir)
    ts    <-subset(res, qname %in% c("biomass","harvest"))
    refpts<-subset(res, qname %in% c("bmsy",   "fmsy"))

    ## Quantiles in data.frame
    qtls<-transform(melt(ddply(ts,.(X1,year,qname),function(x) quantile(x$data,prob=c(0.25,0.5,0.75))),id.vars=c("X1","qname","year")),
		      Scenario=factor(X1),Quantity=factor(qname), Year=year, Quantile=variable)[,c("scenario","quantity","year","qantile","value")]

    ## Model frame with points
    ts<-cast(subset(res, qname %in% c("biomass","harvest"),select=c("X1","year","iter","data","qname")), 
		  X1+year+iter~qname,value="data")
    names(ts)<-c("Scenario","Year","iter","biomass","harvest")

    return(list(ts=ts,quantiles=qtls,refpts=refpts))}

readASPICProj<-function(dir,scen){
    prj       <-subset(mdply(scen, function(scen,TAC,dir) as.data.frame(readASPIC(paste(dir,"/",scen,TAC,".prb",sep=""))), dir=dir), !is.na(data))   
    prj       <-cast(prj,scen+year+TAC+iter~qname,value="data") 
    names(prj)<-c("scenario","year","TAC","iter","biomass","harvest")

    prjP      <-cbind(prj,kobeP(prj$biomass,prj$harvest))

    prjP      <-ddply(prjP,.(scenario,year,TAC), function(x) cbind(f=mean(x$f,na.rm=T),b=mean(x$b,na.rm=T),p=mean(x$p,na.rm=T),collapsed=mean(x$collapsed)))
    return(prjP)}

.readASPIC<-function(x,type,scen="missing"){
  
  if (!missing(scen)){
     if (substr(tolower(type[1]),1,1)=="b") return(readASPICAssess(x,scen))
     if (substr(tolower(type[1]),1,1)=="p") return(readASPICProj(  x,scen))}
  
  if(missing(type)) type=getExt(x)

  res<-switch(substr(tolower(type[1]),1,1),
       "b"=aspicTS(  x),
       "p"=aspicProj(x))

  return(res)}
################################################################################

