# io.VPA2Box - «Short one line description»
# io.VPA2Box

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

readPro2Refpts<-function(x,data.frame=FALSE){
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

readPro2Sta<-function(x,file="SSBIO-1.STA",flq=FALSE){
       res<-read.table(paste(x,file,sep="/"),skip=1,header=F)

       names(res)<-c("Scen","Year","lowerCL","Median","upperCL","mean","det","sd")

       if (flq){
         res2<-data.frame("quant"=rep(c("lowerCL","Median","upperCL","mean","det","sd"),each=dim(res)[1]),
                          "data"  =unlist((res[,3:8])),
                          "year"  =rep(res[,"Year"],6),
                          "Scen"  =rep(res[,"Scen"],6),row.names=NULL)

         res<-dlply(res2, "Scen", function(x) as.FLQuant(x[,1:3]))
         names(res)<-unique(res2[,"Scen"])
         }
         
       return(res)}

readPro2Out<-function(x,minyear=1,flq=FALSE){
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
                       
       if (flq)
          {
          res2<-array(list(),dim=c(length(unique(res[,"scen"])),7),
                                dimnames=list(scen=unique(res[,"scen"]),
                                              val =c("biomFish","biomass","fapex","rec","ssb","ssn","yield")))

          for (i in dimnames(res2)[2]){
             res3<-res[,c("scen","year","iter",i)]
             names(res3)[4]<-"data"
             
          res2[,i]<-dlply(res3, "scen", function(x) as.FLQuant(x[,c("iter","year","data")]))}
          return(res2)}
          
       return(res)}

## reads in data from VPA2Box files
readFLStock<-function(x,y,m=NA,mat=NA,swt=NULL,cwt=swt,m.spwn=NA,harvest.spwn=m.spwn,nits=501){
    dat<-scan(paste(x,y,sep="/"),what="",sep="\n")
    dat<-dat[nchar(dat)>1]

    ln <-c(F=grep("F",dat)[1],
           N=grep("N",dat)[1],
           C=grep("C",dat)[1],
           W=grep("W",dat)[1],
           I=grep("I",dat)[2])

    aaIn<-function(aa,minage=1){
           N <-length(aa)
           aa<-unlist(strsplit(aa," "))
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

#   harvest.   =aaIn(dat[(ln[1]+1):(ln[2]-1)])
#   landings.n.=aaIn(dat[(ln[3]+1):(ln[4]-1)])
#   stock.wt.  =aaIn(dat[(ln[4]+1):(ln[5]-1)])
#   harvest(   stk) =harvest.
#   landings.n(stk) =landings.n.
#   stock.wt(  stk) =stock.wt.

   m.spwn(      stk)[]<-m.spwn
   harvest.spwn(stk)[]<-harvest.spwn
   
   if (!is.null(swt)) stock.wt(   stk)<-swt
   if (!is.null(cwt)) catch.wt(   stk)<-cwt
   if (!is.null(cwt)) landings.wt(stk)<-cwt
   discards.wt(stk)[]<-0

    
   m(  stk)<-getFLQuant(m,  dimnames=dimnames(m(  stk)))
   mat(stk)<-getFLQuant(mat,dimnames=dimnames(mat(stk)))

   dmns     <-dimnames(stock.n(stk))
   dmns$iter<-1:nits

   if (file.exists(paste(x,"MAA.OUT",sep="/")))
      m(stk)      <-readBinary(paste(x,"MAA.OUT",sep="/"), dmns)
   if (file.exists(paste(x,"FAA.OUT",sep="/")))
      harvest(stk)<-readBinary(paste(x,"FAA.OUT",sep="/"), dmns)
   if (file.exists(paste(x,"NAA.OUT",sep="/")))
     stk@stock.n  <-readBinary(paste(x,"NAA.OUT",sep="/"), dmns)
   if (file.exists(paste(x,"CAA.OUT",sep="/")))
     catch.n(stk) <-readBinary(paste(x,"CAA.OUT",sep="/"), dmns)

   units(harvest(stk))<-"f"

   catch.n(stk)     <-stock.n(stk)*harvest(stk)/z(stk)*(1-exp(-z(stk)))
   landings.n(stk)  <-catch.n(stk)
   discards.n(stk)[]<-0
   
   if (file.exists(paste(x,"WAA.OUT",sep="/"))){
     stock.wt(   stk)<-readBinary(paste(x,"WAA.OUT",sep="/"),dimnames(stock.n(stk)))
     catch.wt(   stk)<-stock.wt(stk)
     landings.wt(stk)<-stock.wt(stk)
     discards.wt(stk)<-stock.wt(stk)}

   wt(stk)          <-stock.wt(stk)

   catch(stk)<-computeCatch(      stk,"all")
   landings(stk)<-computeLandings(stk)
   discards(stk)<-computeDiscards(stk)

#   units(stk)<-units

   return(stk)}
   
diags<-function(x){
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

readVPA2BoxOld<-function(data, results, m=as.numeric(NA), no.discards=TRUE){

  # read input data in Adapt format
  res <- readFLStock(data, type='Adapt', m=m)

  # read N and F from summary output file
  nf <- lapply(getNF(results), setPlusGroup, dims(res)$max)

  stock.n(res)[,dimnames(nf[["stock.n"]])$year]<-nf[["stock.n"]]
  harvest(res)[,dimnames(nf[["harvest"]])$year]<-nf[["harvest"]]
  units(harvest(res))<-units(nf[["harvest"]])

  # discards
  if(no.discards){
    discards.n(res) <- 0
    discards.wt(res) <- landings.wt(res)
    catch(res) <- computeCatch(res, 'all')}

  # desc
  desc(res) <- paste('Imported from a set of VPA2Box files: [', data, ',', results,
      ']', sep='')

  range(res)["plusgroup"]<-range(res)["max"]

  return(res)}

# getNF {{{
getNF <- function(filename){
  posFile <- function(i,filename,char="-"){
    while (TRUE){
      firstChar <- substr(scan(filename, skip = i, nlines = 1,
        what = ("character"), quiet = TRUE)[1], 1, 1)
      if (!is.na(firstChar))
        if (firstChar == char) break
      i<-i+1}

    return(i)}

  getFLQ <- function(filename,pos1, pos2){
    nyrs <- pos2-pos1-1
    t. <- scan(filename, skip = pos1+1, nlines=nyrs, quiet = TRUE)
    nages <- length(t.)/nyrs
    t. <- array(t.,c(nages,nyrs))

    yrs <- array(t.,c(nages,nyrs))[1,]
    ages <- scan(filename, skip = pos1-1, nlines=1, quiet = TRUE)

    flq <- FLQuant(t.[-1,],dimnames=list(age=ages,year=yrs))

    return(flq)}

  i <- 0
  pos1 <- posFile(i,filename)
  pos2 <- posFile(pos1,filename,char="=")
  harvest <- getFLQ(filename,pos1, pos2)
  units(harvest) <- "f"

  pos1 <- posFile(pos2,filename)
  pos2 <- posFile(pos1,filename,char="=")
  stock.n <- getFLQ(filename,pos1, pos2-1)

  return(FLQuants(stock.n=stock.n,harvest=harvest))}

getM <- function(filename, ages){
  i <- 0
  while(TRUE){
    firstChar <- scan(filename, skip = i, nlines = 1,
        what = ("character"), quiet = TRUE)
      if (!is.na(firstChar)[1])
        if ('natural' %in% firstChar)
          break
      i<-i+1
   }

   mat <- matrix(scan(filename, skip = i+1, nlines=ages, what='character', quiet = TRUE),
     nrow=ages, byrow=TRUE)[,2]

   return(as.double(sub('D', 'E', mat)))}



