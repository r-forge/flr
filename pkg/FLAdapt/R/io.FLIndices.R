validFLIndex <- function(object) {
#return(TRUE)
  dimnms <- qapply(object, function(x) dimnames(x))

  # iters are 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$iter))))>2)
     stop("Iters in FLIndex can only be of length 1 or n")

  # quant is 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$max))))>2)
     stop("quant dimension in FLIndex can only be 'all' or n")

  # iter is 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$iter))))>2)
     stop("iter dimension in FLIndex can only be '1' or n")

  # dims[2:5] match
  for(i in names(dimnms)[-1])
    if(!all.equal(dimnms[[i]][c(-1,-6)], dimnms[[1]][c(-1,-6)]))
      stop(cat("Mismatch in dims for", i))
      
  # first dim equal for all index.* slots
  #for(i in grep('index', names(dimnms), value=TRUE))
  #  if(!all.equal(dimnms[[i]][1], dimnms[[1]][1]))
  #    stop(cat("Mismatch in dims for", i))

  # effort should have quant='all'
  if (!(dims(slot(object,"effort"))[1] == 1))
     stop("Effort can only have quant = 'all'")

  # min / max
  dims <- dims(object@catch.n)
  min <- object@range["min"]

  if (!is.na(min) && (min < dims(object@catch.n)$min || min > dims(object@catch.n)$max))
     stop(paste("min is outside quant range in FLQuant slot", i))

  max <- object@range["max"]
  if(!is.na(max) && (max < dims(object@catch.n)$min || max > dims(object@catch.n)$max))
    stop(paste("max is outside quant range in FLQuant slot", i))

  if (!is.na(min) && !is.na(max) && max < min)
    stop(paste("max quant is lower than min quant in FLQuant slot", i))

  # plusgroup
  plusgroup <- object@range["plusgroup"]
  if (!is.na(plusgroup) && (plusgroup < dims$min || plusgroup > dims$max))
     stop("plusgroup is outside [min, max] range in FLQuant slots")

  # minyear / maxyear
  dims <- dims(object@index)
  minyear <- object@range["minyear"]
  if (!is.na(minyear) && (minyear < dims$minyear || minyear > dims$maxyear))
     stop(paste("minyear is outside years range in FLQuant slot", i))
  maxyear <- object@range["maxyear"]
  if (!is.na(maxyear) && (maxyear < dims$minyear || maxyear > dims$maxyear))
     stop(paste("maxyear is outside years range in FLQuant slot", i))
  if (!is.na(minyear) && !is.na(maxyear) && maxyear < minyear)
     stop(paste("maxyear is lower than minyear in FLQuant slot", i))

  # Everything is fine
  return(TRUE)}

setClass("FLIndex",
    representation(
  	"FLComp",
        type         = "character",
        distribution = "character",
        index        = "FLQuant",
        index.var    = "FLQuant",
        catch.n      = "FLQuant",
		    catch.wt     = "FLQuant",
		    effort       = "FLQuant",
		    sel.pattern  = "FLQuant",
		    index.q      = "FLQuant"),
    prototype=prototype(
        name         = character(0),
        desc         = character(0),
        type         = character(0),
        range        = unlist(list(min=0,     max=0,     plusgroup=NA,
			                             minyear=1, maxyear=1, startf=NA, endf=NA)),
        distribution = character(0),
        index        = new("FLQuant"),
        index.var    = new("FLQuant"),
		    catch.n      = new("FLQuant"),
		    catch.wt     = new("FLQuant"),
		    effort       = new("FLQuant"),
		    sel.pattern  = new("FLQuant"),
		    index.q      = new("FLQuant")),
    validity=validFLIndex)


setValidity("FLIndex", validFLIndex)
 remove(validFLIndex)    #   }}}


skip.hash<-function(i,x) {
        i<-i+1
        while (substr(scan(x,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)=="#")
            i<-i+1
        return(i)}
    
skip.until.minus.1<-function(i,x) {
        i<-i+1
        while (scan(x,skip=i,nlines=1,what=("character"),quiet=TRUE)[1]!="-1")
            i<-i+1
        return(i)}

timing=function(i,smry){
  # TIMING (-1 = AVERAGE DURING YEAR, POSITIVE INTEGER = NUMBER OF MONTHS ELAPSED)
  if   (smry[i,"timing"]==-1) return(data.frame(startf=-1,endf=-1))
  else                        return(data.frame(startf=smry[i,"timing"]/12,endf=smry[i,"timing"]/12))}

pdf=function(i,smry){
    # PDF   (0= do not use,1=lognormal, 2=normal)
    if (smry[i,"pdf"]==0) return("notused")
    if (smry[i,"pdf"]==2) return("normal")
    else return("lognormal")}

type=function(i,smry){
    # UNITS       (1 = numbers, 2 = biomass)
    if (smry[i,"units"]==2) return("biomass") 
    else                    return("numbers")}

sel=function(i,smry){
    # SELECTIVITY (1 = fixed, 2 = fractional catches, 
    # 3 = Powers and Restrepo partial catches,4=Butterworth and Geromont eq 4)
    if (smry[i,"type"]==1)  return("sel") 
    if (smry[i,"type"]==2)  return("catches")
    if (smry[i,"type"]==3)  return("Powers")
    if (smry[i,"type"]==4)  return("Butterworth")}
          
getIdxData=function(x,na.strings="NA") {

# INDEX PDF  (0= do not use,1=lognormal, 2=normal)
# |     |    UNITS (1 = numbers, 2 = biomass)
# |     |    |      VULNERABILITY (1=fixed, 2=frac.catches, 3=part. catches, 4=Butt. & Gero.
# |     |    |      |     TIMING (-1=average, +integer = number of months elapased}
# |     |    |      |     |     FIRST AGE  LAST AGE   TITLE (IN SINGLE QUOTES)

  range<-numeric(5)
  names(range)<-c("min","max","plusgroup","minyear","maxyear")
	
  i <- skip.hash(0,x)
  range[c("minyear", "maxyear")] = scan(x,skip = i, nlines = 1, nmax = 2, quiet = TRUE,na.strings=na.strings)
  i <- skip.hash(i,x)
  range[c("min", "max", "plusgroup")] = scan(x,skip = i, nlines = 1, nmax = 3, quiet = TRUE,na.strings=na.strings)
  i =skip.hash(i,  x)

  NIndex =scan(x,skip=i,nlines=1,nmax=1,quiet=TRUE)
  i =skip.until.minus.1(i,x)
  i =skip.hash(         i,x)

  smry=array(0,dim     =c(NIndex,6),
               dimnames=list(index=1:NIndex,c("pdf","units","vulnerability","timing","min","max")))
      
  k =numeric(1)
  nms=NULL
  for (i in i:(i+NIndex-1)) {
     k=k+1
     smry[k,]=      scan(x,skip=i,                 nlines=1,nmax=7,quiet=TRUE,na.strings=na.strings)[-1]
     nms     =c(nms,scan(x,skip=i,what=character(),nlines=1,nmax=8,quiet=TRUE,na.strings=na.strings)[ 8])}

  dimnames(smry)$index=nms

  i=skip.hash(i,x)
  i=skip.hash(i,x)
 
  series=array(0,dim=c(skip.until.minus.1(i,x)-i, 4))
  for (j in i:(skip.until.minus.1(i,x)-1))
    series[j-i+1,]<-scan(x,skip=j,nlines=1,nmax=4,quiet=T,na.strings=na.strings)

  dimnames(series)=list(NULL,c("index","year","data","cv"))
  i  =skip.until.minus.1(i,x)+1     	
  vul=read.table(x,skip=i,fill=T,nrows=(skip.until.minus.1(i,x)-i-3),colClasses="numeric",na.strings=na.strings)
  i  =skip.until.minus.1(i,x)+1       
  i  =skip.hash(i,x)
  wts=read.table(x,skip=i,fill=T,nrows=(skip.until.minus.1(i,x)-i),  colClasses="numeric",na.strings=na.strings)
  
  names(vul)=c("index","year",range["min"]:range["max"])
  names(wts)=c("index","year",range["min"]:range["max"])

  smry=data.frame(smry,daply(as.data.frame(series), .(index), function(x) range(x$year)))
  smry$plusgroup=range["plusgroup"]
  smry$plusgroup[smry[,"max"]<range["plusgroup"]]=NA

  names(smry)[7:8]=c("minyear","maxyear")

return(list(range=range,smry=smry,"cpue"=data.frame(series),vul=vul,wts=wts))}

getIdx=function(i,smry){
  flq2=FLQuant(NA, dimnames=list(age =smry$smry[i,"min"    ]:smry$smry[i,"max"],
                                 year=smry$smry[i,"minyear"]:smry$smry[i,"maxyear"]))
  flq1=FLQuant(NA, dimnames=list(age ="all",
                                 year=smry$smry[i,"minyear"]:smry$smry[i,"maxyear"]))
  
  index    =as.FLQuant(subset(smry$cpue, index==i)[,c("year","data")],quant="age")
  index.var=index
  index.var[]=subset(smry$cpue, index==i)[,"cv"]
  units(index)=type(i,smry$smry)
  catch.wt=flq2
  if (i %in% smry$wts$index) {
      catch.wt=melt(subset(smry$wts, i==index & year %in% smry$smry[i,"minyear"]:smry$smry[i,"maxyear"])[,-1],id.var="year",variable_name="age")
      names(catch.wt)[3]="data"
      catch.wt=as.FLQuant(catch.wt)
      }
  
  catch.n=flq2
  if (i %in% smry$vul$index) {
      catch.n=melt(subset(smry$vul, i==index & year %in% smry$smry[i,"minyear"]:smry$smry[i,"maxyear"])[,-1],id.var="year",variable_name="age")
      names(catch.n)[3]="data"
      catch.n=as.FLQuant(catch.n)
      }
  flq2=FLQuant(NA,dimnames=dimnames(catch.n))
                                 
  idx=FLIndex(index      =index,index.var=index.var,index.q=flq1,effort=flq1,
              sel.pattern=flq2,catch.n  =catch.n,catch.wt=catch.wt,
              range       =unlist(smry$smry[i,c(5,6,9,7,8)]),
              name        =dimnames(smry$smry)[[1]][i],
              desc        ="read in via text file",
              distribution=pdf( i,smry$smry),
              sel.pattern =type(i,smry$smry))

   ## years
   if (smry$smry[i,"minyear"]<smry$range["minyear"] | smry$smry[i,"maxyear"]>smry$range["maxyear"])
      idx=trim(idx,year=smry$range["maxyear"]:smry$range["maxyear"])
   
   ## plus group
#    if (smry$smry[i,"plusgroup"]>smry$range$plusgroup)
#      idx=setPlusGroup(idx,smry$range$plusgroup)
 
   ## ages
#    if (smry$smry[i,"min"]<smry$range["minyear"] | smry$smry[i,"max"]>smry$range["minyear"])
#       idx=trim(idx,age=smry$range["minyear"]:smry$range["maxyear"])
 
  return(idx)}

# read.FLIndex
read.FLIndex <- function(...){
  warning("read.FLIndex has been renamed as readFLIndex and will de deprecated", inmediate. = TRUE)
  readFLIndex(...)}

# read.FLIndices
read.FLIndices <- function(...){
  warning("read.FLIndices has been renamed as readFLIndices and will de deprecated", inmediate. = TRUE)
  readFLIndices(...)}

#idx=readVPA2BoxIndices("/home/lkell/Dropbox/adapt/inputs/Bootstraps/Inflated/run13/bfte2010.d1")
#validFLIndex(idx[[2]])

readVPA2BoxIndices=function(x){
  smry=getIdxData(x)

  res=FLIndices(mlply(data.frame(i=seq(dim(smry$smry)[1])), getIdx, smry=smry))
  names(res)=laply(res, function(x) name(x))
  
  return(res)}

#x="/home/lkell/Dropbox/adapt/inputs/Bootstraps/Inflated/run13/bfte2010.d1"
#smry=getIdxData(x)
#idx=FLIndices(mlply(data.frame(i=seq(dim(smry$smry)[1])), getIdx, smry=smry))

  
