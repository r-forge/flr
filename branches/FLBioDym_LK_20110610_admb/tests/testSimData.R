################################################################################
#                                                                              #
#  Simulated data sets                                                         #
#                                                                              #
################################################################################

# warnings all over the shop
# FLPar(r=1,p=2)
# kacknife.summary missing
# need to sort out accessors
# turn functions into methods
# work on ADMB intgration
# drops in mf & df
###############################################################################

#### These should all be the branched versions (for now)
library(FLCore)
library(FLash)
library(FLBioDym)

cd /home/lkell/Dropbox/jrciccat/FLBioDym
export ADMB_HOME='/usr/local/admb/'
sudo ln -s /usr/local/admb/bin/adlink /usr/local/bin/adlink

if (FALSE){
  source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/class.R")
  source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/constructors.R")
  source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/createAccessors.R")
  source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/msy.R")
  source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/sp.R")
  source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/pars.R")
  source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/methods.R")
  source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/plot.R")
  source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/fwd.R")
 }


## temporary functions to be replaced by FLCore functions
readADMB<-function(file){
  ## read in data from ADMB Par or Rep file
  dat  <-scan(file,what="",sep="\n",skip=1)
  ## Get values
  vals <-lapply(strsplit(dat[grep("#",dat,invert=TRUE)]," "), function(x) as.numeric(x[nchar(x)>0]))
  ## name elements
  names(vals)<-lapply(grep("#",dat,value=T),function(x) substr(x,3,nchar(x)-1))
  return(vals)}

writeADMB<-function(x,file){
  cat("#", names(x[1]),"\n",file=file,append=FALSE)
  cat(x[[1]],"\n",file=file,append=TRUE)
  for (i in 2:length(x)){
    cat("#", names(x[1]),"\n",file=file,append=TRUE)
    cat(x[[i]],"\n",file=file,append=TRUE)}}
    
jacknife.summary<-function (flx) {
    nms     <-names(dimnames(flx))
    idx     <-seq(length(nms))[nms != 'iter']
    n       <-dims(flx)$iter-1
    
    mn      <-iter(flx,  1)
    u       <-iter(flx, -1)
    mnU     <-apply(u,idx,mean)   

    SS      <-apply(sweep(u,idx, mnU,"-")^2,idx,sum)

    bias<-(n - 1) * (mnU - mn)
    se  <-sqrt(((n-1)/n)*SS)

    return(list("1"=mn,mean=mnU,se=se,bias=bias))}

drop<-function(x,nms2drop=c("age","quant","year","unit","season","area","iter")){
  nms     <-names(x)
  nms2drop<-nms2drop[nms2drop %in% nms]
  
  nms2keep <-nms[!(nms %in% nms2drop)]
  nms2drop.<-nms2drop[!maply(nms2drop, function(nms,x) length(unique(x[[nms[1]]]))==1,x=x)]
  
  return(x[,c(nms2drop.,nms2keep)])}
################################################################################################

### test function ##############################################################################
simBioDym<-function(model,harvest=NULL,r=0.5,K=100,p=1,m=0.25,b0=1.0,error="log"){

    if (model=="fletcher") msy <-r.*K/4
    nyr  =dims(harvest)$year
    
    bd   =FLBioDym(model ="pellat",
                   stock =FLQuant(rep(K,nyr)),
                   params=FLPar(c(r=r,K=K,p=p,b0=b0,q=1,sigma=0.3)))
    bd   =fwd(bd,harvest=harvest)               
                   
    index(bd)<-switch(error,
                    log   =exp(rnorm(nyr,0,.2))*(stock(bd)[,-(nyr+1)]+stock(bd)[,-1])/2,
                    normal=exp(rnorm(nyr,0,.2))*(stock(bd)[,-(nyr+1)]+stock(bd)[,-1])/2,
                    cv    =exp(rnorm(nyr,0,.2))*(stock(bd)[,-(nyr+1)]+stock(bd)[,-1])/2)
   
    bd<-setInit(bd,params(bd))
    bd@priors["p",1]<-1
    
    return(bd)}

#### Prototype for fit #####################################################################
setMethod('fit', signature(object='FLBioDym'),
f.<-function(object,cmdOps=paste("-maxfn 500"),pathNm=getwd(),admbNm="pella"){
    pathOrg<-getwd()
    setwd(pathNm)

    admbDatBioDym<-function(x,file){
      ctc<-as.list(drop(model.frame(x[["catch"]])))
      ctc<-c(nYrs=length(ctc[[1]]),ctc)
      
      idx      <-as.list(drop(model.frame(x[["index"]])))
      idx$year <-idx$year[ !is.na(idx$index)]
      idx$index<-idx$index[!is.na(idx$index)]
 
      res<-c(ctc,c(nYrs=length(idx[[1]]),idx))
      writeADMB(res,file)
 
      return(idx$year)}
      
    ctl<-object@bounds 
    ctl[,2:4]<-log(ctl[,2:4])
    ctl<-alply(ctl,1)
    names(ctl)<-dimnames(object@bounds)$params
    writeADMB(ctl,paste(pathNm,"/",admbNm,".ctl",sep=""))
     
    prr<-object@priors 
    prr[,2:3]<-log(prr[,2:3])
    prr<-alply(prr,1)
    names(prr)<-dimnames(object@priors)$params
    writeADMB(prr,paste(pathNm,"/",admbNm,".prr",sep=""))
 
    params(object)  =propagate(params(   object), seq(dims(object)$iter))
    
    dms             =dimnames(object@index)
    dms$iter        =seq(dims(object)$iter)
    object@index.hat=FLQuant(NA, dimnames=dms)
    
    dms          =dimnames(object@stock)
    dms$iter     =seq(dims(object)$iter)
    object@stock =FLQuant(NA, dimnames=dms)
 
    fitter<-function(x) {
      idxYrs<-admbDatBioDym(iter(object,x),paste(pathNm,"/",admbNm,".dat",sep=""))
        
      sys.result=system(paste("./", admbNm, " ", cmdOps, sep=""))
    
      t1<-read.table(         paste(pathNm,"/",admbNm,".rep",sep=""),skip =18,header=T)
      t2<-unlist(c(read.table(paste(pathNm,"/",admbNm,".rep",sep=""),nrows=8)))

      object@params[c("r","K","b0","p","q","sigma"),x]<-t2[1:6]
      #logLik( iter(object,x))<-t2[7]
      #iter(object,x)@rsdlVar <-t2[8]
    
      object@index.hat[,ac(idxYrs),,,,x][]<-unlist(c(t1[t1$Year %in% idxYrs,"IndexFit"]))
      object@stock[,1:dim(t1)[1],,,,x]    <-unlist(c(t1["Biomass"]))
 
      #print(c(sum(t1[,"Index"]*t1[,"Biomass"])/sum(t1[,"Biomass"]^2),
      #       log((sum((log(t1$Index)-log(t1$IndexFit))^2)/4)^0.5)))
      
      object<<-object}
 
      t.<-m_ply(data.frame(x=seq(dims(object)$iter)),function(x) fitter(x))
              
    setwd(pathOrg)

    return(object)})

### tests ###################################################################################
setwd("/home/lkell/Dropbox/jrciccat/FLBioDym")

# 1) Simulate data and fit all year 
## Harvest
hvst=FLQuant(c(seq(0.0,1.0,length.out=20),seq(1.0,0.25,length.out=20)))*1.25*fmsy("pellat",FLPar(c(K=1000,r=0.5, p=1)))    

sims<-rdply(100,function(){
             bd=simBioDym("pellat",hvst,r=0.5,K=100,p=1)
       
             as.data.frame(params(fit(bd)))
             })
ggplot(subset(sims, params %in% c("r","K","p","q","sigma")))+geom_histogram(aes(data))+facet_wrap(~params,scale="free")

# 2) fit every 5 years 
sims5<-rdply(1000, 
             function(){
                 bd  =simBioDym("pellat",hvst,r=0.5,K=100,p=1)
                 mdply(data.frame(end=seq(20,40,5)), function(end) as.data.frame(params(fit(window(bd,end=end)))))
                 })
                 
ggplot(subset(sims5, params %in% c("r","K","q","sigma"))) + 
          geom_histogram(aes(data)) + 
          facet_grid(end~params,scale="free")
                 
# 3) Jacknife 
bd       =simBioDym("pellat",hvst,r=0.5,K=100,p=1)
bdJK<-function(bd){
  index(bd)=jacknife(index(bd))
  return(as.data.frame(jacknife.summary(params(fit(bd)))))}
bdJK(bd)             
             
# 4) Jacknife every 5 years
sims5<-rdply(1000, 
             function(){
                 bd  =simBioDym("pellat",hvst,r=0.5,K=100,p=1)
                 mdply(data.frame(end=seq(20,40,5)), function(end) bdJK(window(bd,end=end))))
                 })

