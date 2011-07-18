################################################################################
#                                                                              #
#  Simulated data sets                                                         #
#                                                                              #
################################################################################

# warnings all over the shop
# FLPar(r=1,p=1)
# jacknife.summary missing
# need to sort out accessors
# turn functions into methods
# work on ADMB intgration
# drops in mf & df
###############################################################################


cd /home/lkell/Dropbox/jrciccat/FLBioDym
export ADMB_HOME='/usr/local/admb/'
sudo ln -s /usr/local/admb/bin/adlink /usr/local/bin/adlink

#### These should all be the branched versions (for now)
library(FLCore)               #/home/lkell/flr/pkg
library(FLash)                #/home/lkell/flr/pkg
library(FLBioDym)             #/home/lkell/flr/branches
library(ggplotFL)             #/home/lkell/flr/pkga
library(FLAssess)             #/home/lkell/flr/pkg
library(FLXSA)                #/home/lkell/flr/pkg
library(FLAdvice)             #/home/lkell/flr/branches

setwd("/home/lkell/Dropbox/jrciccat/FLBioDym")

source("/home/lkell/flr/pkg/ggplotFL/R/ggplot.R")
source("/home/lkell/flr/pkg/ggplotFL/R/plot.R")
source("/home/lkell/flr/pkg/ggplotFL/R/plotComp.R")

source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/sp.R")
source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/fwd.R")
source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/msy.R")
source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/plot.R")
source("/home/lkell/flr/branches/FLBioDym_LK_20110610_admb/R/methods.R")
  
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
      cat("#", names(x[i]),"\n",file=file,append=TRUE)
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
simBioDym<-function(model,harvest=NULL,r=0.5,K=100,p=1,m=0.25,b0=1.0,error="log",cv=0.3){
  
      if (model=="fletcher") msy <-r.*K/4
      nyr  =dims(harvest)$year
      
      object   =FLBioDym(model ="pellat",
                     stock =FLQuant(rep(K,nyr)),
                     params=FLPar(c(r=r,K=K,p=p,b0=b0,q=1,sigma=0.3)))
     
      object   =fwd(object,harvest=harvest)               

      stk=stock(object) 
 
      index(object)<-switch(error,
                      log   =exp(rnorm(nyr,0,cv))*(stk[,-(nyr+1)]+stk[,-1])/2,
                      normal=exp(rnorm(nyr,0,cv))*(stk[,-(nyr+1)]+stk[,-1])/2,
                      cv    =exp(rnorm(nyr,0,cv))*(stk[,-(nyr+1)]+stk[,-1])/2)
     
      #bd<-setInit(bd,params(bd))
      object@bounds["r",     "start"]=r
      object@bounds["K",     "start"]=K
      object@bounds["p",     "start"]=p
      object@bounds["b0",    "start"]=b0
      object@bounds["q",     "start"]=1.0
      object@bounds["sigma", "start"]=0.3
  
      object@bounds[,"lower"]=object@bounds[,"start"]*.1
      object@bounds[,"upper"]=object@bounds[,"start"]*10
      
      object@bounds["p", "phase"]=-1
      object@bounds["b0","phase"]=-1
      object@priors[,1]=-1
 
      return(object)}
          
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
  
smryStats<-function(bd){                 
    stk   =as.data.frame(stock(  bd)[,dims(bd)$year-1])[,6:7]
    hvt   =as.data.frame(catch(bd)[,dims(bd)$year-1]/stock(bd)[,dims(bd)$year-1])[,6:7]
    stkRel=as.data.frame(stock(bd)[,dims(bd)$year-1]/c(refpts(bd)["bmsy"]))[,6:7]
    hvtRel=as.data.frame((catch(bd)[,dims(bd)$year-1]/stock(  bd)[,dims(bd)$year-1])/c(refpts(bd)["fmsy"]))[,6:7]
    rps   =as.data.frame(refpts(bd))
    par   =as.data.frame(params(bd))
    
    names(rps)[1]="params"
    names(par)[1]="params"

    res<-rbind(rps,par,
               data.frame(params="stock",     stk),
               data.frame(params="harvest",   hvt),
               data.frame(params="stockMSY",  stkRel),
               data.frame(params="harvestMSY",hvtRel))
 
    return(as.FLQuant(res))}
       
  bdJK<-function(bd){
    
    index(bd)=jacknife(index(bd))
    bd       =fit(bd)
    res      =as.data.frame(FLQuants(jacknife.summary(smryStats(bd))),drop=T)
  
  return(res)}
    
  
### tests ###################################################################################
hvst     =seq(0,1.5,length.out=40)*fmsy("pellat",FLPar(c(r=0.5,K=1000,p=1)))
hvst     =FLQuant(c(hvst,rev(hvst)[-1]))
bd       =simBioDym("pellat",hvst,r=0.5,K=1000,p=1,cv=0.3)

plot.sp(bd)
plot(   bd)
kobe(model.frame(mcf(bd[[c("stock","harvest")]]))) + 
     geom_path( aes(stock/c(bmsy(bd)),harvest/c(fmsy(bd)))) +
     geom_point(aes(stock/c(bmsy(bd)),harvest/c(fmsy(bd)),colour=year),size=1.25)
 
## one fit
bd<-fit(simBioDym("pellat",window(hvst,end=40), r=0.5,K=1000,p=1,cv=0.3))
ggplot(model.frame(mcf(bd[[c("index","fitted","stock")]]))) + 
         geom_line(aes(year, fitted)) + 
         geom_point(aes(year,index))
plot(bd)         
  
## multiple fits
t100=rdply(1000,function() {
  res<-fit(simBioDym("pellat",window(hvst,end=60),r=0.5,K=1000,p=1,cv=0.3))
  print(ggplot(modelsystem.frame(mcf(res[[c("index","fitted","stock")]]))) +
     geom_line(aes(year,fitted))+
     geom_point(aes(year,index)))
  as.data.frame(smryStats(res),drop=T)
  })
ggplot(subset(t1000,params=="r"))+geom_histogram(aes(data))+facet_wrap(~params,scale="free") 
save(t1000,file="/home/lkell/Dropbox/jrciccat/FLBioDym/t1000.RData")

true<-as.data.frame(smryStats(window(bd,end=60)),drop=T)
names(true)[2]<-"true"
t1000.<-transform(merge(t1000,true),stat=(data-true)/true)
ggplot(subset(t1000., !(params %in% c("b0","p"))))+geom_histogram(aes(stat))+facet_wrap(~params) 


## sequential jk fits
decline=rdply(10, function() {
        sim<-simBioDym("pellat",hvst,r=0.5,K=1000,p=1,cv=0.3)
        mdply(data.frame(end=seq(30,50,5)), function(end,sim2) {
                sim2       <-window(sim2,end=end)
                index(sim2)<-jacknife(index(sim2))
                sim2       <-fit(sim2)
                as.data.frame(smryStats(sim2),drop=T)},sim2=sim)})
ggplot(decline)+geom_histogram(aes(data))+facet_wrap(~params,scale="free") 
save(decline,file="/home/lkell/decline1.RData")

ggplot(subset(decline,.id %in% "1" & params %in% c("bmsy","stock","stockMSY"))) + 
      geom_histogram(aes(data)) + 
      facet_grid(end~params,scale="free") +
      scale_x_continuous(limits=c(-0.5,2))

recovery=rdply(200, function() {
        sim<-window(simBioDym("pellat",hvst,r=0.5,K=1000,p=1,cv=0.3),start=21)
        mdply(data.frame(end=seq(40,60,1)), function(end,sim2) {
                sim2       <-window(sim2,end=end)
                index(sim2)<-jacknife(index(sim2))
                sim2       <-fit(sim2)
                as.data.frame(smryStats(sim2),drop=T)},sim2=sim)})
ggplot(recovery)+geom_histogram(aes(data))+facet_wrap(~params,scale="free") 
save(recovery,file="/home/lkell/recovery1.RData")

mmm=rdply(200, function() 
    mdply(p=1:2,cv=c(0.2,0.3)),
       function(p,cv){
          sim<-simBioDym("pellat",hvst,r=0.5,K=1000,p=p,cv=cv)
          mdply(expand.grid(start=c(11,31),end=seq(30,50,1)),
             function(start,end,sim){
                sim<-window(sim,start=start,end=start+end)
                index(sim)<-jacknife(index(sim))
                sim       <-fit(sim)
                as.data.frame(smryStats(sim),drop=T)},sim=sim)})
save(mmm,file="/home/lkell/mmm.RData")

save(t60,file="/home/lkell/sims5.RData")
load("/home/lkell/sims5.RData")

bdHat=mdply(data.frame(end=seq(30,50,1)), function(end) as.data.frame(smryStats(fit(simBioDym("pellat",window(hvst,end=end), r=0.5,K=100,p=1,cv=0)))))[,c(1,2,8)]
names(bdHat)[3]<-"true"
s5<-cast(sims5,.n+params+end~qname,value="hat")
s5<-merge(s5,bdHat)

ggplot(subset(s5,params=="stockMSY")) + 
    geom_line( aes(end,true))         + 
    geom_point(aes(end,se))           +
    scale_y_continuous(limits=c(0,2))

mlply(1:5,function(x) scan("/home/lkell/Dropbox/jrciccat/FLBioDym/pella.cor",what=as.character(),skip=x,nline=1)[-(1:4)])

