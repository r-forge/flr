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
library(FLAdvice)

cd /home/lkell/Dropbox/jrciccat/FLBioDym
export ADMB_HOME='/usr/local/admb/'
sudo ln -s /usr/local/admb/bin/adlink /usr/local/bin/adlink

source("/home/lkell/flr/pkg/ggplotFL/R/ggplot.R")
source("/home/lkell/flr/pkg/ggplotFL/R/plot.R")
source("/home/lkell/flr/pkg/ggplotFL/R/plotComp.R")

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
      
      bd   =FLBioDym(model ="pellat",
                     stock =FLQuant(rep(K,nyr)),
                     params=FLPar(c(r=r,K=K,p=p,b0=b0,q=1,sigma=0.3)))
      bd   =fwd(bd,harvest=harvest)               
                     
      index(bd)<-switch(error,
                      log   =exp(rnorm(nyr,0,cv))*(stock(bd)[,-(nyr+1)]+stock(bd)[,-1])/2,
                      normal=exp(rnorm(nyr,0,cv))*(stock(bd)[,-(nyr+1)]+stock(bd)[,-1])/2,
                      cv    =exp(rnorm(nyr,0,cv))*(stock(bd)[,-(nyr+1)]+stock(bd)[,-1])/2)
     
      bd<-setInit(bd,params(bd))
      #bd@priors["p",1]<-1
      
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
        
      object@bounds["b0",1]<--1  
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
  bd<-fit(bd)
  plot(bd)
  res<-as.data.frame(FLQuants(jacknife.summary(smryStats(bd))),drop=T)
  return(res)}
     
  ### tests ###################################################################################
  setwd("/home/lkell/Dropbox/jrciccat/FLBioDym")
  
  bd  =simBioDym("pellat",FLQuant(seq(0,1,0.1)),r=0.5,K=1000,p=2)
  
  #hvst=FLQuant(c(seq(0.0,1.0,length.out=20),seq(1.0,0.25,length.out=20)))*1.25*fmsy("pellat",FLPar(c(K=2,r=0.5, p=2)))    
  hvst=seq(0.5,1.5,length.out=40)*fmsy("pellat",FLPar(c(r=0.5,K=1000,p=2)))
  hvst=FLQuant(c(rep(hvst[1],10),hvst,rev(hvst)[-1]))
  #bd  =window(simBioDym("pellat",hvst,r=0.5,K=1000,p=2),start=11)
  index(bd)=window(index(bd),start=11)
  stock(bd)=window(stock(bd),start=11)
  catch(bd)=window(catch(bd),start=11)
  range(bd,"minyear")=11
 
  plot(bd)
  kobe(model.frame(mcf(bd[[c("stock","harvest")]]))) + 
     geom_path( aes(stock/c(bmsy(bd)),harvest/c(fmsy(bd)))) +
     geom_point(aes(stock/c(bmsy(bd)),harvest/c(fmsy(bd)),colour=year),size=1.25)
 
 
# Jacknife every year
sims5<-rdply(100, function(){
                   bd  =simBioDym("pellat",hvst,r=0.5,K=1000,p=2)
                 
ggplot(subset(s5, params %in% c("fmsy"))) + 
   geom_histogram(aes(se))+facet_grid(end~params,scale="free") + 
   scale_x_continuous(limits=c(0,50))

                   mdply(data.frame(end=seq(25,40,1)), function(end) bdJK(window(bd,end=end)))})
                 
ggplot(subset(sims5,qname==1))+geom_point(aes(end,data))+facet_wrap(~params,scale="free")

save(sims5,file="/home/lkell/sims5.RData")
load("/home/lkell/sims5.RData")

bdHat=mdply(data.frame(end=seq(25,40,1)), function(end) as.data.frame(smryStats(fit(simBioDym("pellat",window(hvst,end=end), r=0.5,K=100,p=1,cv=0)))))[,c(1,2,8)]
names(bdHat)[3]<-"true"
s5<-cast(sims5,.n+params+end~qname,value="hat")
s5<-merge(s5,bdHat)

ggplot(subset(s5,params=="stockMSY")) + 
    geom_line( aes(end,true))         + 
    geom_point(aes(end,se))           +
    scale_y_continuous(limits=c(0,2))
ggplot(subset(s5,params=="stockMSY")) + 
    geom_line( aes(end,true))         + 
    geom_boxplot(aes(end,se))           +
    scale_y_continuous(limits=c(0,2))

ggplot(subset(s5,params=="msy"))+geom_point(aes(end,mean))+facet_wrap(~params,scale="free")

ggplot(subset(s5, params %in% c("msy","bmsy","fmay")))+geom_histogram(aes(1))+facet_wrap(~end,scale="free")

ggplot(subset(s5, params %in% c("fmsy"))) + 
   geom_histogram(aes(se))+facet_grid(end~params,scale="free") + 
   scale_x_continuous(limits=c(0,50))

ggplot(subset(s5,params=="stockMSY"))+ geom_line(aes(end,true))
