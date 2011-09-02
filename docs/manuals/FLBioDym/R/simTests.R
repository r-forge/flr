library(FLBioDym)
library(ggplotFL)

####################################################################################################
# 1) check iter & propagate                                                                        #
# 2) vcov & hessian are arrays make vcov & hessian FLPar objects                                   #                                                        #
# 3) Different lengths of index,stock & harvest & window etc.: making stock a method will fix it   #
#    make stock a method                                                                           #
# 5) add validation                                                                                #
####################################################################################################

### tests ###################################################################################
params   =FLPar(r=0.5,K=1000,p=1,b0=1,q=1,sigma=0.3)
hvst     =seq(0,1.5,length.out=40)*fmsy("pellat",params)
hvst     =FLQuant(c(hvst,rev(hvst)[-1]))
model    ="pellat"

#OEMSurvey<-function(OM,start=range(OM,"minyear"),end=start,startf=0,endf=0.01,deviates=NULL,...){
OEMSurveyBD=function(object,error="log"){
      nyr <- dims(harvest(object))$year
      stk <- stock(object)
   
      res <- switch(error,
         'log'   = exp(rnorm(prod(dim(stk)[-2])*nyr, 0, params["sigma"]))*(stk[,-(nyr+1)]+stk[,-1])/2,
          NULL)
      
      return(res)}

bd       =simFLBioDym(model,params,hvst)
index(bd)=OEMSurveyBD(bd)
 
plot.sp(bd)
plot(   bd)
kobe(model.frame(mcf(bd[[c("stock","harvest")]]))) + 
     geom_path( aes(stock/c(bmsy(bd)),harvest/c(fmsy(bd)))) +
     geom_point(aes(stock/c(bmsy(bd)),harvest/c(fmsy(bd)),colour=year),size=1.25)
object=bd 
## one fit ###########################################################################
#runADMBBioDym <- function(object, iter, path, admbNm, cmdOps) 

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
            
#### Prototype for fit #####################################################################
cd /home/lkell/Dropbox/jrciccat/work_in_progress/FLBioDym/backUp
export ADMB_HOME='/usr/local/admb/'
sudo ln -s /usr/local/admb/bin/adlink /usr/local/bin/adlink

dirMy="/home/lkell/Dropbox/jrciccat/work_in_progress/FLBioDym/backUp"
setwd(dirMy)

setGeneric('fit', function(object,bio,params,...)
    standardGeneric('fit'))

setMethod('fit', signature(object='FLBioDym'),
  f.<-function(object,cmdOps=paste("-maxfn 500"),pathNm=getwd(),admbNm="pella"){
           
     cmdOps=paste("-maxfn 500")
     pathNm=getwd()
     admbNm="pella"
    
      pathOrg<-getwd()
      setwd(pathNm)
  
      admbDatBioDym<-function(x,file){
        ctc<-as.list(model.frame(x[["catch"]],drop=T))
        ctc<-c(nYrs=length(ctc[[1]]),ctc)
        
        idx      <-as.list(model.frame(x[["index"]],drop=T))
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
   
      if (dims(object)$iter != dims(params(object))$iter & dims(params(object))$iter==1)
         params(object)  =propagate(params(object), dims(object)$iter)
      if (dims(object)$iter != dims(params(object))$iter & dims(object@index.hat)$iter==1)
         object@index.hat  =propagate(object@index.hat, dims(object)$iter)
      if (dims(object)$iter != dims(params(object))$iter & dims(stock(object))$iter==1)
         stock(object)  =propagate(stock(object), dims(object)$iter)
 
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
 

bd       =simFLBioDym(model,params,hvst)
index(bd)=OEMSurveyBD(bd)

bd <- admbBD(bd)
ggplot(model.frame(mcf(bd[[c("index","fitted","stock")]]))) + 
         geom_line(aes(year, fitted)) + 
         geom_point(aes(year,index))
plot(bd)         

## multiple fits
its=1:1000
# t1000=rdply(max(its),function() {
#   index(bd)=OEMSurveyBD(bd)
#   bd       =fit(bd)
#   
#   as.data.frame(smryStats(bd),drop=T)})

## multiple fits 2
t.            =array(as.numeric(NA),dim(index(bd))[-6],dimnames(index(bd))[-6])
index_bd      =vapply(its, function(x,bd) OEMSurveyBD(bd), t., bd=bd)
dmns          =dimnames(index(bd))
dmns[["iter"]]=its
index(bd)     =FLQuant(index_bd,dimnames=dmns)

t1000    =as.data.frame(smryStats(
    admbBD(bd)
    ),drop=T)  
  
## multiple fits with different length
runWindow=function(start,end,object){
        object        =window(object,start=start,end=end)
        catch(object)=window(catch(object),end=end-1)
        index(object)=window(index(object),end=end-1)
        res          =admbBD(object)
        
        as.data.frame(smryStats(res),drop=T)}
       
t1000_wnd =mdply(expand.grid(start=1,end=seq(1,51,10)+29), runWindow,object=bd)
        
t1000_wnd2=mdply(data.frame(start=seq(1,51,10),end=seq(1,51,10)+29), 
                  runWindow,object=bd)

ggplot(subset(t1000_wnd,params %in% c("stock","bmsy","stockMSY"))) + 
                        geom_histogram(aes(data))+facet_grid(start~params,scale="free")

jk1000=mdply(1: dims(bd)$iter, function(x,bd){
                           bd.      =iter(bd,x)
                           params(bd.)=iter(params(bd.),x)   
                           index(bd.)=jacknife(index(bd.))
                          
                           model.frame(jacknife.summary(smryStats(fit(bd.))),drop=TRUE)},bd=bd)
                        
                                              
save(t1000,t1000_wnd,t1000_wnd2, #t1000_10,t1000_wn,jk1000_10,jk1000_wn,
     file="/home/lkell/Dropbox/jrciccat/documentation/manuals/FLBioDym/data/t1000.RData")
      
load("/home/lkell/Dropbox/jrciccat/documentation/manuals/FLBioDym/data/t1000.RData")


bd       =simFLBioDym(model,params,hvst)
rp       =as.data.frame(refpts(bd))[,-2]
pr       =as.data.frame(params(bd))[,-2]
names(rp)=c("params","true")
names(pr)=c("params","true")
pr       =rbind(rp,pr)

sk=cbind(as.data.frame(stock(  bd)[,ac(unique(t1000_wnd$end))],  drop=T),"params"="stock")
names(sk)=c("end","true","params")
hv=cbind(as.data.frame(harvest(bd)[,ac(unique(t1000_wnd$end)-1)],drop=T),"params"="harvest")
names(hv)=c("end","true","params")
sk2=cbind(as.data.frame(stock(  bd)[,ac(unique(t1000_wnd$end))  ]/refpts(bd)["bmsy"],drop=T),"params"="stockMSY")
names(sk2)=c("end","true","params")
hv2=cbind(as.data.frame(harvest(bd)[,ac(unique(t1000_wnd$end)-1)]/refpts(bd)["fmsy"],drop=T),"params"="harvestMSY")
names(hv2)=c("end","true","params")
hv$end =hv$end+1
hv2$end=hv2$end+1

tr=rbind.fill(sk,sk2,hv,hv2)

wnd =merge(t1000_wnd, pr,all=TRUE)
wnd =merge(wnd,       tr,all=TRUE)
wnd2=merge(t1000_wnd2,tr,all=TRUE)
wnd2=merge(wnd2,      tr,all=TRUE)
wnd$rel =(wnd$data -wnd$true )/wnd$true
wnd2$rel=(wnd2$data-wnd2$true)/wnd2$true

ggplot(subset(wnd, params %in% c("bmsy","fmsy","msy","stock","harvest","stockMSY","harvestMSY")[2])) +
    geom_histogram(aes(rel))+
    facet_grid(end~params,scale="free")

ggplot(subset(wnd2, params %in% c("bmsy","fmsy","msy","stock","harvest","stockMSY","harvestMSY"))) +
    geom_histogram(aes(rel))+
    facet_grid(end~params,scale="free")

        
