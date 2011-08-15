#### Data ##############################################################################
## example data set based on FLR
data(ple4)
  
data(ple4sex)
ple4mf=as.data.frame(stock.wt(ple4sex),drop=T)
dat   =data.frame(x=1:10,y=rnorm(10))
  
## example data set based on Med-SWO
load(paste(dirMy,"data/cas.RData",sep="/"))

## BFT-E Assessments
if (!file.exists(paste(dirMy,"data/ts.RData",sep="/"))){
  scenVPA=apply(expand.grid(Catch=c("Inflated","Reported"),
                            Run =c(13,15)),2,ac)
  fileVPA=apply(scenVPA,1,function(x) paste(dirMy,"inputs/bft-e/VPA",paste(x,collapse="/"),"bfte2010.c1",sep="/"))
  pgWt   =function(x) stock.wt(x)[range(x)["plusgroup"]]
  ts     =FLStocks(mlply(fileVPA,readVPA2Box,.progress="text"))
  ts     =ldply(ts, function(x) as.data.frame(x[[c("ssb","fbar","rec","catch","computeStock","mnSwt","pgWt")]]), .progress="text")[,c("X1","qname","year","data","iter")]
  ts     =cbind(scenVPA[ts$X1,],ts)
  
  names(ts)[3]="scen"
  
  
  SSB=subset(ts,qname=="ssb",select=-qname)

  save(ts,SSB,file=paste(dirMy,"data/ts.RData",sep="/"))} else
load(paste(dirMy,"data/ts.RData",sep="/"))
     
## BFT-E projections 
if (!file.exists(paste(dirMy,"data/prj.RData",sep="/"))){
  scenPrj=apply(expand.grid(Implementation=c("Error","Perfect"),
                            Catch         =c("Inflated","Reported"),
                            Run           =c(13,15)[1],
                            Recruitment   =c("High","Low","Medium")[3],
                            Steepness     =c("0.5","0.75","0.9")[2]),2,ac)
  filePrj=apply(scenPrj,1,function(x) paste(dirMy,"inputs/bft-e/proj",paste(x,collapse="/"),sep="/"))
     
  rel=function(x){
       Ref=readPro2Box(x,type="ref")
       Out=readPro2Box(x,type="out")
      
       msy.=subset(Ref,refpt="msy")
       rel.=transform(Out,ssb    =ssb/msy.[iter+1,"ssb"],
                          rec    =rec/msy.[iter+1,"rec"],
                          harvest=fapex/msy.[iter+1,"harvest"],
                          yield  =yield/msy.[iter+1,"yield"])[,c("scen","iter","year","ssb","harvest")]
       
       return(rel.)}
  
   prj=mdply(filePrj,rel,.progress="text")
   prj=cbind(scenPrj[prj$X1,],prj)
   save(prj,file=paste(dirMy,"data/prj.RData",sep="/"))} else
load(paste(dirMy,"data/prj.RData",sep="/"))  
#### End Data ###########################################################################
  
            