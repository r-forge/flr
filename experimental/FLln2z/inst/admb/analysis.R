library(ggplotFL)

dirMy ="/home/lkell/Desktop/Stuff/ICCAT/SCRS/2011/ALB_SA/Analysis/Med/SEINE/"

cd /home/lkell/flr/experimental/FLSeine/admb
export ADMB_HOME='/usr/local/admb/'
sudo ln -s /usr/local/admb/bin/adlink /usr/local/bin/adlink

pathNm="/home/lkell/flr/experimental/FLSeine/admb"

#### Data #######################################################################
albLen=read.csv(paste(dirMy,"inputs/albMebMnSz.csv",sep=""),sep=";")
rng   =range(albLen$year[-1])
albLen=merge(data.frame(year=rng[1]:rng[2]),albLen,all=TRUE)

wts              =1/albLen$se^2
wts[is.na(wts)]  =0
wts              =rev(rev(wts[-1]))

obs              =albLen$len
obs[is.na(obs)]  =mean(obs,na.rm=T)
obs              =rev(rev(obs[-1]))

ones             =wts
ones[wts>0]      =1

grwNAtl=FLPar(Linf= 122.8,
              t0  =-0.9892,
              K   = 0.3013445,
              Lc  = 30,
              a   = 1.3718e-5,
              b   = 3.1066)
grwMed =FLPar(Linf=112.13,
              k   =0.46,
              t0  =-0.9892,
              Lc  =30,
              a   =1.3718e-5,
              b   =3.1066)
#############################################################################################

##### Functions #############################################################################
ln2zFunc<-function(object,linf,lc,k) k*(linf-object)/(object-lc)
setGeneric("ln2z", function(object,param,...)
    standardGeneric("ln2z"))
setMethod('ln2z', signature(object='numeric',param="FLPar"),
    function(object,param) {
    dimnames(param)$params=tolower(dimnames(param)$params)  
    ln2zFunc(object,param["linf"],param["lc"],param["k"])})
setMethod('ln2z', signature(object='FLQuant',param="FLPar"),
    function(object,param) {
    dimnames(param)$params=tolower(dimnames(param)$params)  
    ln2zFunc(object,param["linf"],param["lc"],param["k"])})

seine=function(obs,wts,rng,breaks,grw,cmdOps=paste("-maxfn 500"),admbNm="seine")
  {  
  dimnames(grw)$params=tolower(dimnames(grw)$params)  
  
  dimnames(grw)$params=tolower(dimnames(grw)$params)
  
  fn=function(obs,wts,grw,breaks,cmdOps,admbNm){  
    
    ## Data
    ObsLength    = obs
    SampleSize   = wts
   
    ## Parameters
    nbreaks=length(breaks)
    zguess =array(rep(c(0.5,1),nbreaks+1),c(2,nbreaks+1))
    yguess =rbind(breaks,-1)
    sigma  =10.0
   
    ## Growth
    KParm        = grw["k"]
    LInf         = grw["linf"]
    Lc           = 10
        
    ## Output data file
    dat<-       list("Number of Breaks"                            =nbreaks,
                     "First Year of Data"                          =rng[1],
                     "Last Year of Data"                           =rng[2],
        
                     "(1,NYears Observed Mean Lengths)"            =ObsLength,
                     "(1,NYears Observed Sample Sizes)"            =SampleSize,
          
                     "VB K Parameter"                              =KParm,
                     "VB L-Infinity"                               =LInf,
          
                     "Critical Length - Length at first capture"   =Lc,
        
                     "(1,NBreaks+1,1,2)"                           =zguess,
                     "(1,NBreaks,  1,2)"                           =yguess,
          
                     "sigma"                                       =sigma,
                     "stepsize"                                    =5,
                     "casenum"                                     =10)
  
     #### Run ADMB.exe
     writeADMB(dat,file="/home/lkell/flr/experimental/FLSeine/admb/seine.dat")
               
     pathOrg<-getwd()
     setwd(pathNm)
          
     sys.result=system(paste("./", admbNm, " ", cmdOps, sep=""))
             
     setwd(pathOrg)
        
     rep=readADMB("/home/lkell/flr/experimental/FLSeine/admb/seine.rep")

     std=read.table("/home/lkell/flr/experimental/FLSeine/admb/seine.std",skip=1)[,-1]
     names(std)=c("param","value","sd")
       
     return(list(std=std,rep=rep))}
      
  res=fn(obs,wts,grw=grw,breaks=breaks,cmdOps=cmdOps,admbNm=admbNm)
                       
  return(res)}

getStd=function(std,breaks,rng){
  t.=data.frame(from=c(rng[1],breaks),to=c(breaks,rng[2]),bit=1:(length(breaks)+1))
  
  merge(mdply(t.,function(from,to,bit) data.frame(year=seq(from,to),bit=bit))[,3:4],
        data.frame(std[-dim(std)[1],],bit=1:(dim(std)[1]-1)))}
        
setMethod("diags", signature(object="data.frame"),
  function(object, i=NULL) {
    
    dmns <- list(year=object$x)
   
    x        <- FLQuant(object$x,        dimnames=dmns)
    y        <- FLQuant(object$y,        dimnames=dmns)  
    yHat     <- FLQuant(object$hat,      dimnames=dmns)
    residual <- FLQuant(object$residual, dimnames=dmns)

 
    residualLag <- FLQuant(NA, dimnames=dmns)
    residualLag[,-dim(residual)[2]] <- residual[,-1]

    qq. <- qqnorm(c(residual),plot.it=FALSE)
    qqx <- FLQuant(qq.$x,dimnames=dmns)
    qqy <- FLQuant(qq.$y,dimnames=dmns)

    res <- model.frame(FLQuants(x=x, y=y, yHat=yHat, residual=residual, residualLag=residualLag, qqx=qqx, qqy=qqy))

    return(res)})

#### Plots ##########################################################################################
## data
ggplot(albLen) + geom_line(    aes(year,len))     + 
                 geom_line(    aes(year,nominal),colour="red") +
                 geom_point(   aes(year,len))     + 
                 geom_point(   aes(year,nominal),colour="red") +
                 geom_errorbar(aes(year,ymin=len-se*2,ymax=len+se*2)) +
                 expand_limits(y = 0) +
                 theme_flr(size=12.5)   +
                 scale_y_continuous(name="Length") +
                 scale_x_continuous(name="Year")

## equilibrium
bhZ=ln2z(as.FLQuant(transform(albLen,data=len)[,c("year","data")]),grwMed)
fig2a=ggplot(bhZ)+geom_point(aes(year,data))  +
            geom_smooth(aes(year,data))  +
            expand_limits(y = 0) +
            theme_flr(size=12.5)   +
            scale_y_continuous(name="Z") +
            scale_x_continuous(name="Year")

bhZ=ln2z(as.FLQuant(transform(albLen,data=len)[,c("year","data")]),grwNAtl)
fig2b=ggplot(bhZ)+geom_point(aes(year,data))  +
            geom_smooth(aes(year,data))  +
            expand_limits(y = 0) +
            theme_flr(size=12.5)   +
            scale_y_continuous(name="Z") +
            scale_x_continuous(name="Year")
## Seine #########################################################################
breaks     =c(1990,1999,2004)

res=list()
res[["Med 1"]]   =seine(obs,ones,rng,breaks,grwMed)
res[["NAtl 1"]]  =seine(obs,ones,rng,breaks,grwNAtl)
res[["Med Var"]] =seine(obs, wts,rng,breaks,grwMed)
res[["NAtl Var"]]=seine(obs, wts,rng,breaks,grwNAtl)

rep=res[[1]]$rep
std=res[[1]]$std
    
object=data.frame(y       =rep$obs,
                  x       =rep$minyr:rep$maxyr,
                  hat     =rep$hat,
                  residual=rep$Residuals)            
hat   =getStd(std,breaks,rng)
   
fig2a+geom_line(aes(year,value),data=hat) +
     geom_line(aes(year,value+sd),data=hat,colour="red") + 
     geom_line(aes(year,value-sd),data=hat,colour="red")


rsd=data.frame(year=rng[1]:rng[2],data=res$rep$Residuals)
rsd$data[wts==0]=NA
ggplot(as.FLQuant(rsd))+geom_point(aes(year,data))+geom_smooth(aes(year,data))              

jk=mdply((1:length(wts))[wts!=0],
    function(x,obs,wts,rng,breaks,grw){
       wts[x]=0
       seine(obs,wts,rng,breaks,grw)$std},
    obs=obs,wts=wts,rng=rng,breaks=breaks,grw=grw)

ggplot(jk)+geom_histogram(aes(value))+facet_wrap(~param,scale="free")
ggplot(jk)+geom_point(aes(X1,value))+facet_wrap(~param,scale="free")

wts2=wts
wts2[wts2>0]=1
seine(obs,wts2,grw,rng,breaks)
