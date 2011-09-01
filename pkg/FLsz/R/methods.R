pathNm="/home/lkell/flr/experimental/FLsz/inst/admb"

# jk=mdply((1:length(n))[n!=0],
#     function(x,obs,n,rng,breaks,grw){
#        n[x]=0
#        object(obs,n,rng,breaks,grw)$std},
#     obs=obs,n=n,rng=rng,breaks=breaks,grw=grw)


getHat=function(object){
  rng  =range(object)[c("minyear","maxyear")]
  nbrks=dim(object@params)[1]/2
  brks =object@params[1:(nbrks-1)+nbrks]
                      
  t.=data.frame(from=c(rng[1],brks),to=c(brks-1,rng[2]),bit=1:(length(brks)+1))
  std=object@params[1:nbrks]
  z  =merge(mdply(t.,function(from,to,bit) data.frame(year=seq(from,to),bit=bit))[,3:4],
            data.frame(std,bit=1:(dim(std)[1])),all=TRUE)
  
  std=object@se[1:nbrks]
  se =merge(mdply(t.,function(from,to,bit) data.frame(year=seq(from,to),bit=bit))[,3:4],
            data.frame(std,bit=1:(dim(std)[1])),all=TRUE)
  
  res=cbind(z,se[,3])
  names(res)[3:4]=c("z","sd")
  
  res=res[,c("year","z","sd","bit")]
  
  return(res[order(res$year),])}
        
#############################################################################################
setGeneric('seine', function(object,...)
   standardGeneric('seine'))
setMethod('seine', signature(object='FLsz'),
  function(object,cmdOps=paste("-maxfn 500"),admbNm="seine")
  {  
  object=window(object,start=range(object)["minyear"],end=range(object)["maxyear"])
  dimnames(object@grw)$params=tolower(dimnames(object@grw)$params)  
  
  fn=function(object,cmdOps,admbNm){  
    
    ## Data
    ObsLength    = object@obs
    SampleSize   = object@n
   
    ObsLength[ is.na(ObsLength)]=mean(ObsLength,na.rm=T)
    SampleSize[is.na(SampleSize)]=0
 
    ## Parameters
    nbreaks= dim(object@params)[1]/2 -1
    zguess =array(c(object@bounds[,"initial"][1:(nbreaks+1)],
                    object@bounds[,  "phase"][1:(nbreaks+1)]),c(nbreaks+1,2))
 
    yguess =array(c(object@bounds[,"initial"][1+nbreaks+(1:(nbreaks))],
                    object@bounds[,  "phase"][1+nbreaks+(1:(nbreaks))]),c(nbreaks,2))
 
    sigma  =rev(object@bounds[,"initial"])[1]
   
    ## Growth
    KParm        = object@grw["k"]
    LInf         = object@grw["linf"]
    Lc           = object@grw["lc"]
        
    ## Output data file
    dat<-       list("Number of Breaks"                            =nbreaks,
                     "First Year of Data"                          =range(object)["minyear"],
                     "Last Year of Data"                           =range(object)["maxyear"],
        
                     "(1,NYears Observed Mean Lengths)"            =ObsLength,
                     "(1,NYears Observed Sample Sizes)"            =SampleSize,
          
                     "VB K Parameter"                              =KParm,
                     "VB L-Infinity"                               =LInf,
          
                     "Critical Length - Length at first capture"   =Lc,
        
                     "(1,NBreaks+1,1,2)"                           =c(t(zguess)),
                     "(1,NBreaks,  1,2)"                           =c(t(yguess)),
          
                     "sigma"                                       =sigma,
                     "stepsize"                                    =5,
                     "casenum"                                     =10)
  
     #### Run ADMB.exe
     writeADMB(dat,file=paste(pathNm,"/seine.dat",sep=""))
               
     pathOrg<-getwd()
     setwd(pathNm)
          
     sys.result=system(paste("./", admbNm, " ", cmdOps, sep=""))
             
     setwd(pathOrg)
        
     rep=readADMB(paste(pathNm,"/seine.rep",sep=""))

     std=read.table(paste(pathNm,"/seine.std",sep=""),skip=1)[,-1]
     names(std)=c("param","value","sd")
 
      params(object)=bounds(object)[,"initial"]
      params(object)[bounds(object)[,"phase"]>0]=std[,"value"]
      object@se[]= 0
      object@se[object@bounds[,"phase"]>0]=std[,"sd"]
      object@hat      =FLQuant(rep$hat,      dimnames=dimnames(object@obs))
      object@residuals=FLQuant(rep$Residuals,dimnames=dimnames(object@obs))
      
      # object@vcov    =rep$vcov     
      # object@hessian =rep$hessian      
      # object@logLik  =rep$logLik     
      # object@rsdlvar =rep$rsdlVar         
      # object@dof     =rep$dof    
      # object@stopmess=rep$stopmess
      # object@aic     =rep$aic

     return(object)}
      
  ### TO DO #####################################################
  ##  doesnt yet work with iterations!
  ##  If either n or obs have more than 1 iteration then need to 
  ##  make iters match for other slots i.e.  
    
  iterSlots=c("obs","hat","n","residuals",
              "grw",
              "params","se","vcov","hessian",
              "logLik","rsdlVar","stopmess")
  
  iters=mlply(iterSlots, function(x,object) dimnames(slot(object,x))$iter, object=object)

  if (dim(object@obs)[6]>1 | dim(object@n)[6]>1) {

    ## check all iters are 1 or n            
    nits=laply(iters,length)
              
    ## make 1s Ns                                   
    #   m_ply(iterSlots[nits>1], function(x,object,nIts) { 
    #           if (dim( slot(object,x))|=nits)    
    #              slot(object,x)=propagate(slot(object,x),nits)) 
    #          object        <<-object},
    #           object=object,nIts=max(nits))
              
    ## then do an FLComp::apply over iter iter dim   
    ##res=qqply(object, 6,  fn(object),cmdOps=cmdOps,admbNm=admbNm)
    }
  
  ##############################################################
  object=fn(object,cmdOps=cmdOps,admbNm=admbNm)
                       
  return(object)})

#setMethod("diags", signature(object="data.frame"),
  diags.=function(object, i=NULL) {
    
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

    return(res)}
#)

ini=function(object,breaks){
    its    = 1:max(dim(object@obs)[6],dim(object@n)[6])
    nbreaks=length(breaks)
    parNms =c(paste("z",1:(length(breaks)+1),sep=""),paste("brk",1:nbreaks,sep=""),"sigma")
    nPar   =length(parNms)  
    
    par       =c(rep(0.5,nbreaks+1),breaks,10) 
    names(par)=parNms
    par       =FLPar(par)
    se        =par
    bounds    =array(NA, dim=c(length(parNms),4),dimnames=list(param=parNms,c("phase","lower","initial","upper")))
    priors    =array(NA, dim=c(length(parNms),3),dimnames=list(param=parNms,c("a","b","pdf")))
    
    object@params=par
    object@se    =se
    object@priors=priors
    object@bounds=bounds 
    
    object@bounds[,"phase"]  =c(rep(1,nbreaks+1),rep(-1,nbreaks),1)
    object@bounds[,"initial"]=apply(object@params,1,mean,na.rm=TRUE)
 
    object@vcov   =array(NA,c(length(nPar),length(nPar),length(its))) 
    object@hessian=object@vcov 
    
    return(object)}

setGeneric("breaks<-", function(object,value){
  standardGeneric("breaks<-")})
setMethod("breaks<-", signature(object="FLsz", value="numeric"),
  function(object, value) ini(object,value))

setGeneric("breaks=", function(object,value){
  standardGeneric("breaks=")})
setMethod("breaks=", signature(object="FLsz", value="numeric"),
  function(object, value) ini(object,value))

setMethod("sz", signature(object="FLsz"),
  function(object) sz(object@obs,object@grw))

readADMBMCMChst=function(fl){
   t.=scan(fl,sep="\n",what=character())
      
   idx=1:length(t.)
   idx=c(idx[substr(t.,1,1)=="#"],length(t.)+1)
   idx=data.frame(from=rev(rev(idx)[-1])+1,to=idx[-1]-1)
      
   t.. =llply(mlply(idx, seq),function(x) t.[x])
    
   data=llply(t..,  function(x){
                        res=unlist(strsplit(x," "))
                        res=res[nchar(res)>0]
                        as.numeric(res)})
                   
   ## headings
   nms=t.[substr(t.,1,1)=="#"]
   names(data)=gsub(" ", "", substr(nms,2,nchar(nms)))
     
   data[11:length(data)]=llply(data[11:length(data)], function(x) data.frame(t(array(x,c(2,length(x)/2),dimnames=list(c("value","density"),NULL)))))
      
   nPar=length(data[[4]])          
  
   ary=array(NA,c(nPar,5), list(param=sub("std_","",names(data)[10+1:nPar]),c("mean","sd","lower","upper","step")))          
   ary[,"mean"] =data$means
   ary[,"sd"]   =data$standarddevs
   ary[,"lower"]=data$lowerbounds
   ary[,"upper"]=data$upperbounds
   ary[,"step"] =data$stepsizes
     
   pdf=mdply(names(data)[10+1:nPar], function(x,data) data[[x]], data=data)
   pdf=transform(pdf,param=dimnames(ary)$param[X1])
   pdf=transform(pdf,aprox=qnorm(value,ary[param,"mean"],ary[param,"sd"]))[,c("param","value","density","aprox")]

   smry=c("samplessize"=data[[ 1]],
          "scaling"    =data[[ 2]],
          "nPar"       =data[[ 8]],   
          "seed"       =data[[10]])
  
  restart=data[[9]]   
   
  return(data)}

  #return(list(param=ary,pdf=pdf,summary=smry,restart=restart))}
