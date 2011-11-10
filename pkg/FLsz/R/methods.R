pathNm="/home/lkell/flr/experimental/FLsz/inst/admb"

iters=function(what=NULL){
  it=c("model",         FALSE,   FALSE,    FALSE,    
       "obs",           TRUE,    TRUE,     FALSE,
       "hat",           TRUE,    FALSE,    TRUE,
       "n",             TRUE,    TRUE,     FALSE,
       "residuals",     TRUE,    FALSE,    TRUE,
      
       "grw",           TRUE,    TRUE,     FALSE,
       "se",            TRUE,    FALSE,    TRUE,
       "params",        TRUE,    FALSE,    TRUE,
       "bounds",        FALSE,   FALSE,    FALSE,
       "priors",        FALSE,   FALSE,    FALSE,
      
       "vcov",          TRUE,    FALSE,    TRUE,
       "hessian",       TRUE,    FALSE,    TRUE)
  
  it=t(array(it,c(4,12)))

  it=data.frame(array(as.logical(it[,-1]),dim=c(12,3),dimnames=list(slot=it[,1],iter=c("n","write","read"))))

  if (is.null(what)) return(it) else return(dimnames(it)[[1]][it[,what]])}

chkIters=function(object){
  
  ## any inputs iters>1
  itersW=maply(iters("write"), function(x) dims(slot(object,x))$iter)
  
  ## set outputs
  if (!all(itersW %in% c(1,max(itersW)))) stop("Iter not ´1 or n´")
  nits=max(itersW)
  for (i in iters("read"))
    slot(object, i) = propagate(slot(object, i),nits)
  
  object}
  

ini=function(object,breaks){
    its    = 1:max(dim(object@obs)[6],dim(object@n)[6])
    nbreaks=length(breaks)
    parNms =c(paste("z",1:(length(breaks)+1),sep=""),paste("brk",1:nbreaks,sep=""),"sigma")
    nPar   =length(parNms)  
    object@hat      =FLQuant(NA, dimnames=dimnames(object@obs))
    object@residuals=FLQuant(NA, dimnames=dimnames(object@obs))

    par       =c(rep(0.5,nbreaks+1),breaks,10) 
    names(par)=parNms
    par       =FLPar(par)
    bounds    =array(NA, dim=c(length(parNms),4),dimnames=list(param=parNms,c("phase","lower","initial","upper")))
    priors    =array(NA, dim=c(length(parNms),3),dimnames=list(param=parNms,c("a","b","pdf")))
    
    object@params=par
    object@se    =par 
    object@se[]  =NA
    object@priors=priors
    object@bounds=bounds 
    
    object@bounds[,"phase"]  =c(rep(1,nbreaks+1),rep(-1,nbreaks),1)
    object@bounds[,"initial"]=apply(object@params,1,mean,na.rm=TRUE)
    
    dm  =c(dim(object@params)[1],dim(object@params)[1],dims(object)$iter)  
    dmns=list(params=dimnames(params(object))$params,
              params=dimnames(params(object))$params,
              iter  =dimnames(params(object))$iter)
    object@vcov   =FLPar(array(as.numeric(NA),dim=dm,dimnames=dmns))
    
    object@hessian=object@vcov 
    
    # "logLik":
    # "rsdlVar":
    # "dof":
    # "stopmess": 
    # "name":
    # "desc"
    
    return(object)}

# jk=mdply((1:length(n))[n!=0],
#     function(x,obs,n,rng,breaks,grw){
#        n[x]=0
#        object(obs,n,rng,breaks,grw)$std},
#     obs=obs,n=n,rng=rng,breaks=breaks,grw=grw)


hatFn=function(object){
  rng  =range(object)[c("minyear","maxyear")]
  nbrks=dim(object@params)[1]/2
  brks =iter(object@params,1)[1:(nbrks-1)+nbrks]
                      
  t.=data.frame(from=c(rng[1],brks),to=c(brks-1,rng[2]),bit=1:(length(brks)+1))
  std=object@params[1:nbrks]
  z  =merge(mdply(t.,function(from,to,bit) data.frame(year=seq(from,to),bit=bit))[,3:4],
            data.frame(std,bit=1:(dim(std)[1])),all=TRUE)
  
  std=object@se[1:nbrks]
  
  se =merge(mdply(t.,function(from,to,bit) 
              data.frame(year=seq(from,to),bit=bit))[,3:4],
              data.frame(std,bit=1:(dim(std)[1])),all=TRUE)
  
  res=cbind(z,se[,3])
  names(res)[3:4]=c("z","sd")
  
  res=res[,c("year","bit","z","sd")]
  
  return(res[order(res$year),])}

getHat=function(object)
  mdply(seq(dims(object)$iter), function(x,obj) cbind(iter=x,hatFn(iter(obj,x))), obj=object)[,-1]

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

setGeneric("breaks<-", function(object,value){
  standardGeneric("breaks<-")})
setMethod("breaks<-", signature(object="FLsz", value="numeric"),
  function(object, value) ini(object,value))

setGeneric("breaks=", function(object,value){
  standardGeneric("breaks=")})
setMethod("breaks=", signature(object="FLsz", value="numeric"),
  function(object, value) ini(object,value))

setGeneric("sz", function(object){
  standardGeneric("sz")})
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

setMethod("jacknife", signature(object="FLQuant"),
  function(object,...) {
    
    # get dimensions
    dmo <- dim(object)

    # propagate
    res <- propagate(object, prod(dmo) + 1)
  
    # create array with 1 at each location by iter
    idx <- array(c(TRUE, rep(NA, prod(dmo[-6]))), dim=c(dmo[-6], prod(dmo)))
    res[,,,,,-1][idx] <- NA

    attributes(res)$jacknife=TRUE
    return(res)}) 
  
chkJK=function(x)
  any(unlist(qapply(x, function(x)
            ifelse("jacknife" %in% names(attributes(x)), return(attributes(x)$jacknife), return(FALSE)))))
  
# data=readADMBMCMChst(fl)
# 
# 
#    nPar=length(data[[4]])          
#   
#    ary=array(NA,c(nPar,5), list(param=sub("std_","",names(data)[10+1:nPar]),c("mean","sd","lower","upper","step")))          
#    ary[,"mean"] =data$means
#    ary[,"sd"]   =data$standarddevs
#    ary[,"lower"]=data$lowerbounds
#    ary[,"upper"]=data$upperbounds
#    ary[,"step"] =data$stepsizes
#      
#    pdf=mdply(names(data)[10+1:nPar], function(x,data) data[[x]], data=data)
#    pdf=transform(pdf,param=dimnames(ary)$param[X1])
#    pdf=transform(pdf,aprox=qnorm(value,ary[param,"mean"],ary[param,"sd"]))[,c("param","value","density","aprox")]
# 
#    smry=c("samplessize"=data[[ 1]],
#           "scaling"    =data[[ 2]],
#           "nPar"       =data[[ 8]],   
#           "seed"       =data[[10]])
#   
#   restart=data[[9]]   
#  
# 
# ggplot(data[[12]])+geom_line(aes(value,density))

