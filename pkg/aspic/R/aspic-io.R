
setMethod("readAspic",     signature(x="character"),                   function(x,...)            .readAspic(x,...))

setMethod("aspicRuns",     signature(x="character",scen="vector"),     function(x,scen,...)       .aspicRuns(x,scen,...))
setMethod("aspicRuns",     signature(x="character",scen="data.frame"), function(x,scen,...)       .aspicRuns(x,scen,...))

setMethod("aspicProj",     signature(x="character",scen="vector"),     function(x,scen,stringsAsFactors=FALSE,...)       .aspicProj(x,scen,stringsAsFactors,...))
setMethod("aspicProj",     signature(x="character",scen="data.frame"), function(x,scen,stringsAsFactors=FALSE,...)       .aspicProj(x,scen,stringsAsFactors,...))

setMethod("aspicCpue",     signature(x="character"),                   function(x,...)            .aspicCpue(x,...))

setMethod("writeAspic",    signature(x="aspic"),                       function(x,idx,what="FIT",niter=1,fl="aspic.inp",...)        .writeAspicInp(x,idx,what,niter,fl=fl,...))

# ggplotFL/R/diags.R
# 
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

## local function to calculated expected QQ line
qqLine <- function(x,y){ 
  qtlx <- quantile(x, prob=c(0.25,0.75), na.rm=T)
  qtly <- quantile(y, prob=c(0.25,0.75), na.rm=T)
      
  a <- (qtly[1]- qtly[2]) / (qtlx[1] - qtlx[2])
  b <- qtly[1] - qtlx[1] * a
      
  res <- c(a,b)
        
  names(res) <- NULL
  names(res) <- c("a","b")

 return(res)}

 fnDiags=function(res){
      res$residualLag <- c(res$residual[-1],NA)
     
      qq.     <- qqnorm(res$residual,plot.it=FALSE,na.rm=T)
      res$qqx <- qq.$x
      res$qqy <- qq.$y
      
      qqpar <- qqLine(qq.$x,qq.$y)[c("a","b")]

      res$qqHat=qqpar["a"]*res$qqx+qqpar["b"]
      
      res}

aspicFiles=data.frame(ext =c("inp","bio","prb","rdat","det","prn"),
                      desc=c("Input file with data, starting guesses, and run settings",
                             "Estimated B and F trajectory for each bootstrap trial",
                             "As .bio but with projection results",
                             "Inputs and estimates specially formatted for R",
                             "Estimates from each bootstrap trial",
                             "cpue fitted and observed"),stringAsFactor=FALSE)

getExt <- function(file)
  tolower(substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)))

checkExt=function(x) (tolower(getExt(x)) %in% aspicFiles[,"ext"])

#### Aspic #####################################################################################
.readAspic<-function(x){

  if (!checkExt(x)) stop(cat("File", x, "does not have a valid extension")) 
  type=getExt(x)
  return(switch(tolower(type),
                "inp" =aspicInp(x),
                "bio" =aspicBio(x),
                "prb" =aspicPrb(x),
                "rdat"=aspicRdat(x),
                "ctl" =aspicCtl(x),
                "det" =aspicDet(x),
                "prn" =aspicPrn(x)))}

################################################################################
aspicBio =function(file){
       t.  <-scan(file,skip=4)
       nits<-scan(file,skip=1,nmax=1)
       yrs <-scan(file,skip=2,nmax=2)
       nyrs<-diff(yrs)
       nval<-nyrs*2+3
              
       yrs <-yrs[1]:yrs[2]
              
       b.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+2,     1:nits,function(x,y=nyrs+1) x:(x+y-1)))],dimnames=list(year=yrs,               iter=1:nits))
       f.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+4,1:nits,function(x,y=nyrs)   x:(x+y-1)))],dimnames=list(year=yrs[-length(yrs)], iter=1:nits))
                 
       bmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+1,     1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(                        iter=1:nits))
       fmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+3,1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(                        iter=1:nits))
              
       return(FLQuants(stock=sweep(b.,6,bmsy,"/"),harvest=sweep(f.,6,fmsy,"/"),bmsy=bmsy,fmsy=fmsy))}

aspicPrb =function(file){
        ## Stuff
        nits<-scan(file,skip=1,nmax=1)
        yrs <-scan(file,skip=2,nmax=2)
        t.  <-scan(file,skip=4)
        ncol<-yrs[2]-yrs[1]+2

        ## biomass
        first<-rep((1:nits-1)*ncol*2,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
        b.   <-FLQuant(t.[first],dimnames=list(year=yrs[1]:yrs[2],iter=1:nits))
        first<-((1:nits-1)*ncol*2)+1
        bmsy <-FLQuant(t.[first],dimnames=list(iter=1:nits))
        b.   <-sweep(b.,6,bmsy,"/")

        ## F
        first<-rep((1:nits-1)*ncol*2+ncol,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
        f.   <-FLQuant(t.[first],dimnames=list(year=yrs[1]:yrs[2],iter=1:nits))[,ac(yrs[1]:(yrs[2]-1))]
        first<-((1:nits-1)*ncol*2)+ncol+1
        fmsy <-FLQuant(t.[first],dimnames=list(iter=1:nits))
        f.   <-sweep(f.,6,fmsy,"/")

        return(FLQuants(harvest=f.,biomass=b.))}
                
aspicRdat=function(file){            
        return(dget(file))}

aspicDet =function(x){  
       read.table(det,header=TRUE)}

    
aspicInp =function(x){
  
  aspicC=function(file) {  
          inp=scan(file,sep="\n",what=character())
          ctrl=mlply(inp[5:21], function(x) {
                          tmp=strsplit(x," ")
                          tmp=unlist(tmp)[nchar(unlist(tmp))>0]})
          ctrl=llply(ctrl,as.numeric)
          ctrl=llply(ctrl,function(x) x[!is.na(x)])

        return(ctrl)}
 
  ctrl=suppressWarnings(aspicC(x))
  
  res=aspic()
      
  #  [8] "7  ## Number of fisheries (data series)"                                                                                           
  n     =ctrl[[8]]  
  params=FLPar("b0"=NA,"k"=NA,"msy"=NA)
  parNms=c(c("b0","msy","k"),paste("q",seq(n),sep=""))
  res@bounds=array(NA,c(length(c(c("b0","msy","k"),paste("q",seq(n),sep=""))),5),dimnames=list(params=parNms,c("fit","min","start","max","lambda")))
  
  # [10] "1.00000  ## B1/K (starting guess, usually 0 to 1)"                                                                                 
  res@bounds["b0", "start"]=ctrl[[10]][1]
  # [11] "3.0000E+04  ## MSY (starting guess)"                                                                                               
  res@bounds["msy","start"]=ctrl[[11]]
  # [12] "2.6700E+05  ## K (carrying capacity) (starting guess)"                                                                             
  res@bounds["k", "start"]=ctrl[[12]]
  # [13] "2.1126E-06  6.0195E-06  9.7627E-06  1.4944E-04  2.9980E-06  4.2138E-04  8.3406E-04    ## q (starting guesses -- 1 per data series)"
  res@bounds[parNms[-(1:3)],"start"]=ctrl[[13]][1:n]

  # [14] "0  1  1  1  1  1  1  1  1  1    ## Estimate flags (0 or 1) (B1/K,MSY,K,q1...qn)"                                                   
  res@bounds[,"fit"]=ctrl[[14]]

  # [15] "1.0000E+02  1.0000E+07  ## Min and max constraints -- MSY"                                                                         
  res@bounds["msy",c("min","max")]=ctrl[[15]]
  # [16] "1.0000E+04  2.0000E+07  ## Min and max constraints -- K"                                                                           
  res@bounds["k",  c("min","max")]=ctrl[[16]]
  
  res@bounds[parNms[-(1:3)],"min"]=res@bounds[parNms[-(1:3)],"start"]*0.01  
  res@bounds[parNms[-(1:3)],"max"]=res@bounds[parNms[-(1:3)],"start"]*100  
  res@bounds["b0","min"]=0.01  
  res@bounds["b0","max"]=1  
    
  #  [9] "1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E-02  1.0000E+00  1.0000E-02    ## Statistical weights for data series"      
  if (length(ctrl[[9]])==0)
    res@bounds[parNms[-(1:3)],"lambda"][]=1.0 else
    res@bounds[parNms[-(1:3)],"lambda"]=ctrl[[9]]
   
  # [17] "6745260  ## Random number seed
  res@rnd   =ctrl[[17]]
#     
#   #  [3] "1.0000E-08  ## Convergence crit. for simplex"  
#   if (length(ctrl[[3]])==0) ctrl[[3]]=1.0-08  
#   res@conv["simplex"]=ctrl[[3]]
#     
#   #  [4] "3.0000E-08  6  ## Convergence crit. for restarts, N restarts"                                                                      
#   res@conv["restart"]=ctrl[[4]][1]
#   res@nRestart=ctrl[[4]][1]
#  
#   #  [5] "1.0000E-04  0  ## Conv. crit. for F; N steps/yr for gen. model"                                                                    
#   res@conv["F"]=ctrl[[5]][1]
#   res@nSteps   =ctrl[[5]][1]
#     
#   #  [6] "8.0000  ## Maximum F when cond. on yield"                                                                                          
#   res@maxF    =ctrl[[6]]
#   #  [7] "0.0  ## Stat weight for B1>K as residual (usually 0 or 1)" 
#   res@wt      =ctrl[[7]][1]
    
  return(res)}

aspicPrn =function(x){
    #x="/home/lkell/Dropbox/MyStuff/WHM/analysis/Inputs/aspic/Base case runs/whmrun1bb.prn"
    res=read.table(x,header=TRUE)
   
    res=res[,seq(dim(res)[2]-2)]
    
    obs=melt(res[,seq((dim(res)[2]-1)/2+1)],id.var="year")
    est=melt(res[,c(1,((dim(res)[2]-1)/2+2):dim(res)[2])],id.var="year")
    
    res=data.frame(transform(obs,obs=value,cpue=gsub(".obs","",obs$variable))[,c("year","cpue","obs")],
                   hat=est$value)
    
    res$residual=log(res$obs/res$hat)
    
    res=ddply(res,.(cpue),fnDiags)
    
    res}

### x is the dir & file name  
.aspicRuns<-function(x,scen){
   res   <-mdply(scen, function(x,dir) as.data.frame(readAspic(paste(dir,"/",x,".bio",sep=""))), dir=x)
   ts    <-subset(res, qname %in% c("biomass","harvest"))
   refpts<-subset(res, qname %in% c("bmsy",   "fmsy"))

   ## Quantiles in data.frame
   qtls<-transform(melt(ddply(ts,.(X1,year,qname),function(x) quantile(x$data,prob=c(0.25,0.5,0.75))),id.vars=c("X1","qname","year")),
          scenario=factor(X1),quantity=factor(qname), quantile=variable)[,c("scenario","quantity","year","quantile","value")]

   ## Model frame with points
   ts<-cast(subset(res, qname %in% c("biomass","harvest"),select=c("X1","year","iter","data","qname")), 
 	 X1+year+iter~qname,value="data")
   
   names(ts)<-c("scenario","year","iter","biomass","harvest")

   return(list(ts=ts,quantiles=qtls,refpts=refpts))}

.aspicProj<-function(x,scen,stringsAsFactors,...){
    prj       <-subset(mdply(scen, function(scen,dir) as.data.frame(readAspic(paste(dir,scen,".prb",sep=""))), dir=x), !is.na(data))   
    
    names(prj)[1]="scen"
    if (!stringsAsFactors) prj$scen=as.numeric(ac(prj$scen))
    
    prj       <-cast(prj,scen+year+iter~qname,value="data") 
    names(prj)<-c("scen","year","iter","biomass","harvest")

    prjP      <-cbind(prj,kobeP(prj$biomass,prj$harvest))

    prjP      <-ddply(prjP,.(scen,year), function(x) with(x,  cbind(f=mean(f),b=mean(b),p=mean(p),
                                                                    collapsed  =mean(collapsed))))

    return(list(ts=prj,kobe=prjP))}


.aspicCpue=function(x){
                    
  uN=scan(x,sep="\n",what=character())[22]
  uN=substr(uN,1,regexpr("#",uN)[1]-1)  
  uN=as.numeric(strsplit(uN," ")[[1]])
  uN=uN[!is.na(uN)]
  
  #uN=gsub("(\\s+)", " ", uN, perl=TRUE)
  #uN=strsplit(uN," ")[[1]]
  #uN=as.numeric(uN)
  
  us=scan(x,sep="\n",what=character())[-(1:22)]
  
  pos=cumsum(c(rbind(1,1,uN)))
  
  nms=us[pos[seq(1,length(pos),3)]]
  cde=us[pos[seq(2,length(pos),3)]]
  cde=gsub("(\\s+)", "", cde, perl=TRUE)
  
  pos=mlply(data.frame(from=pos[seq(2,length(pos),3)]+1,
                       to  =pos[seq(3,length(pos),3)]),seq)
  
  
  cpue=llply(pos,function(x) us[x])
  
  cpueFn=function(nm,dat,cd){
          
        res=mdply(data.frame(x=dat), function(x)  as.numeric(strsplit(gsub("\\s+"," ", x, perl=T)," ")[[1]]))[,-1]
        
        res[res<0]=NA
          
        names(res)[1]="year"
        
        if (cpueCode[cd,"ncol"]>=2)
          names(res)[2]=ac(cpueCode[cd,"col2"])
       
        if (cpueCode[cd,"ncol"]>=3)
          names(res)[3]=ac(cpueCode[cd,"col3"])
         
        idx= FLIndex(index=FLQuant(NA,dimnames=list(year=res$year)),
                     name =nm,
                     desc =ac(cpueCode[cd,"desc"]),
                     type =cd,
                     range=c(minyear=min(res$year),maxyear=max(res$year),unlist(c(cpueCode[cd,c("startf","endf")])))
                     )
        
        if (cpueCode[cd,"col2"]=="index")
          index(idx)[]=res[,2]
        
        if (cpueCode[cd,"col2"]=="biomass")
          index(idx)[]=res[,2]
        
        if (cpueCode[cd,"col2"]=="effort")
          index(idx)[]=res[,2]/res[,3]
       
        if (cpueCode[cd,"col3"]=="catch")
          catch.n(idx)[]=res[,3]
       
        return(idx)}
  
  res=FLIndices(mlply(seq(length(nms)), function(x,nms,cpue,cds) {print(x); cpueFn(nms[x],cpue[[x]],cde[x])}, nms=nms, cpue=cpue,cds=cde))
  
  return(res)}
  
.writeAspicInp<-function(object,cpue,what="FIT",niter=ifelse(what=="FIT",1,501),fl="aspic.inp"){
    comment=rep("",22)
    comment[ 1]= "\n"                                                                                               
    comment[ 2]= "\n"                                                                                               
    comment[ 3]= "\n"                                                                                                            
    comment[ 4]= "\t212 ## Verbosity\n"                                                                                                                  
    comment[ 5]= "\t## Number of bootstrap trials, <= 1000\n"                                                                                     
    comment[ 6]= "\t## 0=no MC search, 1=search, 2=repeated srch; N trials\n"                                                                  
    comment[ 7]= "\t## Convergence crit. for simplex\n"                                                                                      
    comment[ 8]= "\t## Convergence crit. for restarts, N restarts\n"                                                                      
    comment[ 9]= "\t## Conv. crit. for F; N steps/yr for gen. model\n"                                                                    
    comment[10]= "\t## Maximum F when cond. on yield\n"                                                                                          
    comment[11]= "\t## Stat weight for B1>K as residual (usually 0 or 1)\n"                                                                         
    comment[12]= "\t## Number of fisheries (data series)\n"                                                                                           
    comment[13]= "\t## Statistical weights for data series\n"      
    comment[14]= "\t## B1/K (starting guess, usually 0 to 1)\n"                                                                                 
    comment[15]= "\t## MSY (starting guess)\n"                                                                                               
    comment[16]= "\t## K (carrying capacity) (starting guess)\n"                                                                             
    comment[17]= "\t## q (starting guesses -- 1 per data series)\n"
    comment[18]= "\t## Estimate flags (0 or 1) (B1/K,MSY,K,q1...qn)\n"                                                   
    comment[19]= "\t## Min and max constraints -- MSY\n"                                                                         
    comment[20]= "\t## Min and max constraints -- K\n"                                                                           
    comment[21]= "\t## Random number seed\n"                                                                                                    
    comment[22]= "\t## Number of years of data in each series\n" 

    # search    trials   simplex  restarts nrestarts    effort    nsteps      maxf 
    #1e+00     1e+05     1e-08     3e-08     6e+00     1e-04     0e+00     8e+00 
    
    cat(what                                             ,comment[ 1],file=fl,append=FALSE)
    cat("FLR generated"                                  ,comment[ 2],file=fl,append=TRUE)
    cat(ac(model(object)), ac(object@conditioning), ac(object@obj)  ,comment[ 3],file=fl,append=TRUE)
    cat("112"                                            ,comment[ 4],file=fl,append=TRUE)
    cat(niter                                            ,comment[ 5],file=fl,append=TRUE)
    cat(as.integer(object@options[c("search","trials")])             ,comment[ 6],file=fl,append=TRUE)
    cat(object@options["simplex"]                        ,comment[ 7],file=fl,append=TRUE)
    cat(object@options["restarts"],as.integer(object@options["nrestarts"]),comment[ 8],file=fl,append=TRUE)
    cat(object@options["effort"],object@options["nsteps"],comment[ 9],file=fl,append=TRUE)
    cat(object@options["maxf"]                           ,comment[10],file=fl,append=TRUE)
    cat(0                                                ,comment[11],file=fl,append=TRUE)
    cat(dim(object@bounds)[1]-3                          ,comment[12],file=fl,append=TRUE)
    cat(object@bounds[-(1:3),"lambda"]                   ,comment[13],file=fl,append=TRUE)
    cat(object@bounds["b0",  "start"]                    ,comment[14],file=fl,append=TRUE)
    cat(object@bounds["msy", "start"]                    ,comment[15],file=fl,append=TRUE)
    cat(object@bounds["k",   "start"]                    ,comment[16],file=fl,append=TRUE)
    cat(object@bounds[-(1:3),"start"]                    ,comment[17],file=fl,append=TRUE)
    cat(object@bounds[      ,"fit"]                      ,comment[18],file=fl,append=TRUE)
    cat(object@bounds["msy",c("min","max")]              ,comment[19],file=fl,append=TRUE)
    cat(object@bounds["k",  c("min","max")]              ,comment[20],file=fl,append=TRUE)
    cat(object@rnd                                       ,comment[21],file=fl,append=TRUE)
    cat(laply(cpue, function(x) dims(x)$year)            ,comment[22],file=fl,append=TRUE)
    
    l_ply(cpue, 
        function(idx){
            cat(name(idx)                                       ,"\n",file=fl,append=TRUE)
            cat(type(idx)                                       ,"\n",file=fl,append=TRUE)
            
            mm=model.frame(FLQuants(col2=index(idx),col3=catch.n(idx)),drop=T)
            mm[is.na(mm)]=-1
                 
            cat(paste(t(apply(as.matrix(mm),1,paste,collapse=" ")),"\n"),file=fl,append=TRUE)})

        
    return()}