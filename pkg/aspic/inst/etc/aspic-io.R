setMethod("readAspic",     signature(object="character"),   function(object,...)            .readAspic(object,...))
setMethod("writeAspic",    signature(object="aspic"),       function(object,cpue=object@cpue,what="FIT",niter=1,fl="aspic.inp",...)        .writeAspicInp(object,cpue,what,niter,fl=fl,...))


.writeAspicInp<-function(object,cpue=object@cpue,what="FIT",niter=ifelse(what=="FIT",1,501),fl="aspic.inp"){
    
   dmmy=expand.grid(year=min(as.numeric(as.character(cpue$year))):max(as.numeric(as.character(cpue$year))),
                    name=unique(cpue$name))[,2:1]
  
   u=merge(dmmy,cpue,all=TRUE,sort=FALSE)   
   u$index[is.na(u$index)]=-1
   u$catch[is.na(u$catch)]=0
   
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
    
    cat(daply(u,.(name), with, length(name)),comment[22],file=fl,append=TRUE)
    d_ply(u,.(name), function(x) {
      cat(as.character(unique(x$name)),"\n",file=fl,append=TRUE)
      cat("CC\n",file=fl,append=TRUE)
      cat(apply(x[,c("year","index","catch")],1,paste, collapse=" "),sep="\n",file=fl,append=TRUE)})
    
#     l_ply(cpue, 
#         function(idx){
#             cat(name(idx)                                       ,"\n",file=fl,append=TRUE)
#             cat(type(idx)                                       ,"\n",file=fl,append=TRUE)
#             
#             mm=model.frame(FLQuants(col2=index(idx),col3=catch.n(idx)),drop=T)
#             mm[is.na(mm)]=-1
#                  
#             cat(paste(t(apply(as.matrix(mm),1,paste,collapse=" ")),"\n"),file=fl,append=TRUE)})

        
    return()}


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
  
  ## stock
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
  
  return(FLQuants(harvest=f.,stock=b.))}

aspicRdat=function(file){            
  return(dget(file))}

aspicDet =function(x){  
  read.table(det,header=TRUE)}

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
  
  names(res)[2]="index"
  
  res}



