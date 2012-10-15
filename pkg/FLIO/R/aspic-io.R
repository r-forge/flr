
setMethod("readAspic",     signature(x="character"),                   function(x,...)            .readAspic(x,...))
setMethod("aspicRuns",     signature(x="character",scen="vector"),     function(x,scen,...)       .aspicRuns(x,scen,...))
setMethod("aspicRuns",     signature(x="character",scen="data.frame"), function(x,scen,...)       .aspicRuns(x,scen,...))
setMethod("aspicProj",     signature(x="character",scen="vector"),     function(x,scen,stringsAsFactors=FALSE,...)       .aspicProj(x,scen,stringsAsFactors,...))
setMethod("aspicProj",     signature(x="character",scen="data.frame"), function(x,scen,stringsAsFactors=FALSE,...)       .aspicProj(x,scen,stringsAsFactors,...))


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
  parNms=c(c("b0","k","msy"),paste("q",seq(n),sep=""))
  res@bounds=array(NA,c(length(c(c("b0","k","msy"),paste("q",seq(n),sep=""))),5),dimnames=list(params=parNms,c("fit","min","start","max","lambda")))
  
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
  