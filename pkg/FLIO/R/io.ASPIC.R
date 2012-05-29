
setMethod("readAspic",     signature(x="character"),    function(x,...)       .readAspic(x,...))
setMethod("aspicRuns",     signature(x="character",scen="vector"),     function(x,scen,...)       .aspicRuns(x,scen,...))
setMethod("aspicRuns",     signature(x="character",scen="data.frame"), function(x,scen,...)       .aspicRuns(x,scen,...))
setMethod("aspicProj",     signature(x="character",scen="vector"),     function(x,scen,stringsAsFactors=FALSE,...)       .aspicProj(x,scen,stringsAsFactors,...))
setMethod("aspicProj",     signature(x="character",scen="data.frame"), function(x,scen,stringsAsFactors=FALSE,...)       .aspicProj(x,scen,stringsAsFactors,...))


aspicFiles=data.frame(ext =c("inp","bio","prb","rdat","det"),
                      desc=c("Input file with data, starting guesses, and run settings",
                             "Estimated B and F trajectory for each bootstrap trial",
                             "As .bio but with projection results",
                             "Inputs and estimates specially formatted for R",
                             "Estimates from each bootstrap trial"),stringAsFactor=FALSE)

getExt <- function(file)
  substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)) # }}}

checkExt=function(x) (getExt(x) %in% aspicFiles[,"ext"])


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
                "det" =aspicDet(x)))}

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
              
       return(FLQuants(biomass=sweep(b.,6,bmsy,"/"),harvest=sweep(f.,6,fmsy,"/"),bmsy=bmsy,fmsy=fmsy))}

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
    return("will return an aspic object")}

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
  