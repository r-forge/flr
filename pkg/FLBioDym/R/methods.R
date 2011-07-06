# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, ICCAT
# $Id:  $

# harvest {{{
setMethod('harvest', signature(object='FLBioDym'),
  function(object) {
    res <- catch(object)/stock(object)[,dimnames(catch(object))$year]
    units(res) <- "hr"
    return(res)
  }
) # }}}

# residuals {{{
setMethod('residuals', signature(object='FLBioDym'),
  function(object) {
    if (object@distribution == "log")
      res <- (log(object@index)-log(fitted(object)[,dimnames(object@index)$year]))
    else
      res <- (object@index-fitted(object)[,dimnames(object@index)$year])
    return(res)
  }
) # }}}

# admbBD {{{
setMethod('admbBD', signature(object='FLBioDym'),
  function(object, cmdOps=paste("-maxfn 500"), pathNm=getwd(), admbNm="pella") {

    pathOrg<-getwd()
    setwd(pathNm)
  
    # admbDatBioDym: writes ADMB input file for model
    admbDatBioDym<-function(x,file) {
      ctc <- as.list(drop(model.frame(x[["catch"]])))
      ctc <- c(nYrs=length(ctc[[1]]),ctc)

      idx <- as.list(drop(model.frame(x[["index"]])))
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
# }}}
