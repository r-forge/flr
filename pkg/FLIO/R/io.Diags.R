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


setMethod("diags",  signature(x="character"), function(x,...) .diags(file=x,...))

.diags=function(x){

  if (gsub(" ", "", scan(df,sep="\n",what=character(),nlines=1,skip=1)) == "VPA-2BOX")
    return(readVpa2boxDiagsFn(x))
  }
  

readVpa2boxDiagsFn<-function(x){

    tab5<-scan(x,what="",sep="\n")
    tab5<-tab5[grep("TABLE 5.",tab5): length(tab5)]

    pos  <-grep("Chi-sq. discrepancy=",tab5)
    nms  <-substr(tab5[pos-7],10,30)
    str  <-pos+5
    end  <-grep("Selectivities",tab5)-2

    fn<-function(uDiag) {
        uDiag<-unlist(strsplit(uDiag," "))
        as.numeric(uDiag[nchar(uDiag)>0])}

    uDiag<-tab5[unlist(mapply(seq,str,end))]
    dim1 <-length(uDiag)
    uDiag<-fn(uDiag)
    dim2 <-length(uDiag)
    nVar<-dim2/dim1
    uDiag<-as.data.frame(t(array(uDiag,c(nVar,length(uDiag)/nVar))))
    uDiag<-data.frame(cpue=unlist(mapply(rep, nms,(end-str)+1)),uDiag,row.names=NULL)

    #uDiag$cpue<-factor(uDiag$cpue)
    if (nVar==9) names(uDiag)<-c("cpue","year","obs","hat","residual","sd","q","obs2","hat2","chi2")
    if (nVar==7) names(uDiag)<-c("cpue","year","obs","hat","residual","sd","q","chi2")

    uDiag$residual2<-c(uDiag$residual[-1],NA)
    uDiag[!duplicated(uDiag[,"cpue"]),"residual2"]<-NA

    fnQQ<-function(object){
       qq.          =qqnorm(c(object$residual),plot.it=FALSE)
       qqx          =qq.$x
       qqy          =qq.$y

       res<-data.frame(qqx=qqx,qqy=qqy)

       return(res)}

    uDiag<-data.frame(uDiag,fnQQ(uDiag)[,c("qqx","qqy")])

    qqLine<-function(obj){
           x<-obj$qqx
           y<-obj$qqy

           qtlx<-quantile(x,prob=c(0.25,0.75),na.rm=T)
           qtly<-quantile(y,prob=c(0.25,0.75),na.rm=T)

           a=(qtly[1]-qtly[2])/(qtlx[1]-qtlx[2])
           b=qtly[1]-qtlx[1]*a

           res<-c(a,b)
           names(res)<-NULL
           names(res)<-c("a","b")

           return(res)
           return(data.frame(y=x*res["a"]+res["b"]))}

    par<-qqLine(uDiag)
    uDiag$qqYhat<-uDiag$qqx*par["a"]+par["b"]

    return(uDiag)}
  
getPos<-function(file){
    lns   <-scan(file,what="",sep="\n")

    n     <-length(lns)
    minus1<-(1:n)[substr(lns,1,2)=="-1"]
    hash  <-(1:n)[substr(lns,1,1)=="#"]
    start <-c(hash[(hash[-1]-hash[-length(hash)])>1],hash[length(hash)])+1
    hash  <-rev(hash)
    end   <-c(rev(hash[hash[-length(hash)]-hash[-1]>1]-1),n)
    end[end %in% minus1]<-end[end %in% minus1]-1
    
    return(data.frame(start=start,end=end))}

getIdx<-function(x){
  posRng  <-1
  posSpc  <-5
  posIdx  <-6
  posVul  <-7
  posWaa  <-8
  
  # range
  t.        <-as.numeric(scan(x,skip=start[posRng]-1,nlines=end[posRng]-start[posRng]+1,what=character()))
  rng       <-t.[!is.na(t.)]
  names(rng)<-c("minyear","maxyear","min","max","plusgroup","pgproj")[1:length(rng)]
  
  # Specifications
  t.        <-as.numeric(scan(x,skip=start[posSpc]-1,nlines=end[posSpc]-start[posSpc]+1,what=character()))
  specs     <-t.[!is.na(t.)]
  specs     <-as.data.frame(t(array(specs,c(7,length(specs)/7))))
  names(specs)<-c("index","pdf","units","vul","timing","min","max")

  # index
  idx       <-as.numeric(scan(x,skip=start[indexPos]-1,nlines=end[indexPos]-start[indexPos]+1,what=character()))
  idx       <-idx[!is.na(idx)]
  n         <-length(idx)
  idx       <-as.data.frame(t(array(idx,dim=c(4,n/4))))
  names(idx)<-c("index","year","data","cv")

  # Vulnerabilities
  vul       <-as.numeric(scan(x,skip=start[posVul]-1,nlines=end[posVul]-start[posVul]+1,what=character()))
  vul       <-vul[!is.na(vul)]
  n         <-length(vul)
  nvar      <-diff(rng[c("min","max")])+3
  vul       <-as.data.frame(t(array(vul,dim=c(nvar,n/nvar))))  
  names(vul)[1:2]<-c("index","year")
  names(vul)[1:2]<-c("index","year")
  names(vul)[-(1:2)]<-seq(rng[c("min","max")])
  vul<-melt(vul,id=c("index","year"),variable="age")
  names(vul)[4]<-"data"

  # catch weight
  waa       <-as.numeric(scan(x,skip=start[posWaa]-1,nlines=end[posWaa]-start[posWaa]+1,what=character()))
  waa       <-waa[!is.na(waa)]
  n         <-length(waa)
  nvar      <-diff(rng[c("min","max")])+3
  waa       <-as.data.frame(t(array(waa,dim=c(nvar,n/nvar))))
  
  names(waa)[1:2]<-c("index","year")
  names(waa)[-(1:2)]<-seq(rng[c("min","max")])
  waa<-melt(waa,id=c("index","year"),variable="age")
  names(waa)[4]<-"data"

  return(list(range=rng,specs=specs,index=idx,vul=vul,waa=waa))} 



