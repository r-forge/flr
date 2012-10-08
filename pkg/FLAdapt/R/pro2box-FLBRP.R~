#pro2box-FLBRP

pro2boxFLBRP=function(x){
  require(FLAdvice)
  require(stringr)
  
  getGrw=function(x){
    
    grw=lapply(strsplit(x," +"),as.numeric)
    
    par=FLPar(NA,dimnames=list(params=c("linf","k","t0","a","b","offset","spwn"),wt=c("stock","catch"),iter=1))
    
    par[c("linf","k","t0","a","b"),1]=grw[[1]][c(2:4,6:7)]
    par[c("linf","k","t0","a","b"),2]=grw[[2]][c(2:4,6:7)]
    par["offset",1]=(grw[[1]][8]-grw[[1]][7])/12
    par["offset",2]=(grw[[2]][8]-grw[[2]][7])/12
    
    par["spwn",1]=grw[[1]][7]/12
    par["spwn",2]=grw[[2]][7]/12
    
    par["t0"]=par["t0"]+par["t0"]+par["t0"]+par["offset"]
    
    return(par)}
  
  getP2boxCtrl=function(object){
    
    ctrl=scan(object,what=as.character(),sep="\n")
    ctrl=gsub("\t+", " ", str_trim(ctrl))
    ctrl=str_trim(ctrl[substr(ctrl,1,1)!="#"])
    
    return(ctrl)}
    
    ops=options()
    options(warn=-1)
    
    s2n=function(x) as.numeric(str_trim(strsplit(x," ")[[1]]))
  
    ctrl=getP2boxCtrl(x)
    dmns=list(age=s2n(ctrl[12])[1]:s2n(ctrl[12])[2])
    
    mat.=gsub("\t+", " ", ctrl[18])
    mat.=FLQuant(as.numeric(strsplit(mat.," +")[[1]][length(dmns$age)]),dimnames=dmns)
   
    res=FLBRP(mat=mat.)
    
    name(res)=""
    res@desc="read in from pro2box"
    
    res@range[c("minsel","maxsel")]=s2n(ctrl[13])
    
    bycatch.harvest(res)[]=0
    availability(res)[]=1           
    
    dir.=getDir(x)
    dmns=list(age=res@range["min"]:res@range["max"])
    
    getOut=function(fl,dir.,dmns){
      if (fl[1]==0)
        rtn  =FLQuant(scan(paste(dir.,fl[2],sep="/"))[seq(length(dmns$age))],dimnames=dmns)
      else if (fl[1]==1)
        rtn  =readBinary(paste(dir.,fl[2],sep="/"),dmns)
    
      return(rtn)}
    
    ### m
    dmns$iter=seq(s2n(ctrl[3][[1]][1]))
    m(res)=getOut(strsplit(ctrl[26]," +")[[1]][1:2],dir.,dmns)
    dmns=dimnames(m(res))
    
    discards.sel(   res)[]=0 
    bycatch.harvest(res)[]=0
    
    s.=scan(paste(dir.,strsplit(ctrl[27]," +")[[1]][2],sep="/"))
    landings.sel(res)=FLQuant(as.numeric(gsub("\t+", " ", s.)),dimnames=dmns)
    
    if (all(as.numeric(strsplit(ctrl[13]," ")[[1]])>0)){ 
       dmns$year=s2n(ctrl[11])[1]:s2n(ctrl[11])[2]
       f.=getOut(strsplit(ctrl[23]," +")[[1]][1:2],dir.,dmns)
       f.=apply(f.[,ac(c(range(res)["minsel"]:range(res)["maxsel"]))],c(1,6),function(x) exp(mean(log(x))))
       landings.sel(res)=f.%*%landings.sel(res)
       
       landings.sel(res)=sweep(landings.sel(res),6,fapex(landings.sel(res)),"/")
       }
    
    #srr
    srr  =scan(paste(dir.,strsplit(ctrl[28]," +")[[1]][2],sep="/"),sep="\n",what=character(),nlines=1)
    srr  =gsub("\t+", " ", str_trim(srr))
    srr  =strsplit(srr," +")[[1]][1:7]
    
    model(res)=switch(srr[1],
                        "1"=bevholt()$model,
                        "2"=ricker()$model, 
                        "3"=segreg()$model, 
                        "4"=segreg()$model, 
                        "5"=segreg()$model, 
                        "6"=geomean()$model)
    
    srr=as.numeric(srr)
    
    params(res)=FLPar(srr[1:2])
    
    mmm=c("landings.sel","discards.sel","bycatch.harvest","stock.wt","landings.wt","discards.wt","bycatch.wt","m","mat","harvest.spwn","m.spwn","availability","price")
    
    maxAge=unique(unlist(qapply(res,function(x) dims(x)$age))[mmm])
    
    if (length(maxAge)>1)
       res=setPlusGroup(res,max(maxAge))

    discards.sel(   res)[]=0 
    bycatch.harvest(res)[]=0
    
    grw=getGrw(ctrl[19:20])
    landings.wt(res)=grw["a","catch"]*vonB(grw[,"catch"],ages(res@m))^grw["b","catch"]
    discards.wt(res)=grw["a","catch"]*vonB(grw[,"catch"],ages(res@m))^grw["b","catch"]
    bycatch.wt( res)=grw["a","catch"]*vonB(grw[,"catch"],ages(res@m))^grw["b","catch"]
    stock.wt(   res)=grw["a","stock"]*vonB(grw[,"stock"],ages(res@m))^grw["b","stock"]
    
    harvest.spwn(res)[]=grw["spwn","catch",drop=T] 
    m.spwn(res)[]      =grw["spwn","stock",drop=T]
    
    # Load given slots
    for(i in names(args))
      slot(res, i) = args[[i]]
    
    options(ops)
    
    return(brp(res))}

#tst=pro2boxFLBRP(x="/home/laurie/Desktop/Dropbox/ICCAT/SCRS/BFT/2012/VPA/projections/bench/Reported/2012/high/Prj12.ctl")

      