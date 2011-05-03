#### To Do ######################################################################
# 1) par operators return matrix, correct so return FLPar
# 2) shift dnormal & logistic to ogive.R and add to FLBRP
# 3) Document M & mat50 and add to FLBRP

### coercion to parameters
mmm<-function(x){
    switch(class(x),
           list   =FLPar(unlist(x)),
           numeric=FLPar(x),
           x)}

#### Life History Generator ####################################################
setGeneric('gislaSim', function(grw,...)
   standardGeneric('lhSim'))
setMethod('gislaSim', signature(grw="FLPar"),
   function(grw,
            mat=FLPar("a50"=NA,"ato95"=3),
            sel=FLPar(a=1,sl=1,sr=1e6),
            m  =NULL,
            sr =list(model="bevholt",steepness=0.9,vbiomass=1e3),
            age=1:40,...){

            if (!("k"  %in% dimnames(grw)$params)) grw=addPar(grw,"k",0.5)
            if (!("t0" %in% dimnames(grw)$params)) grw=addPar(grw,"t0",0)
            if (!("a"  %in% dimnames(grw)$params)) grw=addPar(grw,"a", 0.001)
            if (!("b"  %in% dimnames(grw)$params)) grw=addPar(grw,"b", 3)

       return(lhSim.(grw,mmm(mat),mmm(sel),m,sr,FLQuant(age,dimnames=list(age=age)),...))})

gislaSim.=function(grw,mat,sel,m,sr,age,...){
   ## Biological processes
   wts       =vonB(grw,age)
   if (is.null(m)){
     L<-wts
     L[]<-wt2len(grw[c("a","b")],wts[dim(wts)[1]])
     m       =M(wt2len(grw[c("a","b")],wts),L)}

   if (is.na(mat["a50"]))
     mat["a50"]=mat50(apply(m,2:6,mean),grw["k"])
print(wts)
   mat.        =logistic(FLPar(a50=mat["a50"],ato95=(mat["a50"]+mat["ato95"]),asym=1.0),age)

   sel=FLPar(a1=(mat["a50"]+mat["ato95"])*sel["a"],
             sl=(mat["a50"]+mat["ato95"])*sel["sl"],
	     sr=(mat["a50"]+mat["ato95"])*sel["sr"])

   selPattern =doubleNormal(sel,age)
   
   dms=dimnames(m)
   ## create a FLBRP object to	 calculate expected equilibrium values and ref pts
   res=FLBRP(stock.wt       =wts,
             landings.wt    =wts,
             discards.wt    =wts,
             bycatch.wt     =wts,
             m              =m,
             mat            =mat.,
             landings.sel   =FLQuant(selPattern,dimnames=dms),
             discards.sel   =FLQuant(0,         dimnames=dms),
             bycatch.harvest=FLQuant(0,         dimnames=dms),
             harvest.spwn   =FLQuant(0,         dimnames=dms),
             m.spwn         =FLQuant(0,         dimnames=dms),
             availability   =FLQuant(1,         dimnames=dms))

   ## i.e. FApex
   range(res,c("minfbar","maxfbar"))[]<-as.integer(sel["a1"])

   ## replace any slot passed in as an arg
   args<-list(...)
   for (slt in names(args)[names(args) %in% names(getSlots("FLBRP"))[names(getSlots("FLBRP"))!="fbar"]])
     slot(res, slt)<-args[[slt]]

   ## Stock recruitment relationship
   model(res) =do.call(sr$model,list())$model
   params(res)=FLPar(abPars(sr$model,spr0=spr0(res),s=sr$steepness,v=sr$vbiomass))

   if ("fbar" %in% names(args)) fbar(res)<-args[["fbar"]]
   refpts(res)<-refpts(res)[c(1,4)]

   return(brp(res))}
################################################################################
