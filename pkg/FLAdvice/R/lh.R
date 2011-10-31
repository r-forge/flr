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
   standardGeneric('gislaSim'))
setMethod('gislaSim', signature(grw="FLPar"),
   function(grw,
            m  =function(L,linf,k) exp(0.55 - 1.61*log(L) + 1.44*log(linf) + log(k)),
            mat=function(linf,ato50=3) FLPar(a50=0.8776*linf-0.038-ato50,ato95=ato50),
            sel=FLPar(a=1,sl=1,sr=1e6),
            sr =list(model="bevholt",steepness=0.9,vbiomass=1e3),
            age=1:40,ageOffset=0.5,...){

            if (!("k"  %in% dimnames(grw)$params)) grw=addPar(grw,"k",0.5)
            if (!("t0" %in% dimnames(grw)$params)) grw=addPar(grw,"t0",0)
            if (!("a"  %in% dimnames(grw)$params)) grw=addPar(grw,"a", 0.001)
            if (!("b"  %in% dimnames(grw)$params)) grw=addPar(grw,"b", 3)
           
       return(gislaSim.(grw,mmm(mat),mmm(sel),m,sr,FLQuant(age+ageOffset,dimnames=list(age=age)),...))})

gislaSim.=function(grw,mat,sel,m,sr,age,...){
   ## Biological processes
   grw.=addPar(grw[!(dimnames(grw)$params=="linf")],"sinf",grw["a"]*grw["linf"]^grw["b"])
   wts =vonB(grw.,age)

   ## m
   if (is.function(m)){
      l=wt2len(grw[c("a","b")],wts)
      m=m(l,grw["linf"],grw["k"])}
   
   if (is.function(mat)){
      mat=addPar(mat(grw["linf"]),"asym",1)
      mat["a50"]=invVonB(grw,c(mat["a50"]))
      mat.=logistic(mat,age)}

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
