parLst<-list(fox       =c("r","K"),
             schaefer  =c("r","K"),
             pellat    =c("r","K","p"),
             shepherd  =c("r","K","m"),
             gulland   =c("r","K"),
             fletcher  =c("K","msy","p"))


setParams<-function(model="pellat",its=1){
   return(FLPar(NA,dimnames=list(params=c(parLst[[model]],"b0","q","sigma"),iter=its)))
   }

defaultPar<-function(object){
   params(object)<-FLPar(NA,dimnames=list(params=c(parLst[[model(object)]],"b0","q","sigma"),iter=1:dims(object)$iter))

   unt<-NULL
   if ("r"     %in% dimnames(params(object))$params){
      params(object)["r",    ]<-0.5
      unt<-c(unt,"")}
   if ("K"     %in% dimnames(params(object))$params){
      params(object)["K",    ]<-mean(catch(object))*10
      unt<-c(unt,units(catch(object)))}
   if ("p"     %in% dimnames(params(object))$params){
      params(object)["p",    ]<-2
      unt<-c(unt,"")}
   if ("msy"   %in% dimnames(params(object))$params){
       params(object)["msy",  ]<-mean(catch(object))
       unt<-c(unt,units(catch(object)))}
   if ("b0"    %in% dimnames(params(object))$params){
       params(object)["b0",   ]<-1
       unt<-c(unt,"")}
   if ("m"     %in% dimnames(params(object))$params){
       params(object)["m",    ]<-0.5
       unt<-c(unt,"")}
   if ("q"     %in% dimnames(params(object))$params){
       params(object)["q",    ]<-mean(index(object))/(mean(catch(object))*10)
       unt<-c(unt,"")}
   if ("sigma" %in% dimnames(params(object))$params){
      params(object)["sigma",]<-0.3
      unt<-c(unt,"")}

   units(params(object))<-unt

   invisible(params(object))
   }

getPar<-function(params,nm){
   if (nm %in% dimnames(params)$params)
      return(c(params[nm,]))
   else
      return(rep(as.numeric(NA),length=dims(params)$iter))
   }
   
