rndWlkFn<-function(par,obs,myFn  =function(x,par) par["Asym"]*exp(-par["b2"]*par["b3"]^x),
                           rndWlk=c("Asym"=0.0,"b2"=0.0,"b3"=0.0),
                           flag  =FALSE){

   #### Convert par used by optim to an FLPar
   dmns   <-list(params=names(rndWlk),year=dimnames(obs)[[2]],iter=1)
   params <-FLPar(array(NA,unlist(lapply(dmns,length)),dmns))

   ## fill up FLPar object
   for (i in names(rndWlk))
     params[i]<-par[grep(i, names(par))]

   ## Fitted values
   hat  <-myFn(ages(obs),params)

   ## Liklihood calculations
   rsdl<-(log(obs/hat[dimnames(obs)$age]))

   res<-apply(loglAR1(rsdl),2,sum)

   for (i in names(rndWlk))
      if (rndWlk[i]>0) {
         rwLL<-loglAR1(log(FLQuant(params[i,-1]/params[i,-dim(params[i])[2]])))*rndWlk[i]
         res<-res+rwLL}

#plot(c(params["Asym",,]),type="l", ylim=c(mean(par.[1:62])*.9,mean(par.[1:62])*1.1))
#lines(par.[1:62],col="red")
   print(c(-res))

   ## used to return results in FLQuant & FLPar
   if (flag) return(list(hat=hat,params=params,ll=res))

   if (is.nan(res)) return(10e10)
   if (is.na( res)) return(10e10)

   return(-res)}
