setGeneric('fwd', function(object,...)
		standardGeneric('fwd'))

####  fwdect stock for catch
setMethod('fwd', signature(object="FLQuant"),
   function(object,model="pellat",catch=NULL,harvest=NULL,par=list(r=NULL,K=NULL,msy=NULL,m=0.5,p=2,b0=1)){

      return(FLQuant(fwdArray(object,model=model,catch=catch,harvest=harvest,par)))
      })

fwdArray<-function(object,model="pellat",catch=NULL,harvest=NULL,r=NULL,K=NULL,msy=NULL,m=NULL,p=2,b0=1){
      if(is.null(catch) & is.null(harvest)) stop("need to specify catch or harvest as a target")

      setStock<-function(object,x,K,b0){
         dms<-dimnames(object)
         dms$year<-sort(as.integer(unique(c(dimnames(object)$year,dimnames(x)$year,max(as.integer(dimnames(x)$year))+1))))
         stock<-array(K*b0,unlist(lapply(dms,length)),dms)
         stock[,dimnames(object)$year,,,,]<-object

         return(stock)
         }

      if(!is.null(catch)) {
         if (class(object)!=class(catch)) stop("stock and catch have to be of same class")
         yrs  <-c(as.integer(dimnames(catch)$year),max(as.integer(dimnames(catch)$year))+1)
         stock<-setStock(object,catch,K,b0)
         for(y in yrs[-length(yrs)]) {
            stock[,ac(y+1),,,,]<-stock[,ac(y),,,,]-catch[,ac(y),,,,] + sp(model,stock[,ac(y),,,,],r=r,K=K,m=m,p=p,msy=msy)}
         }
      else {
         if (class(object)!=class(harvest)) stop("object and harvest have to be of same class")
         yrs  <-c(as.integer(dimnames(harvest)$year),max(as.integer(dimnames(harvest)$year))+1)
         stock<-setStock(object,harvest,K,b0)

         for(y in yrs[-length(yrs)]) {
            stock[,ac(y+1),,,,]<-stock[,ac(y),,,,]-stock[,ac(y),,,,]*harvest[,ac(y),,,,] + sp(model,stock[,ac(y),,,,],r=r,K=K,m=m,p=p,msy=msy)}
         }

      stock[stock < 0] <- 1e-8

      return(stock)
      }

setMethod('fwd', signature(object='FLBioDym'),
   function(object,catch=NULL,harvest=NULL,par=list(r=NULL,K=NULL,msy=NULL,m=0.5,p=2,b0=1)){

      params(object)["b0",] <-1

      if (is.numeric(par)) par<-as.list(par)
      if ("r"   %in% names(par)  && "r"   %in% dimnames(params(object))$params) params(object)["r",]  <-par$r
      if ("K"   %in% names(par)  && "K"   %in% dimnames(params(object))$params) params(object)["K",]  <-par$K
      if ("msy" %in% names(par)  && "msy" %in% dimnames(params(object))$params) params(object)["msy",]<-par$msy
      if ("m"   %in% names(par)  && "m"   %in% dimnames(params(object))$params) params(object)["m",]  <-par$m
      if ("p"   %in% names(par)  && "p"   %in% dimnames(params(object))$params) params(object)["p",]  <-par$p
      if ("b0"  %in% names(par)  && "b0"  %in% dimnames(params(object))$params) params(object)["b0",] <-par$b0

      if ("r"   %in% dimnames(object@params)$params) r  =params(object)["r",]   else r  =NULL
      if ("K"   %in% dimnames(object@params)$params) K  =params(object)["K",]   else K  =NULL
      if ("m"   %in% dimnames(object@params)$params) m  =params(object)["m",]   else m  =0.5
      if ("p"   %in% dimnames(object@params)$params) p  =params(object)["p",]   else p  =2
      if ("may" %in% dimnames(object@params)$params) msy=params(object)["msy",] else msy=NULL
      if ("b0"  %in% dimnames(object@params)$params) b0 =params(object)["b0",]  else b0 =1

    stock(object)<-FLQuant(fwdArray(stock(object)@.Data,model=model(object),catch=catch@.Data,harvest=harvest,r=r,K=K,m=m,p=p,msy=msy,b0=b0))
    if (!is.null(catch))
       catch(object)[,dimnames(catch)$year]<-catch
    else if (!is.null(harvest))
       catch(object)[,dimnames(harvest)$year]<-stock(object)[,dimnames(harvest)$year]*stock(object)[,dimnames(harvest)$year]

    return(object)
    })

#### catchHat
setMethod('computeCatch', signature(object='FLBioDym'),
catchHat<-function(object){

   yrs<-dimnames(object@stock)$year

   res<-object@stock[,yrs[-max(length(yrs))]]-stock[,yrs[-1]]+sp(model,stock[,yrs[-1]],r=r,K=K,m=m,p=p,msy=msy)

   return(res)
   })
