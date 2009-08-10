setGeneric('fwd', function(object,...)
		standardGeneric('fwd'))

####  fwdect stock for catch
setMethod('fwd', signature(object="array"),
   function(object,model="pellat",catch=NULL,harvest=NULL,r=NULL,K=NULL,MSY=NULL,m=NULL,p=2,b0=1){
      if(is.null(catch) & is.null(harvest)) stop("need to specify catch or harvest as a target")
      setStock<-function(object,x){
         dms<-dimnames(object)
         dms$year<-sort(as.integer(unique(c(dimnames(object)$year,dimnames(x)$year,max(as.integer(dimnames(x)$year))+1))))
         stock<-array(NA,unlist(lapply(dms,length)),dms)
         stock[,dimnames(object)$year,,,,]<-object

         return(stock)
         }

      if(!is.null(catch)) {
         if (class(object)!=class(catch)) stop("stock and catch have to be of same class")
         yrs  <-c(as.integer(dimnames(catch)$year),max(as.integer(dimnames(catch)$year))+1)
         stock<-setStock(object,catch)
         for(y in yrs[-length(yrs)]) {
            stock[,ac(y+1),,,,]<-stock[,ac(y),,,,]-catch[,ac(y),,,,] + prodFunc(model,stock[,ac(y),,,,],r=r,K=K,m=m,p=p,MSY=MSY)}
         }
      else {
         if (class(object)!=class(harvest)) stop("object and harvest have to be of same class")
         yrs  <-c(as.integer(dimnames(harvest)$year),max(as.integer(dimnames(harvest)$year))+1)
         stock<-setStock(object,catch)

         for(y in yrs[-length(yrs)]) {
            stock[,ac(y+1),,,,]<-stock[,ac(y),,,,]-stock[,ac(y),,,,]*harvest[,ac(y),,,,] + prodFunc(model,stock[,ac(y),,,,],r=r,K=K,m=m,p=p,MSY=MSY)}
         }
         
      stock[stock < 0] <- 1e-8

      return(stock)
      })

setMethod('fwd', signature(object='FLBioDym'),
   function(object,catch=NULL,harvest=NULL){

    if ("r"    %in% dimnames(object@params)$params) r  =params(object)["r",]   else r  =NULL
    if ("K"    %in% dimnames(object@params)$params) K  =params(object)["K",]   else K  =NULL
    if ("m"    %in% dimnames(object@params)$params) m  =params(object)["m",]   else m  =NULL
    if ("p"    %in% dimnames(object@params)$params) p  =params(object)["p",]   else p  =NULL
    if ("rmay" %in% dimnames(object@params)$params) msy=params(object)["msy",] else msy=NULL

    stock(object)<-FLQuant(fwd(stock(object)@.Data,model=model(object),catch=catch@.Data,harvest=harvest,r=r,K=K,MSY=MSY,m=m,p=p))

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

   res<-object@stock[,yrs[-max(length(yrs))]]-stock[,yrs[-1]]+prodFunc(model,stock[,yrs[-1]],r=r,K=K,m=m,p=p,MSY=MSY)

   return(res)
   })
