## fwd(FLStock)
if (!isGeneric("fwd"))
  setGeneric("fwd", function(object, fleets, ...)
	  standardGeneric("fwd"))

####  fwdect stock for catch
setMethod('fwd', signature(object="FLQuant", fleets = "missing"),
   function(object,model="pellat",catch=NULL,harvest=NULL,r=NULL,K=NULL,msy=NULL,m=0.5,p=2,b0=1){

      return(FLQuant(fwdArray(object,model=model,catch=catch,harvest=harvest,r=r,K=K,msy=msy,m=m,p=p,b0=b0)))
      })

setMethod('fwd', signature(object='FLBioDym', fleets = "missing"),
   function(object,catch=NULL,harvest=NULL,model=NULL,par=NULL){

    if (is(par,"FLPar")){
       nms<-dimnames(par)$params[dimnames(par)$params %in% dimnames(params(object))$params]
       params(object)[nms,]<-par[nms,]}

   if (!is.null(model))
      model(object)<-model

    stock(object)<-FLQuant(fwdArray(stock(object)@.Data,model=model(object),catch=catch(object)@.Data,harvest=harvest@.Data,
                                            r  =getPar(params(object),"r"  ),
                                            K  =getPar(params(object),"K"  ),
                                            msy=getPar(params(object),"msy"),
                                            p  =getPar(params(object),"p"  ),
                                            m  =getPar(params(object),"m"  ),
                                            b0 =getPar(params(object),"b0" )))

    if (!is.null(catch))
       catch(object)[,dimnames(catch)$year]<-catch
    else if (!is.null(harvest))
       catch(object)[,dimnames(harvest)$year]<-stock(object)[,dimnames(harvest)$year]*stock(object)[,dimnames(harvest)$year]

    return(object)
    })

fwdArray<-function(object,catch=NULL,harvest=NULL,model="pellat",r=NULL,K=NULL,p=NULL,msy=NULL,m=NULL,b0=NULL){
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
            stock[,ac(y+1),,,,]<-stock[,ac(y),,,,]-catch[,ac(y),,,,] + sp(model,stock[,ac(y),,,,],r=r,K=K,p=p,msy=msy,m=m)}
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

#### catchHat
setMethod('computeCatch', signature(object='FLBioDym'),
  function(object,stock=NULL){
   if (is.null(stock)) stock<-object@stock

   yrs<-dimnames(stock)$year
   
   res<-stock[,yrs[-max(length(yrs))],,,,]-stock[,yrs[-1],,,,]+sp(object@model,stock[,yrs[-max(length(yrs))],,,,],
                      r  =getPar(params(object),"r"),
                      K  =getPar(params(object),"K"),
                      m  =getPar(params(object),"m"),
                      p  =getPar(params(object),"p"),
                      msy=getPar(params(object),"msy"))

   return(res)
   })
