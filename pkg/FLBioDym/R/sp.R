#### Production functions ######################################################
setGeneric('sp', function(object,...)
		standardGeneric('sp'))
		
setMethod('sp', signature(object='character'),
  function(object,bio,r=.5,K=10,m=0.25,p=2,msy=0){

    fox<-function(bio,r,K){
        r*bio*(1-log(bio)/log(K))}

    schaefer<-function(bio,r,K){
        r*bio*(1-bio/K)}

    pellat<-function(bio,r,K,p=2){
        bio*r-(bio)^p*r/K}

    shepherd<-function(bio,r=r,K,m){
        r*bio/(1+bio/K)-m*bio}

    gulland<-function(bio,r,K){
        r*bio*(K-bio)}

    fletcher<-function(bio,K,msy,p){
        lambda<-(p^(p/(p-1)))/(p-1)

        lambda*msy*(bio/K)-lambda*msy*(bio/K)^p
        }

    res<-switch(object,
           fox     =fox(     bio,r,K),
           schaefer=schaefer(bio,r,K),
           gulland =gulland( bio,r,K),
           fletcher=fletcher(bio,  K,msy,p),
           pellat  =pellat(  bio,r,K,p),
           shepherd=shepherd(bio,r,K,m))

    return(res)
    })
    
setMethod('sp', signature(object='FLBioDym'),
  function(object,stock=NULL){
  
   if (is.null(stock)) stock<-stock(object)

   nms <-dimnames(params(object))$params
   
   if ("r"   %in% nms) r  =getPar(params(object),"r")   else r=  NULL
   if ("K"   %in% nms) K  =getPar(params(object),"K")   else K=  NULL
   if ("m"   %in% nms) m  =getPar(params(object),"m")   else m=  NULL
   if ("p"   %in% nms) p  =getPar(params(object),"p")   else p=  NULL
   if ("msy" %in% nms) msy=getPar(params(object),"msy") else msy=NULL

   return(sp(model(object),stock,r=r,K=K,m=m,p=p,msy=msy))
   })
  
  
