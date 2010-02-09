# Add some method that allows you to pass a seperate params
setGeneric("lprof", function(object, ...){
 standardGeneric("lprof")})

setMethod('lprof',
  signature(object='FLSR'),
    function(object, params, plot=TRUE, solution=TRUE, scaling=c("rel","rel"),parscale=NULL,CI=c(.5,.75,.9,.95),xlab=NULL,ylab=NULL,...){

      ## function to get parameter coordinate system
      getScale<-function(x,pars){
          if (is.null(names(x))){
            if (length(x)==1 & length(pars)==2) x<-rep(x,2)
            names(x)<-pars}

          if (!is.character(x) | !(all(substr(x,1,1) %in% c("a","r")))) stop("values in scale have to be rel or abs")

          return(x)
          }

    ## Get parameters to profile
    args <- list(...)
    pars <-names(params)[names(params) %in% dimnames(params(object))$params]

    ## 1D profile
    if (length(pars) == 1){
      ## create a grid centred on best guess
      scaling<-getScale(scaling,pars)

      ## create grid
      rng1<-args[[pars[1]]]

      ## specify rnages as relative proportions
      if (substr(scaling[1],1,1)=="r") rng1<-rng1*params(object)[pars[1],1]

      paramGrid<-expand.grid(rng1,ll=NA)
      names(paramGrid)[1]<-pars
      
      #if ("start" %in% names(formals(lprof))) .start<-start else
                                              .start<-as.list(params(object)[,drop=T])
                                              
      ## profile and plot liklihood around best guess
      for (i in 1:length(paramGrid[,1])){
        fixed=(list(paramGrid[i,pars[1]]))
        names(fixed)<-pars
        if ("fixed" %in% names(args)) fixed=c(fixed,args[names(args)=="fixed"][[1]])

        psPar<-dimnames(params(object))$params[dimnames(params(object))$params!=pars]
        psPar<-psPar[!(psPar %in% names(fixed))]
        paramGrid[i,"ll"]<-logLik(fmle(object,start=.start,fixed=fixed,control=list(parscale=autoParscale(object)[psPar])))}

      ## plot liklihood
      if (plot){
        if (is.null(xlab)) xlab=pars[1]
        plot(  paramGrid[,"ll"]~paramGrid[,1],xlab=xlab,ylab="Likelihood",type="l")

        if (solution)
           points(logLik(object)~params(object)[pars[1],1],pch=19)}

    ## 2D profile
    } else if (length(pars) == 2){

      scaling<-getScale(scaling,pars)

      #if ("start" %in% names(formals(lprof))) .start<-start else
                                              .start<-as.list(params(object)[,drop=T])
      ## create grid
      rng1<-params[[pars[1]]]
      rng2<-params[[pars[2]]]

      ## specify rnages as relative proportions
      if (substr(scaling[1],1,1)=="r") rng1<-rng1*params(object)[pars[1],1]
      if (substr(scaling[2],1,1)=="r") rng2<-rng2*params(object)[pars[2],1]

      ## use automatic parscaling to set parameter scalings so that profile is similar for x- & y-axes
      if (!is.null(parscale[1])) if (parscale[1] %in% pars){
         scl<-params(object)[pars,1,drop=T]/autoParscale(object)[pars]
         scl<-scl[parscale[1]]/scl[!(names(scl) %in% parscale[1])]
         scl<-1/scl
         
         if (parscale[1] == pars[1])
            rng1<-params(object)[pars[1],1,drop=T]-scl*(params(object)[pars[1],1,drop=T]-rng1) else
         if (parscale[1] == pars[2])
            rng2<-params(object)[pars[2],1,drop=T]-scl*(params(object)[pars[2],1,drop=T]-rng2)
         }
         
      ## set grid for profile as data.frame
      paramGrid<-expand.grid(rng1,rng2,ll=NA)
      names(paramGrid)[1:2]<-pars

      ## profile liklihood
      for (i in 1:length(paramGrid[,1])){
        fixed=(list(paramGrid[i,pars[1]],paramGrid[i,pars[2]]))

        names(fixed)<-pars

        if ("fixed" %in% names(args)) fixed=c(fixed,args[names(args)=="fixed"][[1]])

        if (all(dimnames(params(object))$params %in% names(fixed))){
           #paramGrid[i,"ll"]<-logl(object)(fixed[[1]],fixed[[2]],rec(object),ssb(object))
           arg<-fixed
           arg[["ssb"]]<-ssb(object)
           arg[["rec"]]<-rec(object)
           logl.<-logl(object)
           paramGrid[i,"ll"]<-do.call("logl.",arg)
        }else{
           psPar<-dimnames(params(object))$params[!(dimnames(params(object))$params %in% pars)]
           psPar<-psPar[!(psPar %in% names(fixed))]

           paramGrid[i,"ll"]<-logLik(fmle(object,fixed=fixed,start=.start,control=list(  parscale=as.list(autoParscale(object)[psPar]))))}
      }

      ## plot liklihood"
      if (plot){
         plotGrid<-list(x=sort(unique(paramGrid[,1])), y=sort(unique(paramGrid[,2])), z=tapply(paramGrid[,"ll"], list(paramGrid[,1],paramGrid[,2]),mean))
         .CI<-logLik(object)-qchisq(CI,2)
         if (is.null(xlab)) xlab=pars[1]
         if (is.null(ylab)) ylab=pars[2]

         image(  plotGrid,ylab=ylab, xlab=xlab) #, breaks=CI,col=rainbow(length(CI)-1))
         contour(plotGrid,  levels=.CI,add=T, col="grey",  lwd=2, labels=CI)

        #image(  plotGrid,xlab=pars[1],ylab=pars[2])
        #contour(plotGrid,add=T)

        if (solution)
          points(params(object)[pars[1],1,drop=T],params(object)[pars[2],1,drop=T],pch=19)}

    }else if (length(pars) > 2 | length(pars) == 0) stop("need to specify 1 or 2 valid parameters")


    ## explicit return
    invisible(paramGrid)
    })
  
#

    