# Add some method that allows you to pass a seperate params
setGeneric("lprof", function(object, ...){
 standardGeneric("lprof")})

setMethod('lprof',
  signature(object='FLSR'),
    function(object, .plot=TRUE, .soultion=TRUE, .scaling=c("rel","rel"),.parscale=NULL,...){

if (FALSE) {
            object<-cod4SR[["bevholtSV"]]
            plot=TRUE
            .soultion=TRUE
            .scaling=c("rel","rel")
            .parscale="s"
            s =seq(0.5,1.5,length.out=10)
            v =seq(0.5,1.5,length.out=10)
            args<-list(s=s,v=v,fixed=list(spr0=0.45))}


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
    pars <-names(args)[names(args) %in% dimnames(params(object))$params]

    ## 1D profile
    if (length(pars) == 1){
      ## create a grid centred on best guess
      rng1<-args[[pars[1]]]*params(object)[pars[1],1]

      paramGrid<-expand.grid(rng1,ll=NA)
      names(paramGrid)[1]<-pars

      ## profile and plot liklihood around best guess
      for (i in 1:length(paramGrid[,1])){
        fixed=(list(paramGrid[i,pars[1]]))
        names(fixed)<-pars

        psPar<-dimnames(params(object))$params[dimnames(params(object))$params!=pars]
        paramGrid[i,"ll"]<-logLik(fmle(object,fixed=fixed,control=list(.parscale=auto.parscale(object)[psPar])))}

      ## plot liklihood
      if (plot){
        plot(  paramGrid[,"ll"]~paramGrid[,1],xlab=pars[1],ylab="Likelihood",type="l")

        if (.soultion)
           points(logLik(object)~params(object)[pars[1],1],pch=19)}

    ## 2D profile
    } else if (length(pars) == 2){

      .scaling<-getScale(.scaling,pars)

      ## create grid
      rng1<-args[[pars[1]]]
      rng2<-args[[pars[2]]]

      ## specify rnages as relative proportions
      if (substr(.scaling[1],1,1)=="r") rng1<-rng1*params(object)[pars[1],1]
      if (substr(.scaling[2],1,1)=="r") rng2<-rng2*params(object)[pars[2],1]

      ## use automatic par.scaling to set parameter .scalings so that profile is similar for x- & y-axes
      if (!is.null(.parscale[1])) if (.parscale[1] %in% pars){
         scl<-params(object)[pars,1,drop=T]/auto.parscale(object)[pars]
         scl<-scl[.parscale[1]]/scl[!(names(scl) %in% .parscale[1])]
         scl<-1/scl
         
         if (.parscale[1] == pars[1])
            rng1<-params(object)[pars[1],1,drop=T]-scl*(params(object)[pars[1],1,drop=T]-rng1) else
         if (.parscale[1] == pars[2])
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
           paramGrid[i,"ll"]<-logLik(fmle(object,fixed=fixed,control=list(.parscale=as.list(autoParscale(object)[psPar]))))}
      }
      
      ## plot liklihood"
      if (plot){
        plotGrid<-list(x=sort(unique(paramGrid[,1])), y=sort(unique(paramGrid[,2])), z=tapply(paramGrid[,"ll"], list(paramGrid[,1],paramGrid[,2]),mean))
        image(  plotGrid,xlab=pars[1],ylab=pars[2])
        contour(plotGrid,add=T)

        if (.soultion)
          points(params(object)[pars[1],1,drop=T],params(object)[pars[2],1,drop=T],pch=19)}

    }else if (length(pars) > 2 | length(pars) == 0) stop("need to specify 1 or 2 valid parameters")


    ## explicit return
    invisible(paramGrid)
    })
  
#lprof(cod4SR[["ices bevholt"]],plot=TRUE, .soultion=TRUE, .scaling=c("rel","rel"),.parscale="b",a =seq(0.5,1.5,length.out=10),b =seq(0.5,1.5,length.out=10))
#lprof(cod4SR[["ices bevholt"]],plot=TRUE, .soultion=TRUE, .scaling=c("rel","rel"),.parscale=NULL,a =seq(0.5,1.5,length.out=10))

    