#### Kobe Advice plots #########################################################

setGeneric('kobe', function(b,f, ...)
		standardGeneric('kobe'))
    
setMethod("kobe", signature(b="FLQuants", f="FLQuants"),
  function(b,f,ssbPts=NULL,fbrPts=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE,type="tapas",...){

    if (type=="tapas")
       plotTapas2(x=b,y=f,ssbPts,fbrPts,title,biCol,xlab,ylab,maxX,maxY,axs)
    else if (type=="rasta")
       plotRasta2(x=b,y=f)
    })

setMethod("kobe", signature(b="FLQuants", f="FLQuant"),
  function(b,f,ssbPts=NULL,fbrPts=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE,type="tapas",...){
    b<-FLQuants(b)

    kobe(x=b,y=f,ssbPts=NULL,fbrPts=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE,type="tapas",...)
    })

setMethod("kobe", signature(b="FLQuant", f="FLQuants"),
    function(b,f,ssbPts=NULL,fbrPts=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE){
    f<-FLQuants(f)

    plotTapas2(x=b,y=f,ssbPts=ssbPts,fbrPts=fbrPts,title=title,biCol=biCol,xlab=xlab,ylab=ylab,maxX=maxX,maxY=maxY,axs=axs)
    })

setMethod("kobe", signature(b="FLQuant", f="FLQuant"),
    function(b,f,ssbPts=NULL,fbrPts=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab=expression(F:F[MSY]),maxX=NULL,maxY=NULL,axs=TRUE,type="tapas"){

    if (type=="tapas")
       plotTapas2(x=b,y=f,ssbPts=ssbPts,fbrPts=fbrPts,title=title,biCol=biCol,xlab=xlab,ylab=ylab,maxX=maxX,maxY=maxY,axs=axs)
    else if (type=="rasta")
       plotRasta2(b,f,title)
    })

setMethod("kobe", signature(b="FLQuants", f="missing"),
  function(b,f,ssbPts=NULL,fbrPts=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab="Option",bmsy=NULL,fmsy=NULL,xo=500,yo=xo,maxX=NULL,maxY=NULL,axs=TRUE,type="tapas",...){
       if (type=="matrix")
        kobeMatrix(b=b,f=b,bmsy=bmsy,fmsy=fmsy,ylab=ylab,p=T,xo=500,yo=xo)
       else
        FLBRP::kobe(b=b,f=f,type=type,yr=yr,title=title,biCol=biCol,xlab=xlab,ylab=ylab,maxX=maxX,maxY=maxY,axs=axs)
     })

setMethod("kobe", signature(b="array", f="missing"),
  function(b,f,ssbPts=NULL,fbrPts=NULL,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab="Option",bmsy=NULL,fmsy=NULL,xo=500,yo=xo,maxX=NULL,maxY=NULL,axs=TRUE,type="tapas",...){
     kobePlot(res=b,ylab=ylab,x0=xo,yo=yo)
     })

setMethod("kobe", signature(b="data.frame", f="missing"),
  function(b,f,title="",biCol=c("white","blue"),xlab=expression(SSB:B[MSY]),ylab="Option",bmsy=NULL,fmsy=NULL,x0=500,yo=xo,maxX=NULL,maxY=NULL,axs=TRUE,type="tapas",...){
        kobeMatrixDF(b=b,f=f,bmsy=bmsy,fmsy=fmsy,ylab=ylab,p=T,xo=500,yo=xo)
     })

kobePlot<-function(res,ylab="Option",xo=500,yo=xo){
   t.<-defactor(cbind(expand.grid(dimnames(res)[c("option","year")]),val=c(res)))
   t.<-interp(t.[,2],t.[,1],t.[,3], yo=seq(min(t.[,1]), max(t.[,1]), length=xo),
                                        xo=seq(min(t.[,2]), max(t.[,2]), length=yo))
   image(t.,breaks=c(0,.5,.75,1), col=c("red","yellow","green"),ylab=ylab, xlab="Year")
   contour(t.,levels=c(.5,.75), add=T, col="grey",  lwd=2)
   contour(t.,levels=c(.90,.60),add=T, col="grey2", lwd=2)
   }

kobeMatrix<-function(b=NULL,f=NULL,bmsy=NULL,fmsy=NULL,ylab="Option",p=T,xo=500,yo=xo){

    if (!is.null(b) & !is.null(bmsy))
        b<-lapply(b, function(x) sweep(x,6,bmsy,"/"))
    if (!is.null(f) & !is.null(fmsy))
        f<-lapply(f, function(x) sweep(x,6,fmsy,"/"))

    if (!is.null(b)){
      dmns<-list(year=dimnames(b[[1]])$year,iter=dimnames(b[[1]])$iter,option=names(b))
      resB<-aperm(array(unlist(lapply(b, function(x) qmin(floor(x),1)[1,drop=T])),lapply(dmns,length),dimnames=dmns),c(3,1,2))}

    if (!is.null(f)){
      dmns<-list(year=dimnames(f[[1]])$year,iter=dimnames(f[[1]])$iter,option=names(b))
      resF<-aperm(array(unlist(lapply(f, function(x) 1-qmin(floor(x),1)[1,drop=T])),lapply(dmns,length),dimnames=dmns),c(3,1,2))}

    res<-aperm(array(1,lapply(dmns,length),dimnames=dmns),c(3,1,2))
    if (!is.null(b))
       res<-res*resB
    if (!is.null(f))
       res<-res*resF

   res<-apply(res,1:2,mean)
   
   if (p) kobePlot(res,ylab=ylab)
   invisible(res)
   }

kobeMatrixDF<-function(df=NULL,rfpts=NULL,ylab="Option",p=T,xo=500,yo=xo){

    if (!is.null(refpts)){
       if ("bmsy" %in% names(refpts) & "ssb" %in% names(df)){
          df<-merge(df,rfpts)
          df$ssb<-df$ssb/df$bmsy}
       if ("fmsy" %in% names(refpts) & "fbar" %in% names(df)){
          df<-merge(df,rfpts)
          df$fbar<-df$fbar/df$fmsy}}

    if ("ssb"  %in% names(df))
       resB<-tapply(df$ssb, df[,c("year","qname","iter")],mean)
    if ("fbar" %in% names(df))
       resF<-tapply(df$fbar,df[,c("year","qname","iter")],mean)

    if      (!is.null(b) & is.null(f))
       res<-resB
    else if ( is.null(b) & !is.null(f))
       res<-resF
    else if (!is.null(b) & !is.null(f))
       res<-resF*resB

   res<-apply(res,1:2,mean)

   if (p) kobePlot(res,ylab=ylab)
   invisible(res)
   }
################################################################################

