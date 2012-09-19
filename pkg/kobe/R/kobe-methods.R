### provid
setMethod('kobe', signature(object='missing'),
  function(object,xlim=c(0,2),ylim=xlim){
    
       invisible(kobeFn(NULL,xlim,ylim))})

setMethod('kobe', signature(object='data.frame'),
  function(object,xlim=c(0,2),ylim=xlim){
    
       invisible(kobeFn(object,xlim,ylim))})

setMethod('kobe', signature(object='FLBRP'),
  function(object,xlim=c(0,2),ylim=xlim){
    
       object=model.frame(FLQuants(ssb    =sweep( ssb.obs(object),6,refpts(object)["msy","ssb"],    "/"),
                                   harvest=sweep(fbar.obs(object),6,refpts(object)["msy","harvest"],"/")))

       invisible(kobeFn(object,xlim,ylim))})

setMethod('kobe', signature(object='FLlst'),
  function(object,xlim=c(0,2),ylim=xlim){
       
       ldply(object, function(object)
                              model.frame(FLQuants(ssb    =sweep( ssb.obs(object),6,refpts(object)["msy","ssb"],    "/"),
                                                   harvest=sweep(fbar.obs(object),6,refpts(object)["msy","harvest"],"/"))))

       invisible(kobeFn(object,xlim,ylim))})

setMethod('kobeP', signature(b="numeric",f="numeric"),
 function(b,f) {
            
            b =  pmax(pmin(as.integer(b),1),0)
            f =1-pmax(pmin(as.integer(f
),1),0)
            p =f*b
            collapsed=(1-b)*(1-f)
            
            red   =collapsed
            green =p
            yellow=1-p-collapsed
            
            overFished =1-b
            overFishing=1-f  

            data.frame(red=red,green=green,yellow=yellow,overFished=overFished,overFishing=overFishing)})

setMethod('kobeP', signature(b="FLStock",f="FLBRP"),
 function(b,f,rp="msy"){
   res=model.frame(SSB    =ssb(    b),refpts(f)[rp,"ssb"],
                   harvest=harvest(b),refpts(f)[rp,"harvest"],drop=TRUE)
   
   res=cbind(res,kobeP(res[,"SSB"],res[,"harvest"]))
   
   return(res)})


setMethod('kobeShade', signature(object='numeric'),
          function(object,breaks=c(-0.1,50,60,70,80,90,100),
                     shades=c("","\\cellcolor{gray90}","\\cellcolor{gray80}","\\cellcolor{gray70}","\\cellcolor{gray60}","\\cellcolor{gray50}"),...){
    
  #\\cellcolor{gray50}
 print(1)           
  #Kobe II strategy matrices to be prepared by the SCRS should highlight in a similar format as
  #shown in Annex Table 2 a progression of probabilities over 50 % and in the range of 50-59 %, 60-
  #69 %, 70-79 %, 80-89 % and ≥ 90 %.
  object=object*100          
  res=cut(object,breaks)
  gry=data.frame(level=attributes(unique(res))$levels,shades)
  res=merge(data.frame(object=as.integer(object),level=res),gry,all.x=TRUE)
  
  object.=paste(ac(round(object)),"\\%",sep="")
  
 res=with(res,paste(shades,object.,sep=" "))
  
  #write.table(xtable(kobeShade(k2smTab)),file="/tmp/tab.tex")
  return(res)})

setMethod('kobeShade', signature(object='data.frame'),
          function(object,breaks =c(-0.1,50,60,70,80,90,100),
                   shades=c("","\\cellcolor{gray90}","\\cellcolor{gray80}","\\cellcolor{gray70}","\\cellcolor{gray60}","\\cellcolor{gray50}"),...){

     apply(object,2,kobeShade,breaks=breaks,shades=shades)})
setMethod('kobeShade', signature(object='matrix'),
          function(object,breaks =c(-0.1,50,60,70,80,90,100),
                   shades=c("","\\cellcolor{gray90}","\\cellcolor{gray80}","\\cellcolor{gray70}","\\cellcolor{gray60}","\\cellcolor{gray50}"),...){

     apply(object,2,kobeShade,breaks=breaks,shades=shades)})

setMethod('k2sm', signature(object='data.frame'),
          function(object,cex   =1.2,
                         image  =list(levels=seq(0.0,1.0,0.05),
                                      col   =c(colorRampPalette(c("red4","red"))(12),colorRampPalette(c("yellowgreen","darkgreen"))(8))),
                         contour=list(levels=c(.6,.7,1.0,.9),
                                      col   =c("black"))){
                 
    nms   =dimnames(object)[[2]]
    nPlots=length(nms[nms %in% c("overFishing","overFished","green")])
                 
    ops<-par(mfrow=c(nPlots,1), mex=.5,mai=c( 0.5, 0.75 ,0.5, 0.1),cex=par()$cex)
    
    res=list()
    if ("overFishing" %in% nms){
       x=transform(object, NotOverFishing=as.numeric(!object[,"overFishing"]))[,c(nms[1:2],"NotOverFishing")]
       res["F"]    <-k2smFn(x, image=image,contour=contour)
       mtext(expression(plain(P) (F<=F[MSY])),line=3, cex=cex)
       }
    if ("overFished" %in% nms){
       x=transform(object, NotOverFished=as.numeric(!object[,"overFished"]))[,c(nms[1:2],"NotOverFished")]
       res["SSB"]  <-k2smFn(x, image=image,contour=contour)
       mtext(expression(plain(P) (SSB>=SSB[MSY])),line=3, cex=cex)
       }
    if ("green" %in% nms){
       res["Joint"]<-k2smFn(cbind(object[,c(1:2)],object[,"green"]), image=image,contour=contour)
       mtext(expression(plain(P) (F<=F[MSY]) %*% plain(P)(SSB>=SSB[MSY])),line=3, cex=cex)
       }

    par(mfrow=ops$mfrow,mex=ops$mex,mai=ops$mai,cex=ops$cex)

    invisible(res)})          
