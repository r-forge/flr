### provides the back drop on which to overlay data
kobeFn=function(object,xlim,ylim){    
    quads<- rbind(data.frame(x=c(-Inf,-Inf,Inf,Inf), y=c(-Inf,Inf,Inf,-Inf), fill=as.factor("yellow")),
                     data.frame(x=c(   1,   1,Inf,Inf), y=c(-Inf,  1,  1,-Inf), fill=as.factor("green")),
                     data.frame(x=c(-Inf,-Inf,  1,  1), y=c(   1,Inf,Inf,   1), fill=as.factor("red")))

       p=ggplot(object)+geom_polygon(data=quads,aes(x,y,fill=fill)) +
                        scale_fill_manual(values = c("yellow","green","red"), legend=FALSE) +
                        ylab(expression(F/F[MSY]))        +
                        xlab(expression(SSB/B[MSY]))      +
                        scale_y_continuous(limits=ylim)   +
                        scale_x_continuous(limits=xlim)
    
      invisible(p)}
    
 
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

setMethod('kobeP', signature(x="numeric",y="numeric"),
 function(x,y) {
            
            b =  pmax(pmin(as.integer(x),1),0)
            f =1-pmax(pmin(as.integer(y),1),0)
            p =f*b
            collapsed=(1-b)*(1-f)
            
            red   =collapsed
            green =p
            yellow=1-p-collapsed
            
            overFished =1-b
            overFishing=1-f  

            data.frame(red=red,green=green,yellow=yellow,overFished=overFished,overFishing=overFishing)})

setMethod('kobeP', signature(x="FLStock",y="FLBRP"),
 function(x,y,rp="msy"){
   res=model.frame(SSB    =ssb(    x),refpts(y)[rp,"ssb"],
                   harvest=harvest(x),refpts(y)[rp,"harvest"],drop=TRUE)
   
   res=cbind(res,kobeP(res[,"SSB"],res[,"harvest"]))
   
   return(res)})


setMethod('kobeShade', signature(object='numeric'),
          function(object,breaks=c(-0.1,50,60,70,80,90,100),
                     shades=c("\\{","\\grey50{","\\gey60{","\\grey70{","\\grey80{","\\grey90{"),
                     percent=100,...){
    
  #Kobe II strategy matrices to be prepared by the SCRS should highlight in a similar format as
  #shown in Annex Table 2 a progression of probabilities over 50 % and in the range of 50-59 %, 60-
  #69 %, 70-79 %, 80-89 % and ≥ 90 %.
  
  object=object*100          
  res=cut(object,breaks)
  gry=data.frame(level=attributes(unique(res))$levels,shades)
  res=merge(data.frame(object=as.integer(object),level=res),gry,all.x=TRUE)
  
  res=with(res,paste(shades,object,"}",sep=""))
  
  return(res)})

setMethod('kobeShade', signature(object='data.frame'),
          function(object,breaks =c(-0.1,50,60,70,80,90,100),
                     shades =c("\\{","\\grey50{","\\gey60{","\\grey70{","\\grey80{","\\grey90{"),
                     percent=100,...){

     apply(object,2,kobeShade,breaks=breaks,shades=shades,percent=percent)})
setMethod('kobeShade', signature(object='matrix'),
          function(object,breaks =c(-0.1,50,60,70,80,90,100),
                     shades =c("\\{","\\grey50{","\\gey60{","\\grey70{","\\grey80{","\\grey90{"),
                     percent=100,...){

     apply(object,2,kobeShade,breaks=breaks,shades=shades,percent=percent)})
          