kobePfn=function(pts,trk=NULL,mns=FALSE,maxX=2,maxY=maxX,palette=colorRampPalette(c("orange","blue"),space="Lab"),
                 xlab="Stock relative to Benchmark",
                 ylab="F relative to Benchmark"){
     
    if (!("group" %in% names(pts)))
       pts=cbind(pts,group=1)
  
    cols=rgb.palette(length(unique(pts$group)))
   
    ##### Density plots   #############################################################################################
    # stock density plot
    dS<-ggplot(pts) + 
          geom_density(aes(x = stock, y =  ..count.., group=group), fill="#CCCCCCCC", col="#CCCCCCCC", position = "stack") + 
          geom_density(aes(x = stock, y = -..count.., fill =group, alpha=0.4)) + 
          geom_vline(xintercept=1,col="red")  +
          theme_ms(18,textColor=NA, axisColor=NA,angle.y=-90) +
          opts(legend.position = "none") + 
              scale_x_continuous(limits=c(0,maxX)) +
              scale_fill_manual(values=cols)+
              opts(axis.title.x=NULL,axis.text.x=NULL)
    
    # second density plot, oriented vertically (hence the 'coord_flip()' at the end
    dH<-ggplot(pts) + 
          geom_density(aes(x = harvest, y =  ..count.., group=group), fill="#CCCCCCCC", col="#CCCCCCCC", position = "stack") + 
          geom_density(aes(x = harvest, y = -..count..,               fill=group, alpha=0.4)) + 
          geom_vline(xintercept=1,col="red")  +
          theme_ms(18,textColor=NA, axisColor=NA,angle.y=-90) +
              opts(legend.position = "none") + 
              scale_x_continuous(limits=c(0,maxY)) +
              scale_fill_manual(values=cols)+
              opts(axis.title.y=NULL, axis.text.y=NULL) 
  
    # kobe phase plot
    kC=kobe() +
      geom_point(aes(stock,harvest,col=group,col=group),size=1., data=pts)    +
      scale_y_continuous(limits=c(0,maxY))+
      scale_x_continuous(limits=c(0,maxX))+
      theme_ms(18) +
      opts(legend.position = "none")   +
      xlab(xlab)+
      ylab(ylab)    +
      scale_colour_manual(values=cols)
   
    if (mns)
        kC=kC+geom_point(aes(stock,harvest,col=cols,col=group),size=6.0, colour="black",  data=ddply(pts,.(group),function(x) data.frame(stock=median(x$stock),harvest=median(x$harvest)))) +
              geom_point(aes(stock,harvest,col=cols,col=group),size=4.5, colour="cyan",   data=ddply(pts,.(group),function(x) data.frame(stock=median(x$stock),harvest=median(x$harvest))))
   if (!is.null(trk))
        kC=kC+geom_path(aes(stock,harvest, col=cols,col=group),size=1., data=trk)   
      
    fnVP=function(dH,dS,kC){
        vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
          
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(5, 5)))  # 5 by 5 grid
        print(dS, vp=vplayout(1,1:4))                       # the first density plot will occupy the top of the grid
        print(dH +coord_flip(), vp=vplayout(2:5,5))         # 2nd to the left +opts(legend.position = c(0,1.05)) + opts(legend.text = theme_text(colour = "black")) 
        print(kC, vp=vplayout(2:5,1:4))                     # the main x/y plot will instead spread across most of the grid
        }
    
    fnVP(dH,dS,kC)
    invisible(list(harvest=dH,stock=dS,phase=kC))}

## needs a data set with columns for harvest, stock and grouping var
pts=transform(subset(kobe2012,year==2011 & TAC==0),group=Catch,stock=ssb)
kobePfn(pts)


pts=transform(read.csv("/home/laurie/Desktop/Dropbox/ICCAT/SCRS/YFT/2011/P2011.csv",sep=","),stock=biomass,group=Method)
kobePfn(pts,mns=T)




