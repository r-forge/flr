library(FLCore)
library(FLBRP)
library(ggplotFL)

bft<-readVPA2Box("bfte2010.c1",nits=501)

t1 <-readPro2Box("/home/lkell/Desktop/Stuff/data/inputs/pro2box/BENCH-1.OUT",type="ref")
t2 <-readPro2Box("/home/lkell/Desktop/Stuff/data/inputs/pro2box",type="sta")
t3 <-readPro2Box("/home/lkell/Desktop/Stuff/data/inputs/pro2box",type="out")

bio<-FLStocks(inf13=readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/BFT/Bootstraps/Inflated/run13/bfte2010.c1"),
              inf15=readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/BFT/Bootstraps/Inflated/run15/bfte2010.c1"),
              rep13=readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/BFT/Bootstraps/Reported/run13/bfte2010.c1"),
              rep15=readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/BFT/Bootstraps/Reported/run15/bfte2010.c1"))
   
v13=readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/BFT/Retros/run13/bfte2010.c1")

yft<-FLStocks()
yft[[ "continuity"]]<-readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/CONT/yft2008.ctl",  m=0.8)
yft[[ "3"]]         <-readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/Run 3/yft2008.ctl", m=0.8)
yft[[ "4"]]         <-readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/Run 4/yft2008.ctl", m=0.8)
yft[[ "5"]]         <-readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/Run 5/yft2008.ctl", m=0.8)
yft[[ "6"]]         <-readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/Run 6/yft2008.ctl", m=0.8)
yft[[ "7"]]         <-readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/Run 7/yft2008.ctl", m=0.8)
yft[[ "8"]]         <-readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/Run 8/yft2008.ctl", m=0.8)
yft[[ "9"]]         <-readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/Run 9/yft2008.ctl", m=0.8)
yft[["10"]]         <-readVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/Run 10/yft2008.ctl",m=0.8)

diagsVPA2Box("/home/lkell/Desktop/Stuff/data/inputs/VPA2Box/YFT/Run 3/yft2008.rep")

t3.<-transform(merge(t3,subset(t1,refpt=="msy",select=c("iter","harvest","ssb")),by=c("iter")),
		        ssb=ssb.x/ssb.y,harvest=fapex/harvest)[,c("scen","year","iter","ssb","harvest")]
t3.<-cbind(t3.,kobeP(t3.$ssb,t3.$harvest))

t3..<-ddply(t3., .(scen,year), function(x) with(x,cbind(f=mean(f),b=mean(b),overFished=mean(overFished),overFishing=mean(overFishing),collapsed=mean(collapsed),p=mean(p))))


ggplot(t3..) + geom_line(aes(year,1-f),        colour="orange",size=3) + 
               geom_line(aes(year,1-b),        colour="yellow",size=3) +
               geom_line(aes(year,p),          colour="green", size=3) + 
               geom_line(aes(year,collapsed),  colour="red",   size=3) + 
               facet_grid(scen~.)
 
