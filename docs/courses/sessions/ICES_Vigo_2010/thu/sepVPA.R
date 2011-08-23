herVII <-readFLStock("C:/Stuff/FLR/flr4mse/Inputs/VIaS, VIIb, c Herring/index.txt",no.discards=T)
range(herVII)["plusgroup"]<-9

myControl <-FLSepVPA.control(sep.age = 5)
m(herVII)
herVII <-herVII+SepVPA(herVII, myControl, fit.plusgroup=TRUE)

vpaHer<-VPA(window(herVII,end=2005), fit.plusgroup=TRUE, fratio=1)

stock.n(herVII)[,ac(1970:2005)]<-stock.n(vpaHer)[,ac(1970:2005)]
harvest(herVII)[,ac(1970:2005)]<-harvest(vpaHer)[,ac(1970:2005)]
