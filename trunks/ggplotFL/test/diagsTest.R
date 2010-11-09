plotDiag(diagsXSA(swoPG5.xsa,2))
plotDiag(diagsXSA(swoPG5.xsa,2),plots=3,stuff=NULL)

p =mdply(data.frame(i=1:length(nms)), function(x,p) data.frame(Index=nms[x],diagsXSA(p,x)), swoPG5.xsa)
plotDiag(p,plots=3,stuff=facet_wrap(~Index),group=p$age,col=p$age)


plotDiag(diagsXSA(pXSA),plots=3)
plotDiag(diagsXSA(pXSA,2),plots=3,stuff=NULL)

p =mdply(data.frame(i=1:length(nms)), function(x,p) data.frame(Index=nms[x],diagsXSA(p,x)), pXSA)

### Run XSA
pXSA<-FLXSA(ple4,ple4.indices)
## get data for index 1 and plot the various plots
plotDiag(diagsXSA(pXSA,1),plots=1)
plotDiag(diagsXSA(pXSA,1),plots=2)
plotDiag(diagsXSA(pXSA,1),plots=3)
plotDiag(diagsXSA(pXSA,1),plots=4)
plotDiag(diagsXSA(pXSA,1),plots=5)
plotDiag(diagsXSA(pXSA,1),plots=6)

## Plot all plots
plotDiag(diagsXSA(pXSA,1))

## Get all indices diagnostic data
p =mdply(data.frame(i=1:length(names(lapply(ple4.indices,names)))), function(x,p) data.frame(Index=nms[x],diagsXSA(p,x)), pXSA)

## Plot all ages of a CPUE series in same panel
plotDiag(p,plots=3,stuff=facet_wrap(~Index,scale="free"),group=p$age,col=p$age)

## Plot all ages in same panel
plotDiag(p,plots=3,stuff=facet_wrap(~age,scale="free"),group=p$Index,col=p$Index)
