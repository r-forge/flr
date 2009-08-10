r.  <-0.5
K   <-100
bio<-FLQuant(seq(0,K,length.out=100))

par(mfrow=c(3,2))

plot(prodFunc("fox",bio,r.,K)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Fox")
points(   msy("fox",    r.,K)~bmsy("fox",r.,K),col="blue",cex=1.75,pch=16)
points(  bmsy("fox",    r.,K)*fmsy("fox",r.,K)~bmsy("fox",r.,K),col="white",cex=1.25,pch=16)
points((  msy("fox",    r.,K)/fmsy("fox",r.,K)),msy("fox",r.,K),col="red",cex=0.75,pch=16)

plot( prodFunc("schaefer", bio,r.,K       )~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Schaefer")
points(    msy("schaefer",      r.,K)~bmsy("schaefer",r.,K),col="blue",cex=1.75,pch=16)
points(   bmsy("schaefer",      r.,K)*fmsy("schaefer",r.,K)~bmsy("schaefer",r.,K),col="white",cex=1.25,pch=16)
points((   msy("schaefer",      r.,K)/fmsy("schaefer",r.,K)),msy("schaefer",r.,K),col="red",cex=0.75,pch=16)

plot( prodFunc("pellat",  bio,r.,K,p=1.95)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Pella Tomlinson")
points(    msy("pellat",      r.,K,p=1.95)~bmsy("pellat",r.,K,p=1.95),col="blue",cex=1.75,pch=16)
points(   bmsy("pellat",      r.,K,p=1.95)*fmsy("pellat",r.,K,p=1.95)~bmsy("pellat",r.,K,p=1.95),col="white",cex=1.25,pch=16)
points((   msy("pellat",      r.,K,p=1.95)/fmsy("pellat",r.,K,p=1.95)),msy("pellat",r.,K,p=1.95),col="red",cex=0.75,pch=16)

lines(prodFunc("pellat",  bio,r.,K,p=2.00)~bio, col="red")
points(    msy("pellat",      r.,K,p=2.00)~bmsy("pellat",r.,K,p=2.00),col="blue",cex=1.75,pch=16)
points(   bmsy("pellat",      r.,K,p=2.00)*fmsy("pellat",r.,K,p=2.00)~bmsy("pellat",r.,K,p=2.00),col="white",cex=1.25,pch=16)
points((   msy("pellat",      r.,K,p=2.00)/fmsy("pellat",r.,K,p=2.00)),msy("pellat",r.,K,p=2.00),col="red",cex=0.75,pch=16)

lines(prodFunc("pellat",  bio,r.,K,p=2.05)~bio, col="blue")
points(    msy("pellat",      r.,K,p=2.05)~bmsy("pellat",r.,K,p=2.05),col="blue",cex=1.75,pch=16)
points(   bmsy("pellat",      r.,K,p=2.05)*fmsy("pellat",r.,K,p=2.05)~bmsy("pellat",r.,K,p=2.05),col="white",cex=1.25,pch=16)
points((   msy("pellat",      r.,K,p=2.05)/fmsy("pellat",r.,K,p=2.05)),msy("pellat",r.,K,p=2.05),col="red",cex=0.75,pch=16)

plot( prodFunc("shepherd", bio,r.,K,m=0.24)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Shepherd")
points(    msy("shepherd",      r.,K,m=0.24)~bmsy("shepherd",r.,K,m=0.24),col="blue",cex=1.75,pch=16)
points(   bmsy("shepherd",      r.,K,m=0.24)*fmsy("shepherd",r.,K,m=0.24)~bmsy("shepherd",r.,K,m=0.24),col="white",cex=1.25,pch=16)
points((   msy("shepherd",      r.,K,m=0.24)/fmsy("shepherd",r.,K,m=0.24)),msy("shepherd",r.,K,m=0.24),col="red",cex=0.75,pch=16)

lines(prodFunc("shepherd", bio,r.,K,m=0.25)~bio, col="red")
points(    msy("shepherd",      r.,K,m=0.25)~bmsy("shepherd",r.,K,m=0.25),col="blue",cex=1.75,pch=16)
points(   bmsy("shepherd",      r.,K,m=0.25)*fmsy("shepherd",r.,K,m=0.25)~bmsy("shepherd",r.,K,m=0.25),col="white",cex=1.25,pch=16)
points((   msy("shepherd",      r.,K,m=0.25)/fmsy("shepherd",r.,K,m=0.25)),msy("shepherd",r.,K,m=0.25),col="red",cex=0.75,pch=16)

lines(prodFunc("shepherd", bio,r.,K,m=0.26)~bio, col="blue")
points(    msy("shepherd",      r.,K,m=0.26)~bmsy("shepherd",r.,K,,m=0.26),col="blue",cex=1.75,pch=16)
points(   bmsy("shepherd",      r.,K,m=0.26)*fmsy("shepherd",r.,K,,m=0.26)~bmsy("shepherd",r.,K,m=0.26),col="white",cex=1.25,pch=16)
points((   msy("shepherd",      r.,K,m=0.26)/fmsy("shepherd",r.,K,,m=0.26)),msy("shepherd",r.,K,m=0.26),col="red",cex=0.75,pch=16)

plot( prodFunc("gulland",  bio,r.,K)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Gulland")
points(    msy("gulland",      r.,K)~bmsy("gulland",r.,K,p=1.95),col="blue",cex=1.75,pch=16)
points(   bmsy("gulland",      r.,K)*fmsy("gulland",r.,K,p=1.95)~bmsy("gulland",r.,K),col="white",cex=1.25,pch=16)
points((   msy("gulland",      r.,K)/fmsy("gulland",r.,K,p=1.95)),msy("gulland",r.,K),col="red",cex=0.75,pch=16)

plot( prodFunc("fletcher", bio,K=K,msy=K/4,p=2)~bio, type="l",xlab="Biomass",ylab="Surplus Production",main="Fletcher")
points(    msy("fletcher"     ,msy=K/4)~bmsy("fletcher",K=K,p=2),col="blue",cex=1.75,pch=16)
points(   bmsy("fletcher"     ,K=K,p=2)*fmsy("fletcher",K=K,msy=K/4,p=2)~bmsy("fletcher",K=K,p=2),col="white",cex=1.25,pch=16)
points((   msy("fletcher"     ,msy=K/4)/fmsy("fletcher",K=K,msy=K/4,p=2)),msy("fletcher",msy=K/4),col="red",cex=0.75,pch=16)