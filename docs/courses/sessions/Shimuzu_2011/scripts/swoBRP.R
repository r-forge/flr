##### Biological reference points ##############################################
################################################################################
library(FLBRP)

swoBRP<-FLBRP(swo)
refpts(swoBRP)

refpts(swoBRP)[c("f0.1","fmax","spr.30","msy"),c("harvest","yield","rec","ssb","biomass")]
params(swoBRP)

plot(swoBRP)

params(swoBRP)[]<-exp(mean(log(c(rec(swo)))))
plot(swoBRP,obs=T)

fbar(swoBRP)<-seq(0,1,length.out=101)
swoBRP<-brp(swoBRP)
plot(swoBRP,obs=T)

### Natural Mortality simulation
## What if M<0.2
windows()
swoMBRP<-FLBRP(swoM)
params(swoMBRP)[]<-exp(mean(log(c(rec(swoM)))))
fbar(swoMBRP)<-seq(0,1,length.out=101)
swoMBRP<-brp(swoMBRP)
plot(swoMBRP,obs=T)

### Catch simulation
## What if recent catches of large fish IUU
windows()
swoCBRP<-FLBRP(swoC)
params(swoCBRP)[]<-exp(mean(log(c(rec(swoC)))))
fbar(swoCBRP)<-seq(0,1,length.out=101)
swoCBRP<-brp(swoCBRP)
plot(swoCBRP,obs=T)

refpts(swoBRP)[c("f0.1","fmax","spr.30","msy"),c("harvest","yield","rec","ssb","biomass")]
refpts(swoMBRP)[c("f0.1","fmax","spr.30","msy"),c("harvest","yield","rec","ssb","biomass")]
refpts(swoCBRP)[c("f0.1","fmax","spr.30","msy"),c("harvest","yield","rec","ssb","biomass")]

smryTS<-rbind(
data.frame(Scenario="Catch",model.frame(FLQuants(yield=yield.hat(swoCBRP),
                                                   fbar =fbar(swoCBRP)))),
data.frame(Scenario="M",    model.frame(FLQuants(yield=yield.hat(swoMBRP),
                                                   fbar =fbar(swoMBRP)))),
data.frame(Scenario="SG",   model.frame(FLQuants(yield=yield.hat(swoBRP),
                                                   fbar =fbar(swoMBRP)))))


ggplot(smryTS)+geom_line(aes(fbar,yield,group=Scenario,col=Scenario)


#### Monte-Carlo
swoMCBRP<-brp(FLBRP(swoMC))
refpts(swoMCBRP)[c("f0.1","fmax","spr.30","msy"),c("ssb")]
plot(refpts(swoMCBRP)[c("f0.1","fmax","spr.30","msy"),c("harvest")])


rfpt<-as.data.frame(refpts(swoMCBRP)[c("f0.1","fmax","spr.30","msy"),c("ssb","harvest","yield")])

ggplot(rfpt)+geom_density(aes(data))+facet_grid(refpt~quantity)

#### Stock recruit fitting
swoSR        <-as.FLSR(swo)
model(swoSR) <-bevholt()
swoSR <-fmle(swoSR)
plot(swoSR)
profile(swoSR)

swoMSR<-as.FLSR(swoM)
model(swoMSR)<-bevholt()
swoMSR<-fmle(swoMSR)
plot(swoMSR)

swoCSR<-as.FLSR(swoC)
model(swoCSR)<-bevholt()
swoCSR<-fmle(swoCSR)
plot(swoCSR)

swoBRP.SR <-brp(FLBRP(swo, swoSR ))
swoMBRP.SR<-brp(FLBRP(swoM,swoMSR))
swoCBRP.SR<-brp(FLBRP(swoC,swoCSR))

plot(swoBRP.SR)
plot(swoMBRP.SR)
plot(swoCBRP.SR)

getTS<-function(x) model.frame(FLQuants(ssb=ssb(x),yield=yield(x),f=fbar(x),rec=rec(x)))

tmp<-ldply(FLlst(SG=swoBRP,M=swoMBRP,Catch=swoCBRP),getTS)
tmp<-rbind(data.frame(SR="Mean",tmp),
           data.frame(SR="Bevholt",ldply(FLlst(SG=swoBRP.SR,M=swoMBRP.SR,Catch=swoCBRP.SR),getTS)))
           
ggplot(tmp)+geom_line(aes(f,yield,col=.id,group=.id))+facet_wrap(~SR) +
            scale_x_continuous(limits=c(0,1)) +
            scale_y_continuous(limits=c(0,20000))

swoSRSV <-swoSR
model(swoSRSV) <-bevholtSV()
swoSRSV <-fmle(swoSRSV,fixed=list(spr0=spr0(swoBRP),s=0.6))
plot(swoSRSV)
profile(swoSRSV)

swoBRP.SRSV <-brp(FLBRP(swo, ab(swoSRSV)))

plot(swoBRP.SRSV)

tmp<-rbind(tmp,data.frame(.id="SG",SR="0.6",getTS(swoBRP.SRSV)))

ggplot(tmp)+geom_line(aes(f,yield,col=.id,group=.id))+facet_wrap(~SR)
ggplot(tmp)+geom_line(aes(f,yield,col=.id,group=.id))+facet_wrap(~SR)
