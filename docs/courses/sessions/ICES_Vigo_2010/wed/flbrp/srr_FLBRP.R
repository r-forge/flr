#### Stock recruitment relationships ###########################################
pleBRP <- brp(FLBRP(ple4))

# The SR model
model(pleBRP)

# And the current parameter values
params(pleBRP)


# geometric mean
params(pleBRP)[["a",1]] <-exp(mean(log(rec.obs(pleBRP))))
pleBRP <-brp(pleBRP)
plot(pleBRP)

#### ## Stock recruitment relationships
plot(pleBRP,type="rec.ssb",obs=T)

#### Beverton & Holt stock recruitment relationship
pleBH <-as.FLSR(ple4)
model(pleBH)<-bevholtSV()

pleBH1  <-fmle(pleBH,fixed=list(s=1.0))
pleBH9  <-fmle(pleBH,fixed=list(s=0.9))
pleBH75 <-fmle(pleBH,fixed=list(s=0.75))
pleBH6  <-fmle(pleBH,fixed=list(s=0.6))

SSB<-FLQuant(seq(0,max(ssb(pleBH))*1.2,length.out=101))

stp<-as.data.frame(FLQuants("1.0" =predict(pleBH1, ssb=SSB),
                            "0.9" =predict(pleBH9, ssb=SSB),
                            "0.75"=predict(pleBH75,ssb=SSB),
                            "0.6" =predict(pleBH6, ssb=SSB)))[,7:8]

stp<-cbind(stp,"ssb"=SSB[1,,drop=T])

xyplot(data~ssb,groups=qname,data=stp,type="l")


###### reference points
pleRPT<-refpts(as.numeric(NA),refpt=c("msy","Blim","Bpa","Flim","Fpa","Ftarget"))
pleRPT[c("Blim","Bpa"),          "ssb"]    <-c(160000,160000*1.4)
pleRPT[c("Flim","Fpa","Ftarget"),"harvest"]<-c(0.74,0.6,0.3)

brpBH1  <-brp(FLBRP(ple4,sr=ab(pleBH1) ,refpts=pleRPT, fbar=seq(0,1,length.out=101)))
brpBH9  <-brp(FLBRP(ple4,sr=ab(pleBH9) ,refpts=pleRPT, fbar=seq(0,1,length.out=101)))
brpBH75 <-brp(FLBRP(ple4,sr=ab(pleBH75),refpts=pleRPT, fbar=seq(0,1,length.out=101)))
brpBH6  <-brp(FLBRP(ple4,sr=ab(pleBH6) ,refpts=pleRPT, fbar=seq(0,1,length.out=101)))

plot(brpBH1 )
plot(brpBH9 )
plot(brpBH75)
plot(brpBH6 )

yld<-as.data.frame(FLQuants("1.0" =yield(brpBH1),
                            "0.9" =yield(brpBH9),
                            "0.75"=yield(brpBH75),
                            "0.6" =yield(brpBH6)))[,7:8]

SSB<-as.data.frame(FLQuants("1.0" =ssb(brpBH1),
                            "0.9" =ssb(brpBH9),
                            "0.75"=ssb(brpBH75),
                            "0.6" =ssb(brpBH6)))[,7]

yldSSB<-cbind(yld,SSB=SSB)[,c(2,1,3)]
names(yldSSB)<-c("Steepness","Yield","SSB")
xyplot(Yield~SSB,groups=Steepness,data=yldSSB,type="l")























