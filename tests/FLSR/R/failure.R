#I have been checking FLSR and found that in most cases fmle fails to find the maximum
#likelihood; instead it simply returns the initial values as the solution.

#Since most people don´t bother with checking the diganostics to see whether a
#solution has been found this is a potentially serious error, particularly since
#ICES are moving towards MSY based managment which will require stock recruit relationships.

#Here is an example based on North Sea Cod and Beverton & Holt

recDat<-FLQuant(c(399442.954137258,600415.580585818,708510.085863612,612281.826340533,
262676.457244853,228849.963928071,930945.911150857,1407998.18111982,268139.063166856,471631.701086162,470718.792479432,876153.70933718 ,
675946.075397424,1668615.39276925,528504.3998084  ,1350162.24156702,2566637.92434580,544677.694674343,883779.601892284,425490.132955927,
1409443.59246619,256976.180628517,1626307.03113405,354505.992253561,236170.524287349,641789.632571161,204139.127378161,269778.338845183,
582232.134980086,273777.168784740,1036952.37280727,456223.549148364,251197.913032   ,857950.558078434,110825.590175038,199316.466500545,
343949.180003455,106563.542581211,203929.723187428,88799.8652043092,134979.363966414,112299.434615384,275045.49426533 ,119585.227101549))

ssbDat<-FLQuant(c(
157257.341472029,158694.766732714,184554.202597139,213361.253026530,236547.237710208,242373.088439235,240301.726148809,249235.911925125,
252746.974350952,230917.194785131,195341.433410045,224052.104314917,202909.252816087,172324.385537755,155894.925889432,144002.523635220,
149493.419486147,170283.670335253,181696.459248224,176435.178970448,142448.648703270,125186.310507160,118026.386438849,109153.996475384,
101927.686260930,92685.13176263,  87443.7693605805,75901.5057254868,72059.2428046898,71957.9606912199,74045.1973164519,70511.281667785,
91738.9851917991,95260.3873325945,80856.2052433707,76136.0774990915,66702.5985763316,45276.7924599465,33587.6262000806,44677.0138916279,
41068.6215851062,32464.0053121911,34880.2630272190,29566.4782084292))

tst<-FLSR(ssb=ssbDat,rec=recDat)

model(tst)<-bevholt()
tst1<-fmle(tst,control=list(trace=3))
initial(tst)(rec(tst),ssb(tst))
params(tst1)
logLik(tst1)
profile(tst1)

#The problem appears to be with the lower bound (by default this equals 1e-7),
#setting the lower bound to -Inf allows fmle to find the solution. However in some
#cases you obtain -ve values for beta, which gives infinite recruitment at ssb==beta.
#If you set the lower bound to 0.0 to avoid this then in some cases you don´t get
#the best fit. It is best therefore to set the bound to -Inf and then check the fit. I beta<0 then fix beta=0.0

lower(tst)[]<--Inf
tst1<-fmle(tst,control=list(trace=3))
initial(tst)(rec(tst),ssb(tst))
params(tst1)
logLik(tst1)
profile(tst1)

#I suspect a similar problem exists with other SRRs and that a lot of the time
#scaling the data was simply to overcome the problem with the bounds.

#Also there is a bug with fmle as the two methods i.e.

setMethod('fmle', signature(object='FLModel', start='FLPar'),
setMethod('fmle', signature(object='FLModel', start='ANY'),

#have different defaults for upper and lower i.e. (the correct index to use is 1)

lower=rep(-Inf, dim(params(object))[1]),
upper=rep( Inf, dim(params(object))[1])

lower=rep(-Inf, dim(params(object))[2]),
upper=rep( Inf, dim(params(object))[2])

#The goodness with nls that it is fast, but the bad news is that it produces stupid answers

tst3<-nls(tst3)
params(tst3)
logLik(tst3)
profile(tst3)

#What are the advantages of nls? if there are then we should keep it, but it needs to be validated.

#Laurie
