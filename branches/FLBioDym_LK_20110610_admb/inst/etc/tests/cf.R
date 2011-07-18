c(c(stock(bdTest)[,"2010"]),c(catch(bdTest)[,"2010"]),prodFunc("pellat",stock(bdTest)[,"2010"],r.,K),c(stock(bdTest)[,"2010"]*r.-(stock(bdTest)[,"2010"])^2*r./K),c(c(stock(bdTest)[,"2011"])))
stock.<-fwd(stock(bdTest),catch=catch(bdTest),r= params(bdTest)["r",1,drop=T],K= params(bdTest)["K",1,drop=T],p=2)


c.=catch(bdTest)
s.<-c.

r.<-0.50500543308371
K <-919.197447400597
b0<-1

s.[,1]<-K*b0
for (i in 2:35) {
  sp<-r.*s.[,i-1]*(1-s.[,i-1]/K)
  s.[,i]<-s.[,i-1]-c.[,i-1]+sp
  print(c(i,sp))
  }
s.

fwd(stock(bdTest),catch=catch(bdTest),r=r.,K=K,p=2)