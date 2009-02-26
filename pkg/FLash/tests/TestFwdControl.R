##Target
res<-fwdControl(data.frame(year=1990,val=1))

x  <-list(val=array(11:20,c(1,10),dimnames=list(1,1:10)))
res<-fwdControl(data.frame(year=1990),trgtArray=x)

x  <-list(min=array( 1:10,c(1,10),dimnames=list(1,1:10)),
          max=array(21:30,c(1,10),dimnames=list(1,1:10)))
res<-fwdControl(data.frame(year=1990),trgtArray=x)

x<-list(min=array( 1:10,c(1,10),dimnames=list(1,1:10)),
        val=array(11:20,c(1,10),dimnames=list(1,1:10)),
        max=array(21:30,c(1,10),dimnames=list(1,1:10)))
res<-fwdControl(data.frame(year=1990),trgtArray=x)

res<-fwdControl(data.frame(year=1990:1991,val=2:3))

x<-list(val=array(1:20,c(2,10),dimnames=list(1:2,1:10)))
res<-fwdControl(data.frame(year=1990:1991),trgtArray=x)

x<-array(rep(1:10,each=3),dim=c(1,3,10),dimnames=list(row=1,c("min","val","max"),iter=1:10))
res<-fwdControl(data.frame(year=1990,quantity="ssb"),trgtArray=x)
res<-fwdControl(data.frame(year=1990:1991),trgtArray=x)

x<-array(rep(c(1,NA),each=2),dim=c(2,2,10),dimnames=list(row=1:2,c("val","max"),iter=1:10))
res<-fwdControl(data.frame(year=1990:1991),trgtArray=x)

##Target & Effort
res<-fwdControl(data.frame(year=1990,val=1),effort=data.frame(year=1990,val=1))

x  <-list(val=array(11:20,c(1,10),dimnames=list(1,1:10)))
res<-fwdControl(data.frame(year=1990),trgtArray=x,effort=data.frame(year=1990),effArray=x)

