
read.CSA.FLIndex<-function(file.){
   t.    <-scan(file=file.,skip=1,sep=",")
   nrow. <-length(t.)/9
   t.    <-t(array(t.,dim=c(9,nrow.)))
   t.    <-array(t.[,-1],dim=c(20,8),
                         dimnames=list(t.[,1],c("m","c.rec","c.full","w.rec","w.full","s.rat","u.rec","u.full")))

   c.    <-FLIndex(index=as.FLQuant(t(array(cbind(t.[,"u.rec"],t.[,"u.full"]),
                                   dim=c(nrow.,2),
                                   dimnames=list(dimnames(t.)[[1]],c("r","full"))))),
		  effort=as.FLQuant(t(array(1,                                                                    dim=c(nrow.,1),
                                    dimnames=list(dimnames(t.)[[1]],"all"     )))),
		  w     =as.FLQuant(t(array(1,                                                                    dim=c(nrow.,1),
                                    dimnames=list(dimnames(t.)[[1]],"all"     )))),
		  p     =as.FLQuant(t(array(t.[,"s.rat"],                                                         dim=c(nrow.,1),
                                    dimnames=list(dimnames(t.)[[1]],"all"     )))))

    c.@range["minyear"]     <-min(t.[,1])   
    c.@range["maxyear"]     <-max(t.[,1])   
    c.@range["startfishing"]<-0.0
    c.@range["endfishing"]  <-0.0

    c.			<-FLIndices(c.)
    c.@desc		<-"read in from CSA file"

    return(c.)
    }


read.CSA.FLStock<-function(file.){
   t.    <-scan(file=file.,skip=1,sep=",")
   nrow. <-length(t.)/9
   t.    <-t(array(t.,dim=c(9,nrow.)))
   t.    <-array(t.[,-1],dim=c(20,8),
                         dimnames=list(t.[,1],c("m","c.rec","c.full","w.rec","w.full","s.rat","u.rec","u.full")))


   s.       <-FLStock()
   s.@catch.n    <-as.FLQuant(t(array(cbind(t.[,"c.rec"],t.[,"c.full"]),
                         dim=c(nrow.,2),
                         dimnames=list(dimnames(t.)[[1]],c("r","full")))))
   s.@stock.wt   <-as.FLQuant(t(array(cbind(t.[,"w.rec"],t.[,"w.full"]),
                          dim=c(nrow.,2),
                          dimnames=list(dimnames(t.)[[1]],c("r","full")))))
   s.@catch.wt   <-as.FLQuant(t(array(cbind(t.[,"w.rec"],t.[,"w.full"]),
                          dim=c(nrow.,2),
                          dimnames=list(dimnames(t.)[[1]],c("r","full")))))
   s.@m          <-as.FLQuant(t(array(t.[,"m"], 
                          dim=c(nrow.,2),
                          dimnames=list(dimnames(t.)[[1]],c("r","full")))))

   mat.0        <-as.FLQuant(0,dimnames(s.@catch.n))
   mat.na       <- mat.0
   mat.na[,,,,] <- NA

   s.@f.spwn     <-mat.0
   s.@m.spwn     <-mat.0

   s.@landings.n <-s.@catch.n
   s.@landings.wt<-s.@catch.wt
   s.@discards.n <-mat.0
   s.@discards.wt<-mat.0

   d.            <-dimnames(s.@catch.n)
   d.$age        <-"all"
   s.@catch      <-as.FLQuant(apply(s.@catch.wt   *s.@catch.n,   2,sum),d.)
   s.@landings   <-as.FLQuant(apply(s.@landings.wt*s.@landings.n,2,sum),d.)
   s.@discards   <-as.FLQuant(apply(s.@discards.wt*s.@discards.n,2,sum),d.)

   s.@mat        <-mat.na
   s.@stock.n    <-mat.na
   s.@f          <-mat.na

   s.@range["minyear"]     <-min(t.[,1])   
   s.@range["maxyear"]     <-max(t.[,1])   

   s.@desc		<-"read in from CSA file"

   return(s.)
   }
