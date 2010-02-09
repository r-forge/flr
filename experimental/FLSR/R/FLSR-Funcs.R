
sv<-function(type,spr0,a,b=NULL,c=NULL,d=NULL){
      # converts a&b parameterisation into steepness & vergin biomass
      v=sprFunc(type,spr0,a=a,b=b,c=c,d=d)
      s=srrFunc(type,ssb=v*.2,a=a,b=b,c=c,d=d)/srrFunc(type,ssb=v,a=a,b=b,c=c,d=d)

      res<-c(s,v)
      names(res)<-c("s","v")
      return(res)}

ab<-function(type,spr0,s=NULL,v=NULL,c=NULL,d=NULL){
      # converts a&b parameterisation into steepness & vergin biomass
      switch(type,
        "bevholt" ={a<-(v+(v-s*v)/(5*s-1))/spr0;             b<-(v-s*v)/(5*s-1)},
        "ricker"  ={b<-log(5*s)/(v*0.8);                     a<-exp(v*b)/spr0},
        "cushing" ={b<-log(s)/log(0.2);                      a<-(v^(1-b))/(spr0)},
        "shepherd"={b<-v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c));   a<-((v/b)^c+1)/spr0},
        "mean"    ={a<-v/spr0;                               b<-NULL},
        "segreg"  ={a<-5*s/spr0;                             b<-v/(a*spr0)},
        "pellat"  =stop("not done yet"),
        "dersch"  =stop("not done yet"),
        "bevholtD"  =stop("not done yet"),
        "rickerD"  =stop("not done yet"),
        "shepherD"  =stop("not done yet"))

        res<-cbind(a,b)
        return(res)}
