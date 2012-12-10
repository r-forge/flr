library(aspic)
library(FLBioDym)
library(ggplotFL)
library(RCurl)

inp=            "http://www.iccat.int/stocka/Models/ASPIC/albs/2011/run2/aspicb.inp"
inp="/home/laurie/Desktop/gcode/gbyp-sam/data/ICCAT/ASPIC/albs/2011/run2/aspicb.inp"


object=aspic(inp)
cpue  =aspicCpue(inp)

object=fit(object,cpue)
  
bd=getSlots("FLBioDym")
as=getSlots("aspic")

## compare ASPIC and FLBioDym
mm=merge(data.frame(slot=names(as),what=as),
         data.frame(slot=names(bd),what=bd), by="slot",all=T)
names(mm)[2:3]=c("aspic","biodym")

mm[,2]=factor(mm[,2],levels=c("numeric","character","factor","array","data.frame","FLQuant","FLPar"))
mm[,3]=factor(mm[,3],levels=c("numeric","character","factor","array","data.frame","FLQuant","FLPar"))

mm[order(mm[,"aspic"]),]
mm[order(mm[,"biodym"]),]

au=ldply(cpue,model.frame,drop=T)


## Japan & Taiwan Qs
object@bounds[9:10,"fit"] =0

## MSY & k
object@bounds[2:3, "fit"] =0

ll=mdply(expand.grid("msy"=params(object)["msy"]*seq(.25,4,length.out=16),
                     "k"  =params(object)[  "k"]*seq(.25,4,length.out=16)),
      function(msy,k){
        object@bounds[2:3,"fit"]=0
        
        object@bounds["msy","start"]=msy
        object@bounds["k",  "start"]=  k
        
        res=fit(object,cpue)
        
        data.frame(smry(res),ll=res@ll)})

qplot(msy, k, data=transform(ll,V1/max(V1)), geom="tile", fill=V1) 

## MSY & k
object@bounds[2:3, "fit"] =1

qChk=mdply(expand.grid("q"=object@bounds["q6","start"]*seq(.25,4,length.out=11)),
      function(q){
        
        object@bounds["q6","start"]=q
        
        smry(fit(object,cpue))})

ggplot(melt(qChk,id.var="q"))+geom_line(aes(q,value))+facet_wrap(~variable,scale="free_y")+theme_ms(8)

hokey=aspics(mlply(5:10, function(x,object,cpue) { 
                                      object@bounds[x,"lambda"]=0;
                                      fit(object,cpue)}, 
                                  object=object,cpue=cpue))


p=plot(hokey)
p$data=transform(p$data, Series=laply(cpue[-1], name)[.id])
p$data=subset(p$data, qname!="Yield")

p+facet_grid(qname~Series,scales="free_y")
ldply(hokey,smry)

kokey=aspics(mlply(5:10, function(x,object,cpue) { object@bounds[5:10,"lambda"]=0
                                      object@bounds[x,"lambda"]=1
                                      fit(object,cpue)}, 
                                  object=object, cpue=cpue))
p=plot(kokey)
p$data=transform(p$data, Series=laply(cpue[-1], name)[.id])
p$data=subset(p$data, qname!="Yield")
p+facet_grid(qname~Series,scales="free_y")

ldply(kokey,smry)

index(cpue[[4]])=FLQuant(jackknife(index(cpue[[4]])))
object=fit(object,cpue)

res=llply(seq(length(cpue))[-1],function(x,object,cpue){
                                     ## bug
                                     index(cpue[[x]])=FLQuant(jackknife(index(cpue[[x]])))
                                  
                                     object=fit(object,cpue)
                                     
                                     return(object)
                                     #as.data.frame(smry(object))
                                     },
                                object=object,cpue=cpue)

save(res,file="/home/lkell/Dropbox/MyStuff/WHM/analysis/Data/res.RData")


p=plot(ass)
p$data=transform(p$data, Series=laply(cpue[-1], name)[.id])

p+facet_grid(qname~Series,scales="free_y")

ass  =aspic(inpCC)
cpue =aspicCpue(inpCC)

p2=plot(ass2)
p2$data=subset(p2$data, qname!="Yield")
p2$data=transform(p2$data, Series=laply(cpue[-1], name)[.id])

p2+facet_grid(qname~Series,scales="free_y")

data.frame(CPUE=laply(cpue[-1], name),
           r   =laply(ass2, function(x) 4*params(x)["msy"]/params(x)["k"]),
           msy =laply(ass2, function(x) params(x)["msy"]),
           bmsy=laply(ass2, function(x) params(x)["k"]/2),
           fmsy=laply(ass2, function(x) 2*params(x)["msy"]/params(x)["k"]),
           b.bmsy=laply(ass2, function(x) stock(x)[,"2011"]/(params(x)["k"]/2)),
           f.fmsy=laply(ass2, function(x) (catch(x)[,"2010"]/stock(x)[,"2010"])/(2*params(x)["msy"]/params(x)["k"])))


object2=fit(object,cpue)

object@bounds[-1,1]=1
object2=fit(object,cpue)

plot(aspics("1"=object1,"2"=object2))

fa=aspics(mlply(data.frame(x=2:length(cpue)), function(x,object,cpue) { bounds(object)=bounds(object)[-(3+x),]
                                                            fit(object,cpue[-x])},
                                  object=object,cpue=cpue))
 

fa=aspics(fit(object,cpue))

