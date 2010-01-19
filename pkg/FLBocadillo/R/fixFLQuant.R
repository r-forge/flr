as.FLQuantFix<-function(x){
    nms<-names(x)
    nms<-nms[nms %in% c("age","year","unit","season","area","iter")]

    dmns<-list()
    for (i in nms)
       dmns[[i]]<-sort(unique(mdf[,i]))

    dummy<-expand.grid(dmns)
    dummy<-merge(dummy,x)

    if (!("age" %in% names(dummy)) & ("quant" %in% names(dummy)))
       dummy<-cbind(dummy,quant="all")
    if (!("year" %in% names(dummy)))
       dummy<-cbind(dummy,year=1)
    if (!("unit" %in% names(dummy)))
       dummy<-cbind(dummy,unit="unique")
    if (!("season" %in% names(dummy)))
       dummy<-cbind(dummy,season="all")
    if (!("area" %in% names(dummy)))
       dummy<-cbind(dummy,area="unique")
    if (!("iter" %in% names(dummy)))
       dummy<-cbind(dummy,iter=1)

    dummy<-dummy[order(dummy$iter,dummy$area,dummy$season,dummy$unit,dummy$year,dummy$age),]

    return(as.FLQuant(dummy))
    }