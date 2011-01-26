smryClass<-function(class,object=NULL){
   mth<-data.frame(method=TRUE,name=getClassMethods(class))
   slt<-data.frame(slot  =TRUE,name=names(getSlots(class)), returns=getSlots(class))

   res        <-merge(mth,slt,by="name",all=T)
   res$returns<-ac(res$returns)
   res$method[is.na(res$method)]<-FALSE
   res$slot[is.na(res$slot)]    <-FALSE

   res[grep("<-",ac(res$name)),"accessor"]<-TRUE
   res[res$slot & res$method,  "accessor"]<-TRUE

   mth<-res[-grep("<-",ac(res$name)),]
   mth<-mth[is.na(mth$returns),]
   mth<-ac(mth[mth$method,"name"])

   if (!is.null(object)){
     mthRtn<-function(x) {
              res=try(class(do.call(x,list(object))))
              if(class(res) =='try-error') return (NA) else return(res)}

     tmp<-mdply(mth,mthRtn)[,2]
     res[res$name %in% mth,"returns"]<-tmp}

   res<-res[order(res$name),]
   res<-res[order(res$method,decreasing=TRUE),]
   res<-res[order(res$slot,  decreasing=TRUE),]

   res        <-res[,c("name","slot","method","accessor","returns")]

   return(res)}
