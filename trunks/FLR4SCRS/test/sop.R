setGeneric("sop", function(object, ...){
	standardGeneric("sop")})
setMethod("sop", signature("FLStock"),
  function(object,what=c("ssb","catch","discards","landings"),quantity="wt",corrrect=FALSE){

  fn<-function(object,what,quantity){
      res<-switch(what,
                  "ssb"      = sweep(apply(stock.n(   object)*stock.wt(   object)*mat(object),  2:6,sum), 2:6, apply(stock.n(   object),         2:6,sum),"/"),
                  "stock"    = sweep(apply(stock.n(   object)*stock.wt(   object),              2:6,sum), 2:6, apply(stock.n(   object),         2:6,sum),"/"),
                  "catch"    = sweep(apply(catch.n(   object)*catch.wt(   object),              2:6,sum), 2:6, apply(catch.n(   object),         2:6,sum),"/"),
                  "discards" = sweep(apply(discards.n(object)*discards.wt(object),              2:6,sum), 2:6, apply(discards.n(object),         2:6,sum),"/"),
                  "landings" = sweep(apply(landings.n(object)*landings.wt(object),              2:6,sum), 2:6, apply(landings.n(object),         2:6,sum),"/"))

      if (quantity=="wt"){
          res<-switch(quantity,
            "ssb"      =stock.wt(   object)<-sweep(stock.wt(   object),2:6,res,"/"),
            "stock"    =stock.wt(   object)<-sweep(stock.wt(   object),2:6,res,"/"),
            "catch"    =catch.wt(   object)<-sweep(catch.wt(   object),2:6,res,"/"),
            "discards" =discards.wt(object)<-sweep(discards.wt(object),2:6,res,"/"),
            "landings" =landings.wt(object)<-sweep(landings.wt(object),2:6,res,"/"))} else
         {res<-switch(quantity,
            "ssb"      =stock.n(   object)<-sweep(stock.n(     object),2:6,res,"/"),
            "stock"    =stock.n(   object)<-sweep(stock.n(     object),2:6,res,"/"),
            "catch"    =catch.n(   object)<-sweep(catch.n(     object),2:6,res,"/*"),
            "discards" =discards.n(object)<-sweep(discards.n(  object),2:6,res,"/*"),
            "landings" =landings.n(object)<-sweep(landings.n(  object),2:6,res,"/*"))}

      return(object)}

  for (i in what) {
     object<-fn(object,i,quantity)
     if (corrrect)
         {res<-switch(quantity,
            "ssb"      =stock.n(   object)<-sweep(stock.n(     object),2:6,res,"/"),
            "stock"    =stock.n(   object)<-sweep(stock.n(     object),2:6,res,"/"),
            "catch"    =catch.n(   object)<-sweep(catch.n(     object),2:6,res,"/*"),
            "discards" =discards.n(object)<-sweep(discards.n(  object),2:6,res,"/*"),
            "landings" =landings.n(object)<-sweep(landings.n(  object),2:6,res,"/*"))


  return(object)})
