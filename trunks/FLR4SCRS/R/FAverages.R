fwtd<-function(x,ages=ac(range(x)["min"]:range(x)["max"]))
   sweep(apply(harvest(x)[ages]*stock.n(x)[ages],2:6,sum),2:6,apply(stock.n(x)[ages],2:6,sum),"/")
   
fhrvt<-function(x,ages=ac(range(x)["min"]:range(x)["max"]))
   sweep(apply(catch.n(x)[ages],2:6,sum),2:6,apply(stock.n(x)[ages],2:6,sum),"/")

setGeneric("fav", function(x, ...)
  standardGeneric("fav"))

setMethod("fav", signature(x="FLStock"),
   function(x,ages=ac(range(x)["minfbar"]:range(x)["maxfbar"])){
      x<-x[ages]

      m    <-sweep(apply(m(x)*stock.n(x),2:6,sum),2:6,apply(stock.n(x),2:6,sum),"/")
      catch<-apply(catch.n(x),2:6,sum)
      stock<-apply(stock.n(x),2:6,sum)

      harvest(stock,catch,m)})

