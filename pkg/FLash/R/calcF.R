calcF<-function(m,catch,n)
   {
   its<-unique(c(dims(    m)$iter, 
                 dims(catch)$iter, 
                 dims(    n)$iter))

   if (dims(m)$iter==1) 
      m<-propagate(m,max(its))   
   if (dims(catch)$iter==1) 
      catch<-propagate(catch,max(its))
   if (dims(n)$iter==1) 
      n<-propagate(n,max(its))
   
   if (length(its)> 2) stop("iter mismatch")
   if (length(its)==2 & !any((1 %in% its))) stop("iters have to be 1 or n")
      
   res       <-.Call("CalcF",m,catch,n)
   units(res)<-"f"

   return(res)
   }
