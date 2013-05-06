cor2cov<-function(Correl,Var){
   Covar  <-Correl

   for (i in 1:dim(Correl)[1])
      for (j in 1:dim(Correl)[2]){
         Prod = Correl[i,i] * Correl[j,j]

         if (abs(Prod) > 0.0)
            Covar[i,j] = Correl[i,j] * abs(Var[i] * Var[j])^0.5
         else
            Covar[i,j] = 0.0

         if (i != j)
            Covar[j,i] = Covar[i,j]
         }

   return(Covar)
   }