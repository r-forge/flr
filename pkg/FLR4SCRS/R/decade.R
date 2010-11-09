decade<-function(year){
            res<-year - (year %% 10)
            return(factor(res,seq(min(res),max(res),10)))}