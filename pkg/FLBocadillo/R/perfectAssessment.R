perfectAssess<-function(stk,biol,fleet,yrs.assess,bias=NULL,cv=NULL)
    {
    stk        <-stk[,yrs.assess]
    stk@stock.n<-setPlusGroup(biol@n[,yrs.assess],stk@range["plusgroup"])
    stk@harvest<-setPlusGroup(calcF(m(biol)[,yrs.assess],
                              catch.n(fleet,1,1)[,yrs.assess],
                              n(biol)[,yrs.assess]),stk@range["plusgroup"])

    if (!is.null(bias)){
       stk@stock.n[,yrs.assess[length(yrs.assess)]]<-stk@stock.n[,yrs.assess[length(yrs.assess)]]*(1-bias)
       stk@harvest[,yrs.assess[length(yrs.assess)]]<-stk@harvest[,yrs.assess[length(yrs.assess)]]*(1-bias)}

    if (!is.null(CV)){
       stk@stock.n[,yrs.assess[length(yrs.assess)]]<-stk@stock.n[,yrs.assess[length(yrs.assess)]]*exp(rnorm(0,cv))/(cv^2)
       stk@harvest[,yrs.assess[length(yrs.assess)]]<-stk@harvest[,yrs.assess[length(yrs.assess)]]*exp(rnorm(0,cv))/(cv^2)}

    return(stk)
    }
